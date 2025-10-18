use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter::{Once, once, repeat_with},
    rc::Rc,
    vec,
};

use crate::{
    ast::{
        BindingMode, Crate, Mutability, NodeId, Span, Symbol, expr::*, item::*, pat::*, stmt::*,
    },
    impossible, make_semantic_error,
    semantics::{
        error::SemanticError,
        expr::{AssigneeKind, ControlFlowInterruptKind, ExprExtra, ExprResult},
        impls::{DerefLevel, Impls},
        item::{AssociatedInfo, ItemExtra},
        pat::{Binding, PatExtra, PatResult},
        resolved_ty::{ResolvedTy, ResolvedTyKind, TypePtr},
        scope::{MainFunctionState, Scope, ScopeKind},
        stmt::StmtResult,
        utils::{AnalyzeStage, FullName, STAGES, is_all_different},
        value::{ConstantValue, PlaceValue, UnEvalConstant, Value, ValueIndex, ValueKind},
        visitor::Visitor,
    },
};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub(crate) scopes: HashMap<NodeId, Scope>,
    pub(crate) impls: HashMap<TypePtr, Impls>,
    pub(crate) expr_results: HashMap<NodeId, ExprResult>,
    pub(crate) expr_value: HashMap<NodeId, Value>,
    pub(crate) stmt_results: HashMap<NodeId, StmtResult>,
    pub(crate) binding_value: HashMap<NodeId, PlaceValue>,

    pub(crate) stage: AnalyzeStage,
}

impl SemanticAnalyzer {
    fn new() -> Self {
        let mut scopes = HashMap::default();
        scopes.insert(
            0,
            Scope {
                id: 0,
                kind: ScopeKind::Root,
                types: HashMap::default(),
                values: HashMap::default(),
                bindings: HashMap::default(),
                children: HashSet::default(),
                father: 0,
            },
        );

        Self {
            scopes,
            impls: HashMap::default(),
            expr_results: HashMap::default(),
            expr_value: HashMap::default(),
            stmt_results: HashMap::default(),
            binding_value: HashMap::default(),
            stage: AnalyzeStage::SymbolCollect,
        }
    }

    pub fn visit(krate: &Crate) -> (Self, Result<(), SemanticError>) {
        let mut analyzer = Self::new();

        for stage in STAGES {
            analyzer.stage = stage;
            let result = analyzer.visit_crate(krate, 0);
            if result.is_err() {
                return (analyzer, result);
            }
        }

        (analyzer, Ok(()))
    }

    pub fn add_scope(&mut self, id: NodeId, father: NodeId, kind: ScopeKind) {
        let replace = self.scopes.insert(
            id,
            Scope {
                id,
                kind,
                types: HashMap::default(),
                values: HashMap::default(),
                bindings: HashMap::default(),
                children: HashSet::default(),
                father,
            },
        );
        debug_assert!(replace.is_none());
        self.scopes.get_mut(&father).unwrap().children.insert(id);
    }

    pub fn get_scope(&self, id: NodeId) -> &Scope {
        self.scopes.get(&id).unwrap()
    }

    pub fn get_scope_mut(&mut self, id: NodeId) -> &mut Scope {
        self.scopes.get_mut(&id).unwrap()
    }

    pub fn get_parent_scope(&self, id: NodeId) -> Option<NodeId> {
        let scope = self.get_scope(id);
        if scope.kind.is_root() {
            None
        } else {
            Some(scope.father)
        }
    }

    pub fn get_full_name(scope: NodeId, name: Symbol) -> FullName {
        FullName(vec![Symbol(scope.to_string()), name])
    }

    pub fn add_type_placeholder(
        &mut self,
        scope: NodeId,
        name: Symbol,
    ) -> Result<TypePtr, SemanticError> {
        let fullname = Self::get_full_name(scope, name.clone());
        let scope = self.get_scope_mut(scope);

        let ptr = TypePtr(Rc::new(RefCell::new(ResolvedTy {
            name: Some(fullname),
            kind: super::resolved_ty::ResolvedTyKind::Placeholder,
        })));

        let replace = scope.types.insert(name, ptr.clone());

        if replace.is_some() {
            return Err(make_semantic_error!(TypeDefineConflict));
        }

        Ok(ptr)
    }

    pub fn get_type(&self, scope: NodeId, name: &Symbol) -> Option<TypePtr> {
        self.get_scope(scope).types.get(name).cloned()
    }

    pub fn add_scope_value(
        &mut self,
        scope: NodeId,
        name: &Symbol,
        value: PlaceValue,
    ) -> Result<&mut PlaceValue, SemanticError> {
        let scope = self.get_scope_mut(scope);
        let replace = scope.values.insert(name.clone(), value);

        if replace.is_some() {
            return Err(make_semantic_error!(ValueDefineConflict));
        }

        Ok(scope.values.get_mut(name).unwrap())
    }

    pub fn get_scope_value(&self, scope: NodeId, name: &Symbol) -> Option<&PlaceValue> {
        self.get_scope(scope).values.get(name)
    }

    pub fn get_scope_value_mut(&mut self, scope: NodeId, name: &Symbol) -> Option<&mut PlaceValue> {
        self.get_scope_mut(scope).values.get_mut(name)
    }

    pub fn get_binding_index(&self, scope: NodeId, name: &Symbol) -> Option<NodeId> {
        self.get_scope(scope).bindings.get(name).cloned()
    }
}

impl<'ast> Visitor<'ast> for SemanticAnalyzer {
    type DefaultRes<'res>
        = Result<(), SemanticError>
    where
        Self: 'res;

    type ExprRes<'res>
        = Result<(), SemanticError>
    // 去 expr_result 获取 result
    where
        Self: 'res;

    type PatRes<'res>
        = Result<PatResult, SemanticError>
    where
        Self: 'res;

    type StmtRes<'res>
        = Result<(), SemanticError>
    where
        Self: 'res;

    type CrateExtra<'tmp> = NodeId;

    type ItemExtra<'tmp> = ItemExtra;

    type StmtExtra<'tmp> = ExprExtra<'tmp>;

    type ExprExtra<'tmp> = ExprExtra<'tmp>;

    type PatExtra<'tmp> = PatExtra<'tmp>;

    fn visit_crate<'tmp>(
        &mut self,
        Crate { items, id }: &'ast Crate,
        extra: Self::CrateExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(*id, extra, ScopeKind::Crate);
            }
            AnalyzeStage::Definition => {}
            AnalyzeStage::Body => {}
        }

        for item in items {
            self.visit_item(
                item,
                ItemExtra {
                    father: *id,
                    self_id: 0,
                    span: Span::default(),
                    associated_info: None,
                },
            )?;
        }

        Ok(())
    }

    fn visit_item<'tmp>(
        &mut self,
        Item { kind, id, span }: &'ast Item,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        let new_extra = ItemExtra {
            self_id: *id,
            span: *span,
            ..extra
        };

        match kind {
            ItemKind::Const(const_item) => self.visit_const_item(const_item, new_extra),
            ItemKind::Fn(fn_item) => self.visit_fn_item(fn_item, new_extra),
            ItemKind::Mod(mod_item) => self.visit_mod_item(mod_item, new_extra),
            ItemKind::Enum(enum_item) => self.visit_enum_item(enum_item, new_extra),
            ItemKind::Struct(struct_item) => self.visit_struct_item(struct_item, new_extra),
            ItemKind::Trait(trait_item) => self.visit_trait_item(trait_item, new_extra),
            ItemKind::Impl(impl_item) => self.visit_impl_item(impl_item, new_extra),
        }
        .map_err(|e| e.set_span(span))
    }

    fn visit_const_item<'tmp>(
        &mut self,
        ConstItem { ident, ty, expr }: &'ast ConstItem,
        ItemExtra {
            father,
            self_id: _,
            span,
            associated_info,
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {}
            AnalyzeStage::Definition => {
                let t = self.resolve_type(&ty, Some(father))?;

                let const_value = match expr {
                    Some(e) => ConstantValue::UnEval(UnEvalConstant::new(father, e.as_ref())),
                    None => {
                        if associated_info.as_ref().is_none_or(|x| !x.is_trait) {
                            return Err(make_semantic_error!(ConstantWithoutBody));
                        }
                        ConstantValue::Placeholder
                    }
                };

                let value = PlaceValue {
                    value: Value {
                        ty: t,
                        kind: ValueKind::Constant(const_value),
                    },
                    mutbl: Mutability::Not,
                };

                if let Some(asso) = associated_info {
                    self.add_impl_value(&asso, &ident.symbol, value)
                } else {
                    self.add_scope_value(father, &ident.symbol, value)
                }
                .map_err(|e| e.set_span(&span))?;
            }
            AnalyzeStage::Body => {
                let place = if let Some(asso) = associated_info {
                    self.get_impl_value_mut(&asso, &ident.symbol).unwrap()
                } else {
                    self.get_scope_value_mut(father, &ident.symbol).unwrap()
                };
                let constant = place.value.kind.as_constant_mut().unwrap();

                if let ConstantValue::UnEval(u) = constant {
                    let u = u.clone();
                    let mut ty = place.value.ty.clone();
                    let v = self.eval_unevaling(&u, &mut ty)?;
                    let place_mut = self.get_scope_value_mut(father, &ident.symbol).unwrap();
                    place_mut.value.ty = ty;
                    *place_mut.value.kind.as_constant_mut().unwrap() = v;
                }
            }
        }

        Ok(())
    }

    fn visit_fn_item<'tmp>(
        &mut self,
        FnItem {
            ident,
            generics: _,
            sig,
            body,
        }: &'ast FnItem,
        ItemExtra {
            father,
            self_id,
            span,
            associated_info,
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let is_main_function =
                    self.get_scope(father).kind.is_crate() && ident.symbol.0 == "main";
                self.add_scope(
                    self_id,
                    father,
                    ScopeKind::Fn {
                        ret_ty: Self::any_type(),
                        main_fn: if is_main_function {
                            MainFunctionState::UnExited
                        } else {
                            MainFunctionState::Not
                        },
                    },
                );
            }
            AnalyzeStage::Definition => {
                if body.is_none() && associated_info.as_ref().is_none_or(|x| !x.is_trait) {
                    return Err(make_semantic_error!(FunctionWithoutBody));
                }

                let params_ty = sig
                    .decl
                    .inputs
                    .iter()
                    .map(|x| self.resolve_type(&x.ty, Some(father)))
                    .collect::<Result<Vec<_>, _>>()?;
                let ret_ty = match &sig.decl.output {
                    FnRetTy::Default => Self::unit_type(),
                    FnRetTy::Ty(ty) => self.resolve_type(ty, Some(father))?,
                };

                let is_method = sig.decl.inputs.iter().next().map_or(false, |x| {
                    x.pat
                        .kind
                        .as_ident()
                        .map_or(false, |y| y.1.symbol.is_self())
                });
                if is_method && associated_info.is_none() {
                    return Err(make_semantic_error!(SelfInNotAssociateItem));
                }

                let func_ty = Self::fn_type(ret_ty.clone(), params_ty);

                *self.get_scope_mut(self_id).kind.as_fn_mut().unwrap().0 = ret_ty;

                let value = PlaceValue {
                    value: Value {
                        ty: func_ty,
                        kind: ValueKind::Fn {
                            is_method,
                            is_placeholder: body.is_none(),
                        },
                    },
                    mutbl: Mutability::Not,
                };

                if let Some(asso) = associated_info {
                    self.add_impl_value(&asso, &ident.symbol, value)
                } else {
                    self.add_scope_value(father, &ident.symbol, value)
                }
                .map_err(|e| e.set_span(&span))?;
            }
            AnalyzeStage::Body => {
                let bindings = sig
                    .decl
                    .inputs
                    .iter()
                    .map(|x| -> Result<_, SemanticError> {
                        let mut ty = self.resolve_type(&x.ty, Some(father))?;
                        let bindings = self.visit_pat(&x.pat, PatExtra { id: 0, ty: &mut ty })?;
                        Ok(bindings.bindings.into_iter())
                    })
                    .collect::<Result<Vec<_>, SemanticError>>()?
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>();

                self.add_bindings(bindings, self_id)?;
            }
        }

        if let Some(body) = body {
            let mut target_ty = if self.stage.is_body() {
                Some(self.get_scope(self_id).kind.as_fn().unwrap().0.clone())
            } else {
                None
            };

            self.visit_block_expr(
                body,
                ExprExtra {
                    target_ty: target_ty.as_mut(),
                    scope_id: self_id,
                    self_id: body.id,
                    span,
                },
            )?;
        }

        Ok(())
    }

    fn visit_mod_item<'tmp>(
        &mut self,
        _item: &'ast ModItem,
        ItemExtra {
            father: _,
            self_id: _,
            span,
            associated_info: _,
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        Err(make_semantic_error!(NoImplementation).set_span(&span))
    }

    fn visit_enum_item<'tmp>(
        &mut self,
        EnumItem(ident, _, variants): &'ast EnumItem,
        ItemExtra {
            father,
            self_id: _,
            span: _,
            associated_info: _,
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let ptr = self.add_type_placeholder(father, ident.symbol.clone())?;
                ptr.borrow_mut().kind = ResolvedTyKind::Enum;

                variants.iter().enumerate().try_for_each(|(i, x)| {
                    self.add_impl_value(
                        &AssociatedInfo {
                            is_trait: false,
                            ty: ptr.clone(),
                            for_trait: None,
                        },
                        &x.ident.symbol,
                        PlaceValue {
                            value: Value {
                                ty: ptr.clone(),
                                kind: ValueKind::Constant(ConstantValue::ConstantInt(i as u32)),
                            },
                            mutbl: Mutability::Not,
                        },
                    )
                    .map(|_| ())
                    .map_err(|e| e.set_span(&x.span))
                })?;
            }
            AnalyzeStage::Definition => {}
            AnalyzeStage::Body => {}
        }

        Ok(())
    }

    fn visit_struct_item<'tmp>(
        &mut self,
        StructItem(ident, _, variants): &'ast StructItem,
        ItemExtra {
            father,
            self_id,
            span,
            associated_info: _,
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let ptr = self.add_type_placeholder(father, ident.symbol.clone())?;

                let names = match variants {
                    VariantData::Struct { fields } => fields
                        .iter()
                        .map(|x| x.ident.as_ref().unwrap().symbol.clone())
                        .collect(),
                    VariantData::Tuple(_) => {
                        return Err(make_semantic_error!(NoImplementation).set_span(&span));
                    }
                    VariantData::Unit => vec![],
                };

                if !is_all_different(&names) {
                    return Err(make_semantic_error!(MultipleDefinedField).set_span(&span));
                }

                self.add_scope(self_id, father, ScopeKind::Struct(ptr, names));
            }
            AnalyzeStage::Definition => {
                let ptr = self.get_scope(self_id).kind.as_struct().unwrap().0.clone();

                let fields = match variants {
                    VariantData::Struct { fields } => {
                        fields
                            .iter()
                            .map(|x| self.resolve_type(&x.ty, Some(self_id)))
                    }
                    .collect::<Result<Vec<_>, SemanticError>>()?,
                    VariantData::Tuple(_) => impossible!(),
                    VariantData::Unit => vec![],
                };

                ptr.borrow_mut().kind = ResolvedTyKind::Tup(fields);
            }
            AnalyzeStage::Body => {}
        }

        Ok(())
    }

    fn visit_trait_item<'tmp>(
        &mut self,
        TraitItem {
            ident,
            generics: _,
            bounds: _,
            items,
        }: &'ast TraitItem,
        ItemExtra {
            father,
            self_id,
            span: _,
            associated_info: _,
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let ptr = self.add_type_placeholder(father, ident.symbol.clone())?;
                ptr.borrow_mut().kind = ResolvedTyKind::Trait;

                self.add_scope(self_id, father, ScopeKind::Trait(ptr));
            }
            AnalyzeStage::Definition => {}
            AnalyzeStage::Body => {}
        }

        for item in items {
            let ty = self.get_self_type(Some(self_id), false).unwrap();
            self.visit_associate_item(
                item,
                ItemExtra {
                    father: self_id,
                    self_id: 0,
                    span: Span::default(),
                    associated_info: Some(AssociatedInfo {
                        is_trait: true,
                        ty,
                        for_trait: None,
                    }),
                },
            )?;
        }

        Ok(())
    }

    fn visit_impl_item<'tmp>(
        &mut self,
        ImplItem {
            generics: _,
            of_trait,
            self_ty,
            items,
        }: &'ast ImplItem,
        ItemExtra {
            father,
            self_id,
            span,
            associated_info: _,
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(
                    self_id,
                    father,
                    ScopeKind::Impl {
                        ty: SemanticAnalyzer::any_type(),
                        for_trait: if of_trait.is_some() {
                            Some(SemanticAnalyzer::any_type())
                        } else {
                            None
                        },
                    },
                );
            }
            AnalyzeStage::Definition => {
                let self_ty = self.resolve_type(&self_ty, Some(father))?;
                *self.get_scope_mut(self_id).kind.as_impl_mut().unwrap().0 = self_ty;
                if let Some(trait_ty) = of_trait {
                    let t = self.resolve_path_type(&None, &trait_ty.path, Some(father))?;
                    *self.get_scope_mut(self_id).kind.as_impl_mut().unwrap().1 = Some(t);
                }
            }
            AnalyzeStage::Body => {
                let (ty, for_trait) = self.get_scope(self_id).kind.as_impl().unwrap();
                if let Some(trait_ty) = for_trait {
                    let trait_info = &self.impls.get(trait_ty).unwrap().inherent;
                    let mut names: HashSet<&Symbol> = trait_info.values.keys().collect();

                    let self_impl = self.impls.get(ty).unwrap().traits.get(trait_ty).unwrap();
                    for (s, v) in &self_impl.values {
                        let mut trait_v = trait_info
                            .values
                            .get(s)
                            .ok_or(make_semantic_error!(UnknownAssociateItem).set_span(&span))?
                            .value
                            .ty
                            .deep_clone();
                        trait_v.remove_implicit_self(Some(ty));
                        if trait_v != v.value.ty {
                            return Err(make_semantic_error!(AssociateItemMismatch));
                        }
                        names.remove(s);
                    }

                    for name in names {
                        let v = trait_info.values.get(name).unwrap();
                        match &v.value.kind {
                            ValueKind::Fn {
                                is_method: _,
                                is_placeholder: true,
                            }
                            | ValueKind::Constant(ConstantValue::Placeholder) => {
                                return Err(make_semantic_error!(NotCompleteImpl));
                            }
                            _ => impossible!(),
                        }
                    }
                }
            }
        }

        for item in items {
            let (ty, for_trait) = self.get_scope(self_id).kind.as_impl().unwrap();

            self.visit_associate_item(
                item,
                ItemExtra {
                    father: self_id,
                    self_id: 0,
                    span: Span::default(),
                    associated_info: Some(AssociatedInfo {
                        is_trait: false,
                        ty: ty.clone(),
                        for_trait: for_trait.clone(),
                    }),
                },
            )?;
        }

        Ok(())
    }

    fn visit_associate_item<'tmp>(
        &mut self,
        Item::<AssocItemKind> { kind, id, span }: &'ast Item<AssocItemKind>,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        let new_extra = ItemExtra {
            self_id: *id,
            span: *span,
            ..extra
        };

        match kind {
            AssocItemKind::Const(const_item) => self.visit_const_item(const_item, new_extra),
            AssocItemKind::Fn(fn_item) => self.visit_fn_item(fn_item, new_extra),
        }
        .map_err(|e| e.set_span(span))
    }

    fn visit_stmt<'tmp>(
        &mut self,
        Stmt { kind, id, span }: &'ast Stmt,
        mut extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        if !kind.is_expr()
            && let Some(traget) = &mut extra.target_ty
        {
            Self::type_eq(traget, &mut Self::unit_type()).map_err(|e| e.set_span(span))?;
        }

        macro_rules! set_result {
            ($analyzer:expr, $id:expr, $e:expr) => {
                if $analyzer.stage.is_body() {
                    $analyzer.set_stmt_result($e, $id);
                }
            };
        }

        match kind {
            StmtKind::Let(local_stmt) => {
                self.visit_local_stmt(
                    local_stmt,
                    ExprExtra {
                        self_id: *id,
                        ..extra
                    },
                )?;
            }
            StmtKind::Item(item) => {
                self.visit_item(
                    item,
                    ItemExtra {
                        father: extra.scope_id,
                        self_id: 0,
                        span: Span::default(),
                        associated_info: None,
                    },
                )?;
                set_result!(
                    self,
                    *id,
                    StmtResult::Else {
                        interrupt: ControlFlowInterruptKind::Not,
                    }
                );
            }
            StmtKind::Expr(expr) => {
                self.visit_expr(expr, extra)?;
                set_result!(self, *id, StmtResult::Expr(expr.id));
            }
            StmtKind::Semi(expr) => {
                self.visit_expr(expr, extra)?;
                set_result!(
                    self,
                    *id,
                    StmtResult::Else {
                        interrupt: self.expr_results.get(&expr.id).unwrap().interrupt,
                    }
                );
            }
            StmtKind::Empty(_) => set_result!(
                self,
                *id,
                StmtResult::Else {
                    interrupt: ControlFlowInterruptKind::Not,
                }
            ),
        };

        Ok(())
    }

    fn visit_local_stmt<'tmp>(
        &mut self,
        LocalStmt {
            pat,
            ty,
            kind,
            id: _,
            span,
        }: &'ast LocalStmt,
        extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        let mut target_type = if self.stage.is_body() {
            Some(
                self.resolve_type(
                    ty.as_ref()
                        .ok_or(make_semantic_error!(NoImplementation).set_span(span))?,
                    Some(extra.scope_id),
                )?,
            )
        } else {
            None
        };

        match kind {
            LocalKind::Decl => return Err(make_semantic_error!(NoImplementation).set_span(span)),
            LocalKind::Init(expr) => self.visit_expr(
                expr,
                ExprExtra {
                    target_ty: target_type.as_mut(),
                    ..extra
                },
            )?,
        }

        if self.stage.is_body() {
            let PatResult { bindings } = self.visit_pat(
                pat,
                PatExtra {
                    id: 0,
                    ty: target_type.as_mut().unwrap(),
                },
            )?;
            self.add_bindings(bindings, extra.scope_id)?;

            let interrupt = match kind {
                LocalKind::Decl => ControlFlowInterruptKind::Not,
                LocalKind::Init(expr) => self.expr_results.get(&expr.id).unwrap().interrupt,
            };
            self.set_stmt_result(StmtResult::Else { interrupt }, extra.self_id);
        }

        Ok(())
    }

    fn visit_expr<'tmp>(
        &mut self,
        Expr { kind, span, id }: &'ast Expr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let new_extra = ExprExtra {
            self_id: *id,
            span: *span,
            ..extra
        };
        match kind {
            ExprKind::Array(expr) => self.visit_array_expr(expr, new_extra),
            ExprKind::ConstBlock(expr) => self.visit_const_block_expr(expr, new_extra),
            ExprKind::Call(expr) => self.visit_call_expr(expr, new_extra),
            ExprKind::MethodCall(expr) => self.visit_method_call_expr(expr, new_extra),
            ExprKind::Tup(expr) => self.visit_tup_expr(expr, new_extra),
            ExprKind::Binary(expr) => self.visit_binary_expr(expr, new_extra),
            ExprKind::Unary(expr) => self.visit_unary_expr(expr, new_extra),
            ExprKind::Lit(expr) => self.visit_lit_expr(expr, new_extra),
            ExprKind::Cast(expr) => self.visit_cast_expr(expr, new_extra),
            ExprKind::Let(expr) => self.visit_let_expr(expr, new_extra),
            ExprKind::If(expr) => self.visit_if_expr(expr, new_extra),
            ExprKind::While(expr) => self.visit_while_expr(expr, new_extra),
            ExprKind::ForLoop(expr) => self.visit_for_loop_expr(expr, new_extra),
            ExprKind::Loop(expr) => self.visit_loop_expr(expr, new_extra),
            ExprKind::Match(expr) => self.visit_match_expr(expr, new_extra),
            ExprKind::Block(expr) => self.visit_block_expr(expr, new_extra),
            ExprKind::Assign(expr) => self.visit_assign_expr(expr, new_extra),
            ExprKind::AssignOp(expr) => self.visit_assign_op_expr(expr, new_extra),
            ExprKind::Field(expr) => self.visit_field_expr(expr, new_extra),
            ExprKind::Index(expr) => self.visit_index_expr(expr, new_extra),
            ExprKind::Range(expr) => self.visit_range_expr(expr, new_extra),
            ExprKind::Underscore(expr) => self.visit_underscore_expr(expr, new_extra),
            ExprKind::Path(expr) => self.visit_path_expr(expr, new_extra),
            ExprKind::AddrOf(expr) => self.visit_addr_of_expr(expr, new_extra),
            ExprKind::Break(expr) => self.visit_break_expr(expr, new_extra),
            ExprKind::Continue(expr) => self.visit_continue_expr(expr, new_extra),
            ExprKind::Ret(expr) => self.visit_ret_expr(expr, new_extra),
            ExprKind::Struct(expr) => self.visit_struct_expr(expr, new_extra),
            ExprKind::Repeat(expr) => self.visit_repeat_expr(expr, new_extra),
        }
        .map_err(|e| e.set_span(span))
    }

    fn visit_array_expr<'tmp>(
        &mut self,
        ArrayExpr(exprs): &'ast ArrayExpr,
        mut extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let len = exprs.len();
        if let Some(t) = &mut extra.target_ty {
            Self::type_eq(t, &mut Self::array_type(Self::any_type(), Some(len as u32)))?;
        }

        let mut ref_mut = extra.target_ty.as_mut().map(|x| x.borrow_mut());

        for e in exprs {
            self.visit_expr(
                e,
                ExprExtra {
                    target_ty: ref_mut.as_mut().map(|x| x.kind.as_array_mut().unwrap().0),
                    scope_id: extra.scope_id,
                    self_id: 0,
                    span: Span::default(),
                },
            )?;
        }

        drop(ref_mut);

        if self.stage.is_body() {
            let (interrupt, assignee) = self.merge_result_info(exprs.iter())?;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: extra.target_ty.unwrap().clone(),
                    kind: ValueKind::Anon,
                },
                assignee,
                interrupt,
            );
        }

        Ok(())
    }

    fn visit_const_block_expr<'tmp>(
        &mut self,
        _expr: &'ast ConstBlockExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_call_expr<'tmp>(
        &mut self,
        CallExpr(func_expr, arg_exprs): &'ast CallExpr,
        mut extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let mut func_type = if self.stage.is_body() {
            Some(Self::any_type())
        } else {
            None
        };
        self.visit_expr(
            &func_expr,
            ExprExtra {
                target_ty: func_type.as_mut(),
                scope_id: extra.scope_id,
                self_id: 0,
                span: Span::default(),
            },
        )?;

        let mut func_type_ref = func_type.as_mut().map(|x| x.borrow_mut());
        let mut ty = func_type_ref.as_mut().map(|x| x.kind.as_fn_mut().unwrap());

        if let Some(t) = &mut extra.target_ty {
            Self::type_eq(ty.as_mut().unwrap().0, t)?;
            if ty.as_ref().unwrap().1.len() != arg_exprs.len() {
                return Err(make_semantic_error!(ArgumentNumberMismatch));
            }
        }

        for (i, arg) in arg_exprs.iter().enumerate() {
            self.visit_expr(
                &arg,
                ExprExtra {
                    target_ty: ty.as_mut().map(|x| x.1.get_mut(i).unwrap()),
                    scope_id: extra.scope_id,
                    self_id: 0,
                    span: Span::default(),
                },
            )?;
        }

        if self.stage.is_body() {
            let iter = once(func_expr).chain(arg_exprs);
            let interrupt = self.merge_expr_interrupt(iter.clone());
            self.no_assignee(iter)?;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: extra.target_ty.unwrap().clone(),
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Value,
                interrupt,
            );
        }

        Ok(())
    }

    fn visit_method_call_expr<'tmp>(
        &mut self,
        MethodCallExpr {
            seg,
            receiver,
            args,
            span: _,
        }: &'ast MethodCallExpr,
        mut extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let mut receiver_type = if self.is_body_stage() {
            Some(Self::any_type())
        } else {
            None
        };
        self.visit_expr(
            receiver,
            ExprExtra {
                target_ty: receiver_type.as_mut(),
                ..extra
            },
        )?;
        let symbol = &seg.ident.symbol;
        let (level, index, mut method) = if let Some(receiver_type) = &receiver_type {
            let (level, index) = self
                .search_value_in_impl_recursively(receiver_type, symbol)?
                .ok_or(make_semantic_error!(UnkonwnMethod))?;

            let value = self.get_value_by_index(&index);
            let method_ty = value.value.ty.clone();
            if method_ty.borrow().kind.as_fn().unwrap().1.len() != args.len() + 1 {
                return Err(make_semantic_error!(ArgumentNumberMismatch));
            }

            (Some(level), Some(index), Some(method_ty))
        } else {
            (None, None, None)
        };

        let mut method_ref = method.as_mut().map(|x| x.borrow_mut());
        let mut ret_ty = method_ref.as_mut().map(|x| x.kind.as_fn_mut().unwrap().0);

        if let Some(ret_ty) = ret_ty.as_mut() {
            Self::type_eq(ret_ty, extra.target_ty.as_mut().unwrap())?;
        }

        for (i, arg) in args.iter().enumerate() {
            self.visit_expr(
                &arg,
                ExprExtra {
                    target_ty: method_ref
                        .as_mut()
                        .map(|x| x.kind.as_fn_mut().unwrap().1.get_mut(i + 1).unwrap()),
                    ..extra
                },
            )?;
        }

        if self.is_body_stage() {
            let iter = once(receiver).chain(args);
            let interrupt = self.merge_expr_interrupt(iter.clone());
            self.no_assignee(iter)?;

            let self_ty = method_ref
                .as_mut()
                .unwrap()
                .kind
                .as_fn()
                .unwrap()
                .1
                .get(0)
                .unwrap();

            let receiver_mutbl = self.get_expr_result(&receiver.id).assignee.into();

            let level = level.unwrap();

            match &self_ty.borrow().kind {
                ResolvedTyKind::Ref(_, ref_mutability) => {
                    let target_mutbl: Mutability = (*ref_mutability).into();
                    let self_mutbl = level.chain_mutbl(receiver_mutbl);
                    if self_mutbl < target_mutbl {
                        return Err(make_semantic_error!(Immutable));
                    }
                }
                _ => {
                    if matches!(level, DerefLevel::Deref(_, _)) {
                        return Err(make_semantic_error!(CannotMoveOutOfReference));
                    }
                }
            }

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: extra.target_ty.unwrap().clone(),
                    kind: ValueKind::MethodCall {
                        level,
                        index: index.unwrap(),
                    },
                },
                AssigneeKind::Value,
                interrupt,
            );
        }

        Ok(())
    }

    fn visit_tup_expr<'tmp>(
        &mut self,
        TupExpr(exprs): &'ast TupExpr,
        mut extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let self_ty = if let Some(ty) = &mut extra.target_ty {
            let mut t =
                Self::tup_type(repeat_with(|| Self::any_type()).take(exprs.len()).collect());
            Self::type_eq(ty, &mut t)?;
            Some(t)
        } else {
            None
        };

        let mut target_ty_ref = extra.target_ty.as_mut().map(|x| x.borrow_mut());

        for (i, expr) in exprs.iter().enumerate() {
            self.visit_expr(
                expr,
                ExprExtra {
                    target_ty: target_ty_ref
                        .as_mut()
                        .map(|x| x.kind.as_tup_mut().unwrap().get_mut(i).unwrap()),
                    ..extra
                },
            )?;
        }

        if self.is_body_stage() {
            let (interrupt, assignee) = self.merge_result_info(exprs.iter())?;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: self_ty.unwrap(),
                    kind: ValueKind::Anon,
                },
                assignee,
                interrupt,
            );
        }

        Ok(())
    }

    fn visit_binary_expr<'tmp>(
        &mut self,
        expr: &'ast BinaryExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_unary_expr<'tmp>(
        &mut self,
        expr: &'ast UnaryExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_lit_expr<'tmp>(
        &mut self,
        expr: &'ast LitExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_cast_expr<'tmp>(
        &mut self,
        expr: &'ast CastExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_let_expr<'tmp>(
        &mut self,
        _expr: &'ast LetExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_if_expr<'tmp>(
        &mut self,
        expr: &'ast IfExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_while_expr<'tmp>(
        &mut self,
        expr: &'ast WhileExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_for_loop_expr<'tmp>(
        &mut self,
        expr: &'ast ForLoopExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_loop_expr<'tmp>(
        &mut self,
        expr: &'ast LoopExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_match_expr<'tmp>(
        &mut self,
        expr: &'ast MatchExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_block_expr<'tmp>(
        &mut self,
        BlockExpr { stmts, id, span }: &'ast BlockExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(*id, extra.scope_id, ScopeKind::Lambda);
            }
            AnalyzeStage::Definition => {}
            AnalyzeStage::Body => {}
        }

        let is_body = self.stage.is_body();

        let last_expr_pos = if is_body {
            match stmts.iter().rev().position(|x| x.kind.is_expr()) {
                Some(inv_pos) => Some(stmts.len() - 1 - inv_pos),
                None => None,
            }
        } else {
            None
        };

        let mut target = Some(extra.target_ty);

        for (i, stmt) in stmts.iter().enumerate() {
            if last_expr_pos.is_some_and(|x| i == x) {
                self.visit_stmt(
                    stmt,
                    ExprExtra {
                        scope_id: *id,
                        self_id: 0,
                        span: *span,
                        target_ty: target.take().unwrap(),
                    },
                )?;
            } else {
                let mut unit_ty = Self::unit_type();
                self.visit_stmt(
                    stmt,
                    ExprExtra {
                        target_ty: if is_body { Some(&mut unit_ty) } else { None },
                        scope_id: *id,
                        self_id: 0,
                        span: *span,
                    },
                )?;
            }
        }

        if is_body {
            let mut interrupt = ControlFlowInterruptKind::Not;
            for stmt in stmts {
                interrupt = interrupt + self.get_stmt_interrupt(&stmt.id);
            }

            match last_expr_pos {
                Some(pos) => {
                    let stmt_id = stmts.get(pos).unwrap().id;
                    let value_index =
                        ValueIndex::Expr(*self.get_stmt_result(&stmt_id).as_expr().unwrap());
                    self.set_expr_result(
                        extra.self_id,
                        ExprResult {
                            value_index: value_index,
                            assignee: AssigneeKind::Value,
                            interrupt,
                        },
                    );
                }
                None => {
                    Self::type_eq(
                        target.unwrap().unwrap(),
                        &mut if interrupt.is_not() {
                            Self::unit_type()
                        } else {
                            Self::never_type()
                        },
                    )?;

                    self.set_expr_value_and_result(
                        extra.self_id,
                        if interrupt.is_not() {
                            Self::unit_value()
                        } else {
                            Self::never_value()
                        },
                        AssigneeKind::Value,
                        interrupt,
                    );
                }
            }
        }

        Ok(())
    }

    fn visit_assign_expr<'tmp>(
        &mut self,
        expr: &'ast AssignExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_assign_op_expr<'tmp>(
        &mut self,
        expr: &'ast AssignOpExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_field_expr<'tmp>(
        &mut self,
        expr: &'ast FieldExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_index_expr<'tmp>(
        &mut self,
        expr: &'ast IndexExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_range_expr<'tmp>(
        &mut self,
        _expr: &'ast RangeExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_underscore_expr<'tmp>(
        &mut self,
        expr: &'ast UnderscoreExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_path_expr<'tmp>(
        &mut self,
        expr: &'ast PathExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_addr_of_expr<'tmp>(
        &mut self,
        expr: &'ast AddrOfExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_break_expr<'tmp>(
        &mut self,
        expr: &'ast BreakExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_continue_expr<'tmp>(
        &mut self,
        expr: &'ast ContinueExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_ret_expr<'tmp>(
        &mut self,
        expr: &'ast RetExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_struct_expr<'tmp>(
        &mut self,
        expr: &'ast StructExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_repeat_expr<'tmp>(
        &mut self,
        expr: &'ast RepeatExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_pat<'tmp>(
        &mut self,
        Pat { kind, id, span }: &'ast Pat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        let new_extra = PatExtra { id: *id, ..extra };
        match kind {
            PatKind::Wild(pat) => self.visit_wild_pat(pat, new_extra),
            PatKind::Ident(pat) => self.visit_ident_pat(pat, new_extra),
            PatKind::Struct(pat) => self.visit_struct_pat(pat, new_extra),
            PatKind::Or(pat) => self.visit_or_pat(pat, new_extra),
            PatKind::Path(pat) => self.visit_path_pat(pat, new_extra),
            PatKind::Tuple(pat) => self.visit_tuple_pat(pat, new_extra),
            PatKind::Ref(pat) => self.visit_ref_pat(pat, new_extra),
            PatKind::Lit(pat) => self.visit_lit_pat(pat, new_extra),
            PatKind::Range(pat) => self.visit_range_pat(pat, new_extra),
            PatKind::Slice(pat) => self.visit_slice_pat(pat, new_extra),
            PatKind::Rest(pat) => self.visit_rest_pat(pat, new_extra),
        }
        .map_err(|e| e.set_span(span))
    }

    fn visit_wild_pat<'tmp>(
        &mut self,
        _pat: &'ast WildPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_ident_pat<'tmp>(
        &mut self,
        IdentPat(mode, ident, _restriction): &'ast IdentPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Self::visit_ident_pat_impl(mode, ident, extra)
    }

    fn visit_struct_pat<'tmp>(
        &mut self,
        _pat: &'ast StructPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_or_pat<'tmp>(
        &mut self,
        _pat: &'ast OrPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_path_pat<'tmp>(
        &mut self,
        PathPat(_, path): &'ast PathPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        let ident = path.get_ident();
        let mode = BindingMode(crate::ast::ByRef::No, Mutability::Not);
        Self::visit_ident_pat_impl(&mode, ident, extra)
    }

    fn visit_tuple_pat<'tmp>(
        &mut self,
        _pat: &'ast TuplePat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_ref_pat<'tmp>(
        &mut self,
        RefPat(pat, mutbl): &'ast RefPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Self::type_eq(
            extra.ty,
            &mut Self::ref_type(Self::any_type(), (*mutbl).into()),
        )?;

        self.visit_pat(
            pat,
            PatExtra {
                ty: extra.ty.borrow_mut().kind.as_ref_mut().unwrap().0,
                id: 0,
            },
        )
    }

    fn visit_lit_pat<'tmp>(
        &mut self,
        _pat: &'ast LitPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_range_pat<'tmp>(
        &mut self,
        _pat: &'ast RangePat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_slice_pat<'tmp>(
        &mut self,
        _pat: &'ast SlicePat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_rest_pat<'tmp>(
        &mut self,
        _pat: &'ast RestPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }
}
