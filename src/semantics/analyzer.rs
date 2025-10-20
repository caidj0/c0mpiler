use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter::{once, repeat_with, zip},
    vec,
};

use ena::unify::{InPlace, UnificationTable};

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
        pat::{PatExtra, PatResult},
        preludes::Preludes,
        resolved_ty::{
            RefMutability, ResolvedTy, ResolvedTyInstance, ResolvedTyKind, TypeIntern, TypeKey,
        },
        scope::{MainFunctionState, Scope, ScopeKind},
        stmt::StmtResult,
        utils::{AnalyzeStage, FullName, STAGES, is_all_different},
        value::{
            ConstantValue, PlaceValue, PlaceValueIndex, UnEvalConstant, Value, ValueIndex,
            ValueIndexKind, ValueKind,
        },
        visitor::Visitor,
    },
};

pub struct SemanticAnalyzer {
    pub(crate) scopes: HashMap<NodeId, Scope>,
    pub(crate) impls: HashMap<ResolvedTyInstance, Impls>,
    pub(crate) expr_results: HashMap<NodeId, ExprResult>,
    pub(crate) expr_value: HashMap<NodeId, Value>,
    pub(crate) stmt_results: HashMap<NodeId, StmtResult>,
    pub(crate) binding_value: HashMap<NodeId, PlaceValue>,

    pub(crate) ut: RefCell<UnificationTable<InPlace<TypeKey>>>,
    pub(crate) preludes: Preludes,

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

        let mut ut = UnificationTable::new();
        let preludes = Preludes::new(&mut ut);

        let mut analyzer = Self {
            scopes,
            impls: HashMap::default(),
            expr_results: HashMap::default(),
            expr_value: HashMap::default(),
            stmt_results: HashMap::default(),
            binding_value: HashMap::default(),
            stage: AnalyzeStage::SymbolCollect,
            ut: RefCell::new(ut),
            preludes,
        };

        analyzer.add_prelude_functions();

        analyzer
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
        subnames: Option<Vec<Symbol>>,
    ) -> Result<TypeKey, SemanticError> {
        let fullname = Self::get_full_name(scope, name.clone());

        let ty = ResolvedTy {
            names: Some((fullname, subnames)),
            kind: super::resolved_ty::ResolvedTyKind::Placeholder,
        };

        let intern = self.intern_type(ty);

        let scope = self.get_scope_mut(scope);
        let replace = scope.types.insert(name, intern);

        if replace.is_some() {
            return Err(make_semantic_error!(TypeDefineConflict));
        }

        Ok(intern)
    }

    pub fn get_type(&self, scope: NodeId, name: &Symbol) -> Option<TypeKey> {
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

    pub fn get_stage(&self) -> AnalyzeStage {
        self.stage
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

    type StmtExtra<'tmp> = ExprExtra;

    type ExprExtra<'tmp> = ExprExtra;

    type PatExtra<'tmp> = PatExtra;

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
                    let ty = place.value.ty.clone();
                    let v = self.eval_unevaling(&u, ty.to_key())?;
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
                        ret_ty: self.unit_type().to_key(),
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
                    FnRetTy::Default => self.unit_type(),
                    FnRetTy::Ty(ty) => self.resolve_type(ty, Some(father))?,
                };

                if self
                    .get_scope(self_id)
                    .kind
                    .as_fn()
                    .unwrap()
                    .1
                    .is_un_exited()
                    && (ret_ty != self.unit_type() || params_ty.len() != 0)
                {
                    return Err(make_semantic_error!(MainFunctionWithWrongType));
                }

                let is_method = sig.decl.inputs.iter().next().map_or(false, |x| {
                    x.pat
                        .kind
                        .as_ident()
                        .map_or(false, |y| y.1.symbol.is_self())
                });
                if is_method && associated_info.is_none() {
                    return Err(make_semantic_error!(SelfInNotAssociateItem));
                }

                let func_ty = ResolvedTy::fn_type(ret_ty, params_ty);

                *self.get_scope_mut(self_id).kind.as_fn_mut().unwrap().0 = ret_ty.to_key();

                let value = PlaceValue {
                    value: Value {
                        ty: self.intern_type(func_ty).into(),
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
                        let ty = self.resolve_type(&x.ty, Some(father))?;
                        let bindings = self.visit_pat(&x.pat, PatExtra { id: 0, ty: ty })?;
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
            let target_ty = if self.stage.is_body() {
                Some((*self.get_scope(self_id).kind.as_fn().unwrap().0).into())
            } else {
                None
            };

            self.visit_block_expr(
                body,
                ExprExtra {
                    target_ty: target_ty,
                    scope_id: self_id,
                    self_id: body.id,
                    span,
                    allow_i32_max: false,
                },
            )?;
        }

        if self.is_body_stage()
            && self
                .get_scope(self_id)
                .kind
                .as_fn()
                .unwrap()
                .1
                .is_un_exited()
        {
            return Err(make_semantic_error!(MainFnWithoutExit));
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
                let subnames = variants.iter().map(|x| x.ident.symbol.clone()).collect();

                let key =
                    self.add_type_placeholder(father, ident.symbol.clone(), Some(subnames))?;
                self.set_type_kind(key, ResolvedTyKind::Enum);

                variants.iter().enumerate().try_for_each(|(i, x)| {
                    self.add_impl_value(
                        &AssociatedInfo {
                            is_trait: false,
                            ty: key.clone(),
                            for_trait: None,
                        },
                        &x.ident.symbol,
                        PlaceValue {
                            value: Value {
                                ty: key.into(),
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
                let subnames = match variants {
                    VariantData::Struct { fields } => fields
                        .iter()
                        .map(|x| x.ident.as_ref().unwrap().symbol.clone())
                        .collect(),
                    VariantData::Tuple(_) => {
                        return Err(make_semantic_error!(NoImplementation).set_span(&span));
                    }
                    VariantData::Unit => vec![],
                };

                if !is_all_different(&subnames) {
                    return Err(make_semantic_error!(MultipleDefinedField).set_span(&span));
                }

                let ptr =
                    self.add_type_placeholder(father, ident.symbol.clone(), Some(subnames))?;

                self.add_scope(self_id, father, ScopeKind::Struct(ptr));
            }
            AnalyzeStage::Definition => {
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

                let ptr = self.get_scope(self_id).kind.as_struct().unwrap();
                self.set_type_kind(*ptr, ResolvedTyKind::Tup(fields));
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
                let ptr = self.add_type_placeholder(father, ident.symbol.clone(), None)?;
                self.set_type_kind(ptr, ResolvedTyKind::Trait);

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
                        ty: self.unit_type().to_key(),
                        for_trait: if of_trait.is_some() {
                            Some(self.unit_type().to_key())
                        } else {
                            None
                        },
                    },
                );
            }
            AnalyzeStage::Definition => {
                let self_ty = self.resolve_type(&self_ty, Some(father))?.to_key();
                let for_trait = if let Some(trait_ty) = of_trait {
                    let t = self.resolve_path_type(&None, &trait_ty.path, Some(father))?;
                    Some(t.to_key())
                } else {
                    None
                };
                self.get_scope_mut(self_id).kind = ScopeKind::Impl {
                    ty: self_ty,
                    for_trait,
                };
            }
            AnalyzeStage::Body => {
                let (&ty, &for_trait) = self.get_scope(self_id).kind.as_impl().unwrap();
                if let Some(trait_ty) = for_trait {
                    let trait_info = &self.get_impls(&trait_ty).inherent;
                    let mut names: HashSet<&Symbol> = trait_info.values.keys().collect();

                    let self_impl = self.get_impl_for_trait(&ty, &trait_ty);
                    for (s, v) in &self_impl.values {
                        let self_ty = self.probe_type_instance(ty.into()).unwrap();

                        let trait_v = self
                            .probe_type_instance_impl(
                                trait_info
                                    .values
                                    .get(s)
                                    .ok_or(
                                        make_semantic_error!(UnknownAssociateItem).set_span(&span),
                                    )?
                                    .value
                                    .ty,
                                true,
                                Some(&self_ty),
                            )
                            .unwrap();

                        let impl_v = self.probe_type_instance(v.value.ty).unwrap();

                        if trait_v != impl_v {
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
        extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        if !kind.is_expr()
            && let Some(traget) = extra.target_ty
        {
            self.ty_intern_eq(traget.into(), self.unit_type())
                .map_err(|e| e.set_span(span))?;
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
                set_result!(self, *id, {
                    self.no_assignee(expr.id)?;
                    StmtResult::Expr(expr.id)
                });
            }
            StmtKind::Semi(expr) => {
                let expr_ty = self.new_any_type();
                self.visit_expr(expr, extra.replace_target(expr_ty))?;
                set_result!(self, *id, {
                    self.no_assignee(expr.id)?;
                    StmtResult::Else {
                        interrupt: self.expr_results.get(&expr.id).unwrap().interrupt,
                    }
                });
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
        let target_type = if self.stage.is_body() {
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
                    target_ty: target_type,
                    ..extra
                },
            )?,
        }

        if self.stage.is_body() {
            self.no_assignee(kind.as_init().unwrap().id)?;

            let PatResult { bindings } = self.visit_pat(
                pat,
                PatExtra {
                    id: 0,
                    ty: target_type.unwrap(),
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
        if self.search_fn_scope(extra.scope_id).map_or(false, |x| {
            self.get_scope(x).kind.as_fn().unwrap().1.is_exited()
        }) {
            return Err(make_semantic_error!(ExprAfterExit));
        }

        let new_extra = ExprExtra {
            self_id: *id,
            span: *span,
            allow_i32_max: if matches!(
                kind,
                ExprKind::Unary(..) | ExprKind::Lit(..) | ExprKind::Block(..)
            ) {
                extra.allow_i32_max
            } else {
                false
            },
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
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let len = exprs.len();
            let target = extra.target_ty.unwrap();

            let inner_ty = self.new_any_type();
            self.type_eq(target, ResolvedTy::array_type(inner_ty, Some(len as u32)))?;

            for e in exprs {
                self.visit_expr(e, extra.replace_target(inner_ty))?;
            }

            let (interrupt, assignee) = self.merge_result_info(exprs.iter())?;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: extra.target_ty.unwrap(),
                    kind: ValueKind::Anon,
                },
                assignee,
                interrupt,
            );
        } else {
            for x in exprs {
                self.visit_expr(x, extra)?;
            }
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
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let func_ty = self.new_any_type();
            self.visit_expr(&func_expr, extra.replace_target(func_ty))?;
            let func_probe = self.probe_type(func_ty).unwrap();
            let ResolvedTyKind::Fn(ret, args) = func_probe.kind else {
                return Err(make_semantic_error!(NotFunctionType));
            };
            self.ty_downgrade_eq(extra.target_ty.unwrap(), ret)?;
            if args.len() != arg_exprs.len() {
                return Err(make_semantic_error!(ArgumentNumberMismatch));
            }
            for (e, ty) in zip(arg_exprs, args) {
                self.visit_expr(e, extra.replace_target(ty))?;
            }

            let iter = once(func_expr).chain(arg_exprs);
            let mut interrupt = self.merge_expr_interrupt(iter.clone());
            if ret.is_never() {
                interrupt = interrupt + ControlFlowInterruptKind::Return;
            }
            self.batch_no_assignee_expr(iter)?;

            // 针对 exit 函数的特判
            let result = self.get_expr_result(&func_expr.id);
            if result.value_index
                == ValueIndex::Place(PlaceValueIndex {
                    name: Symbol::from("exit"),
                    kind: ValueIndexKind::Global { scope_id: 0 },
                })
            {
                let scope_id = self.search_fn_scope(extra.scope_id)?;
                let scope_mut = self.get_scope_mut(scope_id);
                let (_, t) = scope_mut.kind.as_fn_mut().unwrap();
                match t {
                    MainFunctionState::Not => return Err(make_semantic_error!(NotInMainFunction)),
                    MainFunctionState::UnExited => *t = MainFunctionState::Exited,
                    MainFunctionState::Exited => {
                        return Err(make_semantic_error!(MainFunctionDoubleExit));
                    }
                }
            }

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: ret,
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(&func_expr, extra)?;
            for x in arg_exprs {
                self.visit_expr(&x, extra)?;
            }
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
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let receiver_type = self.new_any_type();
            self.visit_expr(&receiver, extra.replace_target(receiver_type))?;
            let symbol = &seg.ident.symbol;
            let (level, index) = self
                .search_value_in_impl_recursively(&receiver_type, symbol)?
                .ok_or(make_semantic_error!(UnkonwnMethod))?;
            let value = self.get_place_value_by_index(&index);
            if value
                .value
                .kind
                .as_fn()
                .map_or(true, |(is_method, _)| !*is_method)
            {
                return Err(make_semantic_error!(NotMethod));
            }
            let method_ty = value.value.ty;
            let method_probe = self.probe_type(method_ty).unwrap();
            let (ret_ty, method_args) = method_probe.kind.as_fn().unwrap();

            self.ty_downgrade_eq(extra.target_ty.unwrap(), *ret_ty)?;
            if args.len() + 1 != method_args.len() {
                return Err(make_semantic_error!(ArgumentNumberMismatch));
            }

            for (e, ty) in zip(args, method_args.iter().skip(1)) {
                self.visit_expr(e, extra.replace_target(*ty))?;
            }

            let iter = once(receiver).chain(args);
            let interrupt = self.merge_expr_interrupt(iter.clone());
            self.batch_no_assignee_expr(iter)?;
            let first_probe = self.probe_type(*method_args.get(0).unwrap()).unwrap();

            let receiver_mutbl = self.get_expr_result(&receiver.id).assignee.into();

            match &first_probe.kind {
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
                    ty: *ret_ty,
                    kind: ValueKind::MethodCall { level, index },
                },
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(&receiver, extra)?;
            for x in args {
                self.visit_expr(&x, extra)?;
            }
        }

        Ok(())
    }

    fn visit_tup_expr<'tmp>(
        &mut self,
        TupExpr(exprs, force): &'ast TupExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let target_ty = extra.target_ty.unwrap();
            let (right_ty, expanded) = match (&exprs[..], force) {
                ([], _) => (self.unit_type(), false),
                ([_], false) => (self.new_any_type(), true),
                _ => {
                    let tup_ty = ResolvedTy::tup_type(
                        repeat_with(|| self.new_any_type())
                            .take(exprs.len())
                            .collect(),
                    );
                    (self.intern_type(tup_ty).into(), false)
                }
            };

            self.ty_intern_eq(target_ty, right_ty)?;

            if expanded {
                self.visit_expr(exprs.first().unwrap(), extra.replace_target(right_ty))?;
            } else {
                let probe = self.probe_type(right_ty).unwrap();
                let tys = probe.kind.as_tup().unwrap();
                for (e, ty) in zip(exprs, tys) {
                    self.visit_expr(e, extra.replace_target(*ty))?;
                }
            }

            let (interrupt, assignee) = self.merge_result_info(exprs.iter())?;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: right_ty,
                    kind: ValueKind::Anon,
                },
                assignee,
                interrupt,
            );
        } else {
            for x in exprs {
                self.visit_expr(x, extra)?;
            }
        }

        Ok(())
    }

    fn visit_binary_expr<'tmp>(
        &mut self,
        BinaryExpr(bin_op, expr1, expr2): &'ast BinaryExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let target_ty = extra.target_ty.unwrap();
            let probe = self.probe_type(target_ty).unwrap();
            let self_ty = if Self::is_string_type(&probe) && bin_op.is_add() {
                self.visit_expr(&expr1, extra.replace_target(target_ty))?;
                self.visit_expr(&expr2, extra.replace_target(self.ref_str_type()))?;
                target_ty
            } else {
                match bin_op {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Rem
                    | BinOp::BitXor
                    | BinOp::BitAnd
                    | BinOp::BitOr => {
                        self.visit_expr(&expr1, extra.replace_target(target_ty))?;
                        self.visit_expr(&expr2, extra.replace_target(target_ty))?;

                        let probe = self.probe_type(target_ty).unwrap();

                        if !(probe.is_integer()
                            || (matches!(bin_op, BinOp::BitAnd | BinOp::BitXor | BinOp::BitOr)
                                && probe.is_bool_type()))
                        {
                            return Err(make_semantic_error!(NoBinaryOperation));
                        }

                        target_ty
                    }

                    BinOp::And | BinOp::Or => {
                        let bool_ty = self.bool_type();
                        self.visit_expr(&expr1, extra.replace_target(bool_ty))?;
                        self.visit_expr(&expr2, extra.replace_target(bool_ty))?;
                        self.ty_intern_eq(target_ty, bool_ty)?;

                        bool_ty
                    }

                    BinOp::Shl | BinOp::Shr => {
                        self.visit_expr(&expr1, extra.replace_target(target_ty))?;
                        let ty2 = self.new_any_int_type();
                        self.visit_expr(&expr2, extra.replace_target(ty2))?;

                        target_ty
                    }

                    BinOp::Eq | BinOp::Lt | BinOp::Le | BinOp::Ne | BinOp::Ge | BinOp::Gt => {
                        let bool_ty = self.bool_type();
                        self.ty_intern_eq(target_ty, bool_ty)?;

                        let inner_ty = self.new_any_type();
                        self.visit_expr(&expr1, extra.replace_target(inner_ty))?;
                        self.visit_expr(&expr2, extra.replace_target(inner_ty))?;

                        let probe = self.probe_type(inner_ty).unwrap();

                        if !(probe.is_integer() || probe.is_char_type() || probe.is_bool_type()) {
                            return Err(make_semantic_error!(NoBinaryOperation));
                        }

                        bool_ty
                    }
                }
            };

            self.batch_no_assignee_expr([expr1, expr2].into_iter())?;
            let interrupt = self.merge_expr_interrupt([expr1, expr2].into_iter());

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: self_ty,
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(&expr1, extra)?;
            self.visit_expr(&expr2, extra)?;
        }

        Ok(())
    }

    fn visit_unary_expr<'tmp>(
        &mut self,
        UnaryExpr(un_op, expr): &'ast UnaryExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let (ty, assignee) = match un_op {
                UnOp::Deref => {
                    let ty = self.new_any_type();
                    self.visit_expr(expr, extra.replace_target(ty))?;

                    let probe = self.probe_type(ty).unwrap();
                    let ResolvedTyKind::Ref(inner, mutbl) = probe.kind else {
                        return Err(make_semantic_error!(NotReferenceType));
                    };

                    self.ty_downgrade_eq(extra.target_ty.unwrap(), inner)?;

                    (inner, AssigneeKind::Place(mutbl.into()))
                }
                UnOp::Not => {
                    self.visit_expr(expr, extra)?;
                    let ty = self.probe_type(extra.target_ty.unwrap()).unwrap();

                    if !(ty.is_integer() || ty.is_bool_type()) {
                        return Err(make_semantic_error!(NoUnaryOperation));
                    }

                    (extra.target_ty.unwrap(), AssigneeKind::Value)
                }
                UnOp::Neg => {
                    let self_ty = self.new_any_signed_int_type();
                    self.ty_intern_eq(extra.target_ty.unwrap(), self_ty)?;
                    self.visit_expr(
                        &expr,
                        ExprExtra {
                            target_ty: Some(self_ty),
                            allow_i32_max: !extra.allow_i32_max,
                            ..extra
                        },
                    )?;

                    (self_ty, AssigneeKind::Value)
                }
            };
            self.no_assignee(expr.id)?;
            let interrupt = self.get_expr_result(&expr.id).interrupt;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: ty,
                    kind: ValueKind::Anon,
                },
                assignee,
                interrupt,
            );
        } else {
            self.visit_expr(&expr, extra)?;
        }

        Ok(())
    }

    fn visit_lit_expr<'tmp>(
        &mut self,
        LitExpr {
            kind,
            symbol,
            suffix,
        }: &'ast LitExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let target_ty = extra.target_ty.unwrap();

            let (constant, self_ty) = match kind {
                LitKind::Bool => {
                    self.ty_intern_eq(target_ty, self.bool_type())?;
                    (
                        ConstantValue::ConstantInt(if symbol == "true" { 1 } else { 0 }),
                        self.bool_type(),
                    )
                }
                LitKind::Char => {
                    self.ty_intern_eq(target_ty, self.char_type())?;
                    (
                        ConstantValue::ConstantInt(symbol.chars().next().unwrap() as u32),
                        self.char_type(),
                    )
                }
                LitKind::Integer => {
                    let restrict_ty = match suffix.as_ref().map(|x| x.as_str()) {
                        Some("i32") => self.i32_type(),
                        Some("u32") => self.u32_type(),
                        Some("isize") => self.isize_type(),
                        Some("usize") => self.usize_type(),
                        Some(_) => return Err(make_semantic_error!(UnknownSuffix)),
                        None => self.new_any_int_type(),
                    };
                    self.ty_intern_eq(target_ty, restrict_ty)?;

                    let value: u32 = symbol.parse().map_err(|_| make_semantic_error!(Overflow))?;

                    let target_probe = self.probe_type(target_ty).unwrap();

                    use crate::semantics::resolved_ty::BuiltInTyKind::*;
                    use crate::semantics::resolved_ty::ResolvedTyKind::*;
                    if matches!(target_probe.kind, BuiltIn(I32 | ISize))
                        && (value > 2147483648 || (value == 2147483648 && !extra.allow_i32_max))
                    {
                        return Err(make_semantic_error!(Overflow));
                    }

                    (ConstantValue::ConstantInt(value), restrict_ty)
                }
                LitKind::Str | LitKind::StrRaw(_) => {
                    self.ty_intern_eq(target_ty, self.ref_str_type())?;
                    (
                        ConstantValue::ConstantString(symbol.clone()),
                        self.ref_str_type(),
                    )
                }
                _ => return Err(make_semantic_error!(NoImplementation)),
            };

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: self_ty,
                    kind: ValueKind::Constant(constant),
                },
                AssigneeKind::Value,
                ControlFlowInterruptKind::Not,
            );
        }

        Ok(())
    }

    fn visit_cast_expr<'tmp>(
        &mut self,
        CastExpr(expr, to_type): &'ast CastExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let to_type = self.resolve_type(&to_type, Some(extra.scope_id))?;
            self.ty_intern_eq(extra.target_ty.unwrap(), to_type)?;

            let expr_type = self.new_any_type();
            self.visit_expr(expr, extra.replace_target(expr_type))?;

            let expr_probe = self.probe_type(expr_type).unwrap();
            let to_probe = self.probe_type(to_type).unwrap();

            if !((expr_probe.is_integer()
                || expr_probe.is_bool_type()
                || expr_probe.is_char_type())
                && to_probe.is_integer())
            {
                return Err(make_semantic_error!(NotSupportCast));
            }

            self.no_assignee(expr.id)?;
            let interrupt = self.get_expr_result(&expr.id).interrupt;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: to_type,
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(
                expr,
                ExprExtra {
                    target_ty: None,
                    ..extra
                },
            )?;
        }

        Ok(())
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
        IfExpr(cond_expr, body_expr, else_expr): &'ast IfExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            self.visit_expr(&cond_expr, extra.replace_target(self.bool_type()))?;
            let target_ty = extra.target_ty.unwrap();
            self.visit_block_expr(
                &body_expr,
                ExprExtra {
                    target_ty: Some(target_ty),
                    self_id: body_expr.id,
                    span: body_expr.span,
                    ..extra
                },
            )?;
            if let Some(else_expr) = else_expr {
                self.visit_expr(&else_expr, extra.replace_target(target_ty))?;
            } else {
                self.ty_intern_eq(target_ty, self.unit_type())?;
            }

            self.no_assignee(cond_expr.id)
                .map_err(|e| e.set_span(&cond_expr.span))?;
            self.no_assignee(body_expr.id)
                .map_err(|e| e.set_span(&body_expr.span))?;
            if let Some(else_expr) = else_expr {
                self.no_assignee(else_expr.id)
                    .map_err(|e| e.set_span(&else_expr.span))?;
            }

            let interrupt = self.get_expr_result(&cond_expr.id).interrupt
                + (self.get_expr_result(&body_expr.id).interrupt
                    | if let Some(else_expr) = else_expr {
                        self.get_expr_result(&else_expr.id).interrupt
                    } else {
                        ControlFlowInterruptKind::Not
                    });

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: target_ty,
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(&cond_expr, extra)?;
            self.visit_block_expr(
                &body_expr,
                ExprExtra {
                    self_id: body_expr.id,
                    span: body_expr.span,
                    ..extra
                },
            )?;
            if let Some(else_expr) = else_expr {
                self.visit_expr(&else_expr, extra)?;
            }
        }

        Ok(())
    }

    fn visit_while_expr<'tmp>(
        &mut self,
        WhileExpr(cond_expr, body_expr): &'ast WhileExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.stage.is_symbol_collect() {
            self.add_scope(extra.self_id, extra.scope_id, ScopeKind::CycleExceptLoop);
        }

        if self.is_body_stage() {
            self.visit_expr(&cond_expr, extra.replace_target(self.bool_type()))?;
            let target_ty = extra.target_ty.unwrap();
            self.ty_intern_eq(target_ty, self.unit_type())?;
            self.visit_block_expr(
                &body_expr,
                ExprExtra {
                    target_ty: Some(self.unit_type()),
                    scope_id: extra.scope_id,
                    self_id: body_expr.id,
                    span: body_expr.span,
                    ..extra
                },
            )?;

            self.no_assignee(cond_expr.id)
                .map_err(|e| e.set_span(&cond_expr.span))?;
            self.no_assignee(body_expr.id)
                .map_err(|e| e.set_span(&body_expr.span))?;

            let interrupt = self.get_expr_result(&cond_expr.id).interrupt;

            self.set_expr_value_and_result(
                extra.self_id,
                self.unit_value(),
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(&cond_expr, extra)?;
            self.visit_block_expr(
                &body_expr,
                ExprExtra {
                    self_id: body_expr.id,
                    span: body_expr.span,
                    scope_id: extra.self_id,
                    ..extra
                },
            )?;
        }

        Ok(())
    }

    fn visit_for_loop_expr<'tmp>(
        &mut self,
        _expr: &'ast ForLoopExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_loop_expr<'tmp>(
        &mut self,
        LoopExpr(body_expr): &'ast LoopExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.stage.is_symbol_collect() {
            let ret_ty = self.new_any_type().to_key();
            self.add_scope(extra.self_id, extra.scope_id, ScopeKind::Loop { ret_ty });
        }

        if self.is_body_stage() {
            let target_ty = extra.target_ty.unwrap();
            let scope_ty = *self.get_scope(extra.self_id).kind.as_loop().unwrap();
            self.ty_downgrade_eq(target_ty, scope_ty.into())?;

            self.visit_block_expr(
                &body_expr,
                ExprExtra {
                    target_ty: Some(self.unit_type()),
                    scope_id: extra.self_id,
                    self_id: body_expr.id,
                    span: body_expr.span,
                    ..extra
                },
            )?;

            self.no_assignee(body_expr.id)
                .map_err(|e| e.set_span(&body_expr.span))?;
            let interrupt = self.get_expr_result(&body_expr.id).interrupt;

            self.set_expr_value_and_result(
                extra.self_id,
                if interrupt.is_return() || interrupt.is_not() {
                    self.never_value()
                } else {
                    Value {
                        ty: scope_ty.into(),
                        kind: ValueKind::Anon,
                    }
                },
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_block_expr(
                &body_expr,
                ExprExtra {
                    scope_id: extra.self_id,
                    self_id: body_expr.id,
                    span: body_expr.span,
                    ..extra
                },
            )?;
        }
        Ok(())
    }

    fn visit_match_expr<'tmp>(
        &mut self,
        _expr: &'ast MatchExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(NoImplementation))
    }

    fn visit_block_expr<'tmp>(
        &mut self,
        BlockExpr { stmts, id, span }: &'ast BlockExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.stage.is_symbol_collect() {
            self.add_scope(*id, extra.scope_id, ScopeKind::Lambda);
        }

        if self.is_body_stage() {
            let mut last_expr_pos = stmts
                .iter()
                .rev()
                .position(|x| {
                    matches!(
                        x.kind,
                        StmtKind::Empty(_) | StmtKind::Expr(_) | StmtKind::Semi(_)
                    )
                })
                .map(|inv_pos| stmts.len() - 1 - inv_pos);
            if let Some(p) = last_expr_pos
                && !stmts.get(p).unwrap().kind.is_expr()
            {
                last_expr_pos = None;
            }

            let target_ty = extra.target_ty.unwrap();

            for (i, stmt) in stmts.iter().enumerate() {
                if last_expr_pos.is_some_and(|x| i == x) {
                    self.visit_stmt(
                        stmt,
                        ExprExtra {
                            scope_id: *id,
                            self_id: 0,
                            span: *span,
                            target_ty: Some(target_ty),
                            ..extra
                        },
                    )?;
                } else {
                    self.visit_stmt(
                        stmt,
                        ExprExtra {
                            target_ty: Some(self.unit_type()),
                            scope_id: *id,
                            self_id: 0,
                            span: *span,
                            ..extra
                        },
                    )?;
                }
            }

            let mut interrupt = ControlFlowInterruptKind::Not;
            for stmt in stmts {
                interrupt = interrupt + self.get_stmt_interrupt(&stmt.id);
            }

            let value_index = if let Some(pos) = last_expr_pos {
                let expr_id = *self
                    .get_stmt_result(&stmts.get(pos).unwrap().id)
                    .as_expr()
                    .unwrap();
                self.get_expr_result(&expr_id).value_index.clone()
            } else {
                self.set_expr_value(
                    extra.self_id,
                    if !interrupt.is_not() {
                        self.ty_intern_eq(target_ty, self.never_type())?;
                        self.never_value()
                    } else {
                        self.ty_intern_eq(target_ty, self.unit_type())?;
                        self.unit_value()
                    },
                );
                ValueIndex::Expr(extra.self_id)
            };

            self.set_expr_result(
                extra.self_id,
                ExprResult {
                    value_index: value_index,
                    assignee: AssigneeKind::Value,
                    interrupt,
                },
            );
        } else {
            for stmt in stmts {
                self.visit_stmt(
                    stmt,
                    ExprExtra {
                        scope_id: *id,
                        ..extra
                    },
                )?;
            }
        }

        Ok(())
    }

    fn visit_assign_expr<'tmp>(
        &mut self,
        AssignExpr(left, right): &'ast AssignExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            self.ty_intern_eq(extra.target_ty.unwrap(), self.unit_type())?;

            let left_ty = self.new_any_type();
            self.visit_expr(&left, extra.replace_target(left_ty))?;
            self.visit_expr(&right, extra.replace_target(left_ty))?;

            let interrupt = self.merge_expr_interrupt(once(left).chain(once(right)));
            self.no_assignee(right.id)?;
            match self.get_expr_result(&left.id).assignee {
                AssigneeKind::Place(Mutability::Mut) => {}
                AssigneeKind::Place(Mutability::Not) => {
                    return Err(make_semantic_error!(Immutable).set_span(&left.span));
                }
                AssigneeKind::Value => {
                    return Err(make_semantic_error!(IllegalLeftExpression).set_span(&left.span));
                }
                AssigneeKind::Only => {}
            }

            self.set_expr_value_and_result(
                extra.self_id,
                self.unit_value(),
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(&left, extra)?;
            self.visit_expr(&right, extra)?;
        }

        Ok(())
    }

    fn visit_assign_op_expr<'tmp>(
        &mut self,
        AssignOpExpr(op, left, right): &'ast AssignOpExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let target_ty = extra.target_ty.unwrap();
            self.ty_intern_eq(target_ty, self.unit_type())?;

            let exp_ty = self.new_any_type();
            self.visit_expr(&left, extra.replace_target(exp_ty))?;

            let probe = self.probe_type(exp_ty).unwrap();

            let right_ty = if Self::is_string_type(&probe) && op.is_add_assign() {
                self.ref_str_type()
            } else if probe.is_integer() {
                if matches!(op, AssignOp::ShlAssign | AssignOp::ShrAssign) {
                    self.new_any_int_type()
                } else {
                    exp_ty
                }
            } else if probe.is_bool_type()
                && matches!(
                    op,
                    AssignOp::BitAndAssign | AssignOp::BitOrAssign | AssignOp::BitXorAssign
                )
            {
                exp_ty
            } else {
                return Err(make_semantic_error!(NoAssignOperation));
            };

            self.visit_expr(&right, extra.replace_target(right_ty))?;

            self.no_assignee(right.id)?;
            match self.get_expr_result(&left.id).assignee {
                AssigneeKind::Place(Mutability::Mut) => {}
                AssigneeKind::Place(Mutability::Not) => {
                    return Err(make_semantic_error!(Immutable).set_span(&left.span));
                }
                AssigneeKind::Value => {
                    return Err(make_semantic_error!(IllegalLeftExpression).set_span(&left.span));
                }
                AssigneeKind::Only => {
                    return Err(make_semantic_error!(AssigneeExprNotAllowed).set_span(&left.span));
                }
            }

            let interrupt = self.merge_expr_interrupt([left, right].into_iter());

            self.set_expr_value_and_result(
                extra.self_id,
                self.unit_value(),
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(&left, extra)?;
            self.visit_expr(&right, extra)?;
        }

        Ok(())
    }

    fn visit_field_expr<'tmp>(
        &mut self,
        FieldExpr(receiver_expr, ident): &'ast FieldExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let receiver_ty = self.new_any_type();
            self.visit_expr(&receiver_expr, extra.replace_target(receiver_ty))?;
            self.no_assignee(receiver_expr.id)?;
            let (level, _, receiver_probe) = self
                .auto_deref(receiver_ty, |analyzer, intern| {
                    let Some(probe) = analyzer.probe_type(intern) else {
                        return Ok(None);
                    };
                    if probe.kind.is_tup() {
                        Ok(Some(probe))
                    } else {
                        Ok(None)
                    }
                })?
                .ok_or(make_semantic_error!(NotStructType).set_span(&receiver_expr.span))?;
            let index = if let Some((_, Some(names))) = &receiver_probe.names {
                names
                    .iter()
                    .position(|x| *x == ident.symbol)
                    .ok_or(make_semantic_error!(UnknownField).set_span(&ident.span))?
            } else {
                let i = ident
                    .symbol
                    .0
                    .parse()
                    .map_err(|_| make_semantic_error!(GeneralError))?;

                if i >= receiver_probe.kind.as_tup().unwrap().len() {
                    return Err(make_semantic_error!(IndexOutOfBound).set_span(&ident.span));
                }

                i
            };

            let field_ty = receiver_probe.kind.as_tup().unwrap().get(index).unwrap();
            self.ty_downgrade_eq(extra.target_ty.unwrap(), *field_ty)?;
            let (interrupt, assignee) = self.merge_result_info(once(receiver_expr))?;

            let mutbl = level.chain_mutbl(assignee.into());

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: *field_ty,
                    kind: ValueKind::ExtractElement { level, index },
                },
                AssigneeKind::Place(mutbl),
                interrupt,
            );
        } else {
            self.visit_expr(&receiver_expr, extra)?;
        }

        Ok(())
    }

    fn visit_index_expr<'tmp>(
        &mut self,
        IndexExpr(array_expr, index_expr): &'ast IndexExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let target_ty = extra.target_ty.unwrap();
            let array_intern = self.new_any_type();
            self.visit_expr(&array_expr, extra.replace_target(array_intern.into()))?;
            self.visit_expr(&index_expr, extra.replace_target(self.usize_type()))?;

            let (level, _, probe) = self
                .auto_deref(array_intern, |analyzer, intern| {
                    let Some(probe) = analyzer.probe_type(intern) else {
                        return Ok(None);
                    };
                    if probe.kind.is_array() {
                        Ok(Some(probe))
                    } else {
                        Ok(None)
                    }
                })?
                .ok_or(make_semantic_error!(NotArrayType).set_span(&array_expr.span))?;

            let inner_ty = *probe.kind.as_array().unwrap().0;
            self.ty_downgrade_eq(target_ty, inner_ty)?;

            self.no_assignee(array_expr.id)?;
            self.no_assignee(index_expr.id)?;

            let interrupt = self.merge_expr_interrupt(once(array_expr).chain(once(index_expr)));
            let mutbl = level.chain_mutbl(self.get_expr_result(&array_expr.id).assignee.into());

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: inner_ty,
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Place(mutbl),
                interrupt,
            );
        } else {
            self.visit_expr(&array_expr, extra)?;
            self.visit_expr(&index_expr, extra)?;
        }

        Ok(())
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
        UnderscoreExpr: &'ast UnderscoreExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if let Some(t) = extra.target_ty {
            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: t.clone(),
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Only,
                ControlFlowInterruptKind::Not,
            );
        }
        Ok(())
    }

    fn visit_path_expr<'tmp>(
        &mut self,
        PathExpr(qself, path): &'ast PathExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let value_index = self.search_value_by_path(extra.scope_id, qself, path)?;
            let value = self.get_place_value_by_index(&value_index);
            self.ty_downgrade_eq(extra.target_ty.unwrap(), value.value.ty)?;

            let mutbl = value.mutbl;

            self.set_expr_result(
                extra.self_id,
                ExprResult {
                    value_index: value_index.into(),
                    assignee: AssigneeKind::Place(mutbl),
                    interrupt: ControlFlowInterruptKind::Not,
                },
            );
        }

        Ok(())
    }

    fn visit_addr_of_expr<'tmp>(
        &mut self,
        AddrOfExpr(mutbl, expr): &'ast AddrOfExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let inner_ty = self.new_any_type();
            let self_ty = self.intern_type(ResolvedTy::ref_type(
                inner_ty,
                match mutbl {
                    Mutability::Not => RefMutability::Not,
                    Mutability::Mut => RefMutability::WeakMut,
                },
            ));

            // TODO: 有了 downgrade 后是否可以删除 WeakMut？
            self.ty_intern_eq(extra.target_ty.unwrap(), self_ty.into())?;

            self.visit_expr(&expr, extra.replace_target(inner_ty))?;

            self.no_assignee(expr.id)?;
            let (interrupt, assignee) = self.merge_result_info(once(expr))?;
            let inner_mutbl: Mutability = assignee.into();

            if inner_mutbl < *mutbl {
                return Err(make_semantic_error!(Immutable));
            }

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: self_ty.into(),
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(expr, extra)?;
        }

        Ok(())
    }

    fn visit_break_expr<'tmp>(
        &mut self,
        BreakExpr(expr): &'ast BreakExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let self_ty = self.never_type();
            self.ty_intern_eq(extra.target_ty.unwrap(), self_ty)?;

            let scope_id = self.search_cycle_scope(extra.scope_id)?;
            let scope = self.get_scope(scope_id);

            let interrupt = match (expr, &scope.kind) {
                (None, ScopeKind::CycleExceptLoop) => ControlFlowInterruptKind::Loop,
                (Some(_), ScopeKind::CycleExceptLoop) => {
                    return Err(make_semantic_error!(NotInLoopScope));
                }
                (Some(e), ScopeKind::Loop { ret_ty }) => {
                    self.visit_expr(e, extra.replace_target((*ret_ty).into()))?;
                    self.no_assignee(e.id)?;
                    let interrupt = self.get_expr_result(&e.id).interrupt;
                    interrupt + ControlFlowInterruptKind::Loop
                }
                (None, ScopeKind::Loop { ret_ty }) => {
                    self.ty_intern_eq((*ret_ty).into(), self.unit_type())?;
                    ControlFlowInterruptKind::Loop
                }
                _ => impossible!(),
            };

            self.set_expr_value_and_result(
                extra.self_id,
                self.never_value(),
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            if let Some(expr) = expr {
                self.visit_expr(expr, extra)?;
            }
        }

        Ok(())
    }

    fn visit_continue_expr<'tmp>(
        &mut self,
        ContinueExpr: &'ast ContinueExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            self.ty_intern_eq(extra.target_ty.unwrap(), self.never_type())?;

            self.search_cycle_scope(extra.scope_id)?;

            self.set_expr_value_and_result(
                extra.self_id,
                self.never_value(),
                AssigneeKind::Value,
                ControlFlowInterruptKind::Loop,
            );
        }

        Ok(())
    }

    fn visit_ret_expr<'tmp>(
        &mut self,
        RetExpr(expr): &'ast RetExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            self.ty_intern_eq(extra.target_ty.unwrap(), self.never_type())?;

            let scope_id = self.search_fn_scope(extra.scope_id)?;
            let ty = *self.get_scope(scope_id).kind.as_fn().unwrap().0;

            let interrupt = if let Some(e) = expr {
                self.visit_expr(e, extra.replace_target(ty.into()))?;
                self.no_assignee(e.id)?;
                self.get_expr_result(&e.id).interrupt + ControlFlowInterruptKind::Return
            } else {
                self.ty_intern_eq(ty.into(), self.unit_type())?;
                ControlFlowInterruptKind::Return
            };

            self.set_expr_value_and_result(
                extra.self_id,
                self.never_value(),
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            if let Some(e) = expr {
                self.visit_expr(e, extra)?;
            }
        }

        Ok(())
    }

    fn visit_struct_expr<'tmp>(
        &mut self,
        StructExpr {
            qself,
            path,
            fields,
            rest: _,
        }: &'ast StructExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let struct_type = self.resolve_path_type(qself, path, Some(extra.scope_id))?;

            // struct 不可能有 downgrade
            self.ty_intern_eq(extra.target_ty.unwrap(), struct_type)?;

            let probe = self.probe_type(struct_type).unwrap();
            let field_tys = probe
                .kind
                .as_tup()
                .ok_or(make_semantic_error!(NotStructType).set_span(&path.span))?
                .clone();
            let field_names = probe.names.as_ref().unwrap().1.as_ref().unwrap().clone();
            let mut map: HashMap<Symbol, TypeIntern> =
                HashMap::from_iter(field_names.into_iter().zip(field_tys.into_iter()));

            for ExprField {
                ident, expr, span, ..
            } in fields
            {
                let target = map
                    .remove(&ident.symbol)
                    .ok_or(make_semantic_error!(UnknownField).set_span(span))?;
                self.visit_expr(expr, extra.replace_target(target))?;
            }

            if !map.is_empty() {
                return Err(make_semantic_error!(MissingField));
            }

            let (interrupt, assignee) = self.merge_result_info(fields.iter().map(|x| &x.expr))?;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: struct_type,
                    kind: ValueKind::Anon,
                },
                assignee,
                interrupt,
            );
        } else {
            for ExprField { expr, .. } in fields {
                self.visit_expr(&expr, extra)?;
            }
        };

        Ok(())
    }

    fn visit_repeat_expr<'tmp>(
        &mut self,
        RepeatExpr(elm_expr, len_expr): &'ast RepeatExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if self.is_body_stage() {
            let len = self
                .const_eval(&len_expr.value, self.usize_type(), Some(extra.scope_id))?
                .into_constant_int()
                .unwrap();

            let inner_ty = self.new_any_type();
            let ty = self.intern_type(ResolvedTy::array_type(inner_ty, Some(len)));
            self.ty_intern_eq(extra.target_ty.unwrap(), ty.into())?;

            self.visit_expr(&elm_expr, extra.replace_target(inner_ty))?;

            self.no_assignee(elm_expr.id)?;
            let interrupt = self.get_expr_result(&elm_expr.id).interrupt;

            self.set_expr_value_and_result(
                extra.self_id,
                Value {
                    ty: ty.into(),
                    kind: ValueKind::Anon,
                },
                AssigneeKind::Value,
                interrupt,
            );
        } else {
            self.visit_expr(&elm_expr, extra)?;
        }

        Ok(())
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
        self.visit_ident_pat_impl(mode, ident, extra)
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
        self.visit_ident_pat_impl(&mode, ident, extra)
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
        let inner_ty = self.new_any_type();
        self.type_eq(extra.ty, ResolvedTy::ref_type(inner_ty, (*mutbl).into()))?;

        self.visit_pat(
            pat,
            PatExtra {
                ty: inner_ty,
                ..extra
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
