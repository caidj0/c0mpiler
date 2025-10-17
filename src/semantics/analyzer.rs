use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    vec,
};

use crate::{
    ast::{Crate, Mutability, NodeId, Span, Symbol, expr::*, item::*, pat::*, stmt::*},
    impossible, make_semantic_error,
    semantics::{
        error::SemanticError,
        expr::ExprResult,
        impls::Impls,
        item::{AssociatedInfo, ItemExtra},
        pat::PatResult,
        resolved_ty::{ResolvedTy, ResolvedTyKind, TypePtr},
        scope::{MainFunctionState, Scope, ScopeKind},
        stmt::StmtResult,
        utils::{AnalyzeStage, FullName, STAGES, is_all_different},
        value::{ConstantValue, UnEvalConstant, Value, ValueKind},
        visitor::Visitor,
    },
};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub(crate) scopes: HashMap<NodeId, Scope>,
    pub(crate) impls: HashMap<TypePtr, Impls>,
    pub(crate) expr_results: HashMap<NodeId, ExprResult>,
    pub(crate) expr_value: HashMap<NodeId, Value>,
    pub(crate) binding_value: HashMap<NodeId, Value>,

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
        value: Value,
    ) -> Result<&mut Value, SemanticError> {
        let scope = self.get_scope_mut(scope);
        let replace = scope.values.insert(name.clone(), value);

        if replace.is_some() {
            return Err(make_semantic_error!(ValueDefineConflict));
        }

        Ok(scope.values.get_mut(name).unwrap())
    }

    pub fn get_scope_value(&self, scope: NodeId, name: &Symbol) -> Option<&Value> {
        self.get_scope(scope).values.get(name)
    }

    pub fn get_scope_value_mut(&mut self, scope: NodeId, name: &Symbol) -> Option<&mut Value> {
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
        = Result<Option<&'res ExprResult>, SemanticError>
    where
        Self: 'res;

    type PatRes<'res>
        = Result<PatResult, SemanticError>
    where
        Self: 'res;

    type StmtRes<'res>
        = Result<StmtResult, SemanticError>
    where
        Self: 'res;

    type CrateExtra<'tmp> = NodeId;

    type ItemExtra<'tmp> = ItemExtra;

    type StmtExtra<'tmp> = ();

    type ExprExtra<'tmp> = ();

    type PatExtra<'tmp> = ();

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

                let value = Value {
                    ty: t,
                    mutbl: Mutability::Not,
                    kind: ValueKind::Constant(const_value),
                };

                if let Some(asso) = associated_info {
                    self.add_impl_value(&asso, &ident.symbol, value)
                } else {
                    self.add_scope_value(father, &ident.symbol, value)
                }
                .map_err(|e| e.set_span(&span))?;
            }
            AnalyzeStage::Body => {
                let value = if let Some(asso) = associated_info {
                    self.get_impl_value_mut(&asso, &ident.symbol).unwrap()
                } else {
                    self.get_scope_value_mut(father, &ident.symbol).unwrap()
                };
                let constant = value.kind.as_constant_mut().unwrap();

                if let ConstantValue::UnEval(u) = constant {
                    let u = u.clone();
                    let mut ty = value.ty.clone();
                    let v = self.eval_unevaling(&u, &mut ty)?;
                    let value_mut = self.get_scope_value_mut(father, &ident.symbol).unwrap();
                    value_mut.ty = ty;
                    *value_mut.kind.as_constant_mut().unwrap() = v;
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

                let value = Value {
                    ty: func_ty,
                    mutbl: Mutability::Not,
                    kind: ValueKind::Constant(ConstantValue::Fn {
                        is_method,
                        is_placeholder: body.is_none(),
                    }),
                };

                if let Some(asso) = associated_info {
                    self.add_impl_value(&asso, &ident.symbol, value)
                } else {
                    self.add_scope_value(father, &ident.symbol, value)
                }
                .map_err(|e| e.set_span(&span))?;
            }
            AnalyzeStage::Body => {
                // TODO: Add binding
            }
        }

        if let Some(body) = body {
            todo!()
        }
        // TODO: 返回类型校验

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
            self_id,
            span: _,
            associated_info: _,
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let ptr = self.add_type_placeholder(father, ident.symbol.clone())?;
                ptr.borrow_mut().kind = ResolvedTyKind::Enum;

                let enums = variants.iter().map(|x| x.ident.symbol.clone()).collect();
                self.add_scope(self_id, father, ScopeKind::Enum(ptr.clone(), enums));

                variants.iter().enumerate().try_for_each(|(i, x)| {
                    self.add_scope_value(
                        self_id,
                        &x.ident.symbol,
                        Value {
                            ty: ptr.clone(),
                            mutbl: Mutability::Not,
                            kind: ValueKind::Constant(ConstantValue::ConstantInt(i as u32)),
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
            let ty = self.get_self_type(Some(self_id)).unwrap();
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
                            .ty
                            .deep_clone();
                        trait_v.replace(trait_ty.borrow().name.as_ref().unwrap(), ty);
                        if trait_v != v.ty {
                            return Err(make_semantic_error!(AssociateItemMismatch));
                        }
                        names.remove(s);
                    }

                    for name in names {
                        let v = trait_info.values.get(name).unwrap();
                        match &v.kind {
                            ValueKind::Constant(
                                ConstantValue::Fn {
                                    is_method: _,
                                    is_placeholder: true,
                                }
                                | ConstantValue::Placeholder,
                            ) => {
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
        stmt: &'ast Stmt,
        extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        todo!()
    }

    fn visit_local_stmt<'tmp>(
        &mut self,
        stmt: &'ast LocalStmt,
        extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        todo!()
    }

    fn visit_expr<'tmp>(
        &mut self,
        expr: &'ast Expr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_array_expr<'tmp>(
        &mut self,
        expr: &'ast ArrayExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_const_block_expr<'tmp>(
        &mut self,
        expr: &'ast ConstBlockExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_call_expr<'tmp>(
        &mut self,
        expr: &'ast CallExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_method_call_expr<'tmp>(
        &mut self,
        expr: &'ast MethodCallExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_tup_expr<'tmp>(
        &mut self,
        expr: &'ast TupExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
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
        expr: &'ast LetExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
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
        expr: &'ast BlockExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
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
        expr: &'ast RangeExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
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

    fn visit_pat<'tmp>(&mut self, pat: &'ast Pat, extra: Self::PatExtra<'tmp>) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_wild_pat<'tmp>(
        &mut self,
        pat: &'ast WildPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_ident_pat<'tmp>(
        &mut self,
        pat: &'ast IdentPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_struct_pat<'tmp>(
        &mut self,
        pat: &'ast StructPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_or_pat<'tmp>(
        &mut self,
        pat: &'ast OrPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_path_pat<'tmp>(
        &mut self,
        pat: &'ast PathPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_tuple_pat<'tmp>(
        &mut self,
        pat: &'ast TuplePat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_ref_pat<'tmp>(
        &mut self,
        pat: &'ast RefPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_lit_pat<'tmp>(
        &mut self,
        pat: &'ast LitPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_range_pat<'tmp>(
        &mut self,
        pat: &'ast RangePat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_slice_pat<'tmp>(
        &mut self,
        pat: &'ast SlicePat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }

    fn visit_rest_pat<'tmp>(
        &mut self,
        pat: &'ast RestPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        todo!()
    }
}
