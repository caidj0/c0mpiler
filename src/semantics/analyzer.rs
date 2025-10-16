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
        item::ItemExtra,
        pat::PatResult,
        resolved_ty::{ResolvedTy, ResolvedTyKind, TypePtr},
        scope::{Scope, ScopeKind},
        stmt::StmtResult,
        utils::{AnalyzeStage, FullName, STAGES, is_all_different},
        value::{ConstantValue, UnEvalConstant, Value, ValueKind},
        visitor::Visitor,
    },
};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    scopes: HashMap<NodeId, Scope>,
    impls: HashMap<TypePtr, Impls>,
    expr_results: HashMap<NodeId, ExprResult>,
    expr_value: HashMap<NodeId, Value>,
    pat_value: HashMap<NodeId, Value>,

    stage: AnalyzeStage,
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
            pat_value: HashMap::default(),
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

    pub fn add_value(
        &mut self,
        scope: NodeId,
        name: &Symbol,
        value: Value,
    ) -> Result<&Value, SemanticError> {
        let scope = self.get_scope_mut(scope);
        let replace = scope.values.insert(name.clone(), value);

        if replace.is_some() {
            return Err(make_semantic_error!(ValueDefineConflict));
        }

        Ok(scope.values.get(name).unwrap())
    }

    pub fn get_value(&self, scope: NodeId, name: &Symbol) -> Option<&Value> {
        self.get_scope(scope).values.get(name)
    }

    pub fn get_value_mut(&mut self, scope: NodeId, name: &Symbol) -> Option<&mut Value> {
        self.get_scope_mut(scope).values.get_mut(name)
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
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {}
        }

        for item in items {
            self.visit_item(
                item,
                ItemExtra {
                    father: *id,
                    self_id: 0,
                    span: Span::default(),
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
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {}
            AnalyzeStage::Definition => {
                let t = self.resolve_type(&ty, Some(father))?;

                let const_value = match expr {
                    Some(e) => ConstantValue::UnEval(UnEvalConstant::new(father, e.as_ref())),
                    None => ConstantValue::Placeholder,
                };

                self.add_value(
                    father,
                    &ident.symbol,
                    Value {
                        ty: t,
                        mutbl: Mutability::Not,
                        kind: ValueKind::Constant(const_value),
                    },
                )?;
            }
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {
                let value = self.get_value_mut(father, &ident.symbol).unwrap();
                let constant = value.kind.as_constant_mut().unwrap();

                if let ConstantValue::UnEval(u) = constant {
                    todo!()
                }
            }
        }
        todo!()
    }

    fn visit_fn_item<'tmp>(
        &mut self,
        item: &'ast FnItem,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        todo!()
    }

    fn visit_mod_item<'tmp>(
        &mut self,
        _item: &'ast ModItem,
        ItemExtra {
            father: _,
            self_id: _,
            span,
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
        }: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let ptr = self.add_type_placeholder(father, ident.symbol.clone())?;

                let names = variants.iter().map(|x| x.ident.symbol.clone()).collect();
                self.add_scope(self_id, father, ScopeKind::Enum(ptr.clone(), names));

                variants.iter().enumerate().try_for_each(|(i, x)| {
                    self.add_value(
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
            AnalyzeStage::Impl => {}
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
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {}
        }

        Ok(())
    }

    fn visit_trait_item<'tmp>(
        &mut self,
        item: &'ast TraitItem,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        todo!()
    }

    fn visit_impl_item<'tmp>(
        &mut self,
        item: &'ast ImplItem,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        todo!()
    }

    fn visit_associate_item<'tmp>(
        &mut self,
        item: &'ast Item<AssocItemKind>,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        todo!()
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
