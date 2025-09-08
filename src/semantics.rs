pub mod const_eval;
pub mod error;
pub mod primitives;
pub mod resolved_ty;
pub mod utils;
pub mod visitor;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    vec,
};

use crate::{
    ast::{
        BindingMode, Crate, Mutability, NodeId, Span, Symbol,
        expr::{
            AssignExpr, AssignOp, AssignOpExpr, BinOp, BinaryExpr, BlockExpr, CallExpr, Expr,
            ExprKind, FieldExpr, IfExpr, LitKind, LoopExpr, MethodCallExpr, PathExpr, RepeatExpr,
            RetExpr, StructExpr, StructRest, UnOp, WhileExpr,
        },
        item::{
            AssocItemKind, ConstItem, EnumItem, FnItem, FnRetTy, ImplItem, Item, ItemKind,
            StructItem, TraitItem, TraitRef,
        },
        pat::{IdentPat, PathPat, RefPat},
        path::{Path, PathSegment, QSelf},
        stmt::{LocalKind, StmtKind},
        ty::{MutTy, PathTy, RefTy, Ty, TyKind},
    },
    lexer::TokenPosition,
    semantics::{
        const_eval::{ConstEvalError, ConstEvalValue},
        error::SemanticError,
        resolved_ty::ResolvedTy,
        utils::*,
        visitor::Visitor,
    },
};

macro_rules! no_assignee {
    ($id:expr) => {
        if matches!($id, ExprCategory::Only) {
            return Err(SemanticError::AssigneeOnlyExpr);
        }
    };
}

macro_rules! get_mutbl {
    ($id:expr) => {
        match $id {
            ExprCategory::Only => return Err(SemanticError::AssigneeOnlyExpr),
            ExprCategory::Place(mutbl) => mutbl,
            ExprCategory::Not => Mutability::Mut,
        }
    };
}

#[derive(Debug, Clone, Copy)]
pub struct AnalyzerState {
    pub current_ast_id: NodeId,
    pub current_span: Span,
}

impl Default for AnalyzerState {
    fn default() -> Self {
        Self {
            current_ast_id: Default::default(),
            current_span: Span {
                begin: TokenPosition { line: 0, col: 0 },
                end: TokenPosition { line: 0, col: 0 },
            },
        }
    }
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    type_table: RefCell<TypeTable>,
    impls: HashMap<TypeId, Impls>, // (本征 impl, trait impl)
    scopes: HashMap<NodeId, Scope>,
    current_scope: NodeId,
    stage: AnalyzeStage,
    state: AnalyzerState,

    builtin_impls: BulitInImpls,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut type_table: TypeTable = TypeTable::default();
        type_table.intern(ResolvedTy::unit()); // Unit -> 0
        type_table.intern(ResolvedTy::Infer); // Infer -> 1
        type_table.intern(ResolvedTy::Never); // Never -> 2

        let preludes = [
            (
                Symbol("print".to_string()),
                Variable {
                    ty: type_table.intern(ResolvedTy::Fn(
                        vec![ResolvedTy::ref_str()],
                        Box::new(ResolvedTy::unit()),
                    )),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("println".to_string()),
                Variable {
                    ty: type_table.intern(ResolvedTy::Fn(
                        vec![ResolvedTy::ref_str()],
                        Box::new(ResolvedTy::unit()),
                    )),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("printInt".to_string()),
                Variable {
                    ty: type_table.intern(ResolvedTy::Fn(
                        vec![ResolvedTy::i32()],
                        Box::new(ResolvedTy::unit()),
                    )),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("printlnInt".to_string()),
                Variable {
                    ty: type_table.intern(ResolvedTy::Fn(
                        vec![ResolvedTy::i32()],
                        Box::new(ResolvedTy::unit()),
                    )),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("getString".to_string()),
                Variable {
                    ty: type_table.intern(ResolvedTy::Fn(vec![], Box::new(ResolvedTy::string()))),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("getInt".to_string()),
                Variable {
                    ty: type_table.intern(ResolvedTy::Fn(vec![], Box::new(ResolvedTy::i32()))),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("exit".to_string()),
                Variable {
                    ty: type_table.intern(ResolvedTy::Fn(
                        vec![ResolvedTy::i32()],
                        Box::new(ResolvedTy::unit()),
                    )),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
        ];

        let root_scope = Scope {
            id: 0,
            kind: ScopeKind::Root,
            types: HashMap::new(),
            values: HashMap::from(preludes),
            children: HashSet::new(),
            father: 0,
        };

        let builtin_impls = BulitInImpls::new(&mut type_table);

        Self {
            type_table: RefCell::new(type_table),
            impls: HashMap::default(),
            scopes: HashMap::from([(0, root_scope)]),
            current_scope: 0,
            stage: AnalyzeStage::SymbolCollect,
            state: AnalyzerState::default(),
            builtin_impls,
        }
    }

    pub fn get_stage(&self) -> &AnalyzeStage {
        &self.stage
    }

    pub fn get_state(&self) -> &AnalyzerState {
        &self.state
    }

    fn get_scope_mut(&mut self) -> &mut Scope {
        self.scopes.get_mut(&self.current_scope).unwrap()
    }

    fn get_scope(&self) -> &Scope {
        self.scopes.get(&self.current_scope).unwrap()
    }

    fn get_cycle_scope_mut(&mut self) -> Result<&mut Scope, SemanticError> {
        let mut id = self.current_scope;

        loop {
            let scope = self.scopes.get(&id).unwrap();
            match scope.kind {
                ScopeKind::Lambda => {}
                ScopeKind::Root
                | ScopeKind::Crate
                | ScopeKind::Trait(_)
                | ScopeKind::Impl(_)
                | ScopeKind::Fn { ret_ty: _ } => return Err(SemanticError::NoLoopScope),
                ScopeKind::Loop { ret_ty: _ } | ScopeKind::CycleExceptLoop => {
                    return Ok(self.scopes.get_mut(&id).unwrap());
                }
            }
            id = scope.father;
        }
    }

    fn get_fn_scope(&self) -> Result<&Scope, SemanticError> {
        let mut id = self.current_scope;

        loop {
            let scope = self.scopes.get(&id).unwrap();
            match scope.kind {
                ScopeKind::Lambda | ScopeKind::Loop { ret_ty: _ } | ScopeKind::CycleExceptLoop => {}
                ScopeKind::Root | ScopeKind::Crate | ScopeKind::Trait(_) | ScopeKind::Impl(_) => {
                    return Err(SemanticError::NoFnScope);
                }
                ScopeKind::Fn { ret_ty: _ } => return Ok(scope),
            }
            id = scope.father;
        }
    }

    fn add_scope(&mut self, kind: ScopeKind) -> Result<(), SemanticError> {
        let id = self.state.current_ast_id;

        if self.scopes.contains_key(&id) || self.get_scope().children.contains(&id) {
            return Err(SemanticError::InvaildScope);
        }

        self.get_scope_mut().children.insert(id);
        self.scopes.insert(
            id,
            Scope {
                id: id,
                kind: kind,
                types: HashMap::new(),
                values: HashMap::new(),
                children: HashSet::new(),
                father: self.get_scope().id,
            },
        );

        Ok(())
    }

    fn enter_scope(&mut self) -> Result<(), SemanticError> {
        let id = self.state.current_ast_id;
        if !self.get_scope().children.contains(&id) {
            return Err(SemanticError::UndefinedScope);
        }

        self.current_scope = id;

        Ok(())
    }

    fn exit_scope(&mut self) -> Result<(), SemanticError> {
        if matches!(self.get_scope().kind, ScopeKind::Root) {
            return Err(SemanticError::InvaildScope);
        }

        self.current_scope = self.get_scope().father;

        Ok(())
    }

    fn add_type(&mut self, ident: Symbol, info: TypeInfo) -> Result<TypeId, SemanticError> {
        let s = self.get_scope_mut();

        if s.types.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.types.insert(ident.clone(), info);

        let resolved = ResolvedTy::Named(self.get_full_name(ident));

        Ok(self.intern_type(resolved))
    }

    // const 可以遮蔽同一作用域内的 local (function params)
    fn add_value(
        &mut self,
        ident: Symbol,
        var: Variable,
        shadow: bool,
    ) -> Result<(), SemanticError> {
        let s = self.get_scope_mut();

        if let Some(exist) = s.values.get(&ident)
            && !shadow
            && !var.kind.can_shadow_unconditionally(&exist.kind)
        {
            return Err(SemanticError::MultiDefined);
        }

        s.values.insert(ident, var);

        Ok(())
    }

    fn search_type_from(
        &self,
        ident: &Symbol,
        mut id: NodeId,
    ) -> Result<(NodeId, &TypeInfo), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if let Some(info) = scope.types.get(ident) {
                return Ok((id, info));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownType);
            } else {
                id = scope.father;
            }
        }
    }

    fn search_type_from_mut(
        &mut self,
        ident: &Symbol,
        mut id: NodeId,
    ) -> Result<(NodeId, &mut TypeInfo), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get_mut(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if scope.types.contains_key(ident) {
                // 不知道为什么，此处必须重新 get scope，否则会导致借用检查器报错
                return Ok((
                    id,
                    self.scopes
                        .get_mut(&id)
                        .unwrap()
                        .types
                        .get_mut(ident)
                        .unwrap(),
                ));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownType);
            } else {
                id = scope.father;
            }
        }
    }

    fn search_value_from(
        &self,
        ident: &Symbol,
        mut id: NodeId,
        mut include_local: bool,
    ) -> Result<(NodeId, &Variable), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if let Some(var) = scope.values.get(ident) {
                if !include_local && matches!(var.kind, VariableKind::Decl | VariableKind::Inited) {
                    return Err(SemanticError::LocalVarOutOfFn);
                }
                return Ok((id, var));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownVariable);
            } else {
                id = scope.father;

                if matches!(scope.kind, ScopeKind::Fn { ret_ty: _ }) {
                    include_local &= false;
                }
            }
        }
    }

    // fields 不包括 method，field 会自动 deref
    fn get_type_fields(
        &self,
        id: TypeId,
        symbol: &Symbol,
    ) -> Result<(Variable, DerefLevel), SemanticError> {
        let ty = self.get_type_by_id(id);

        match &ty {
            ResolvedTy::Named(symbols) => {
                let info = self.get_type_info(symbols);
                match &info.kind {
                    TypeKind::Placeholder => panic!("Impossible!"),
                    TypeKind::Struct { fields } => {
                        for (ident, field_id) in fields {
                            if ident == symbol {
                                return Ok((
                                    Variable {
                                        ty: *field_id,
                                        mutbl: Mutability::Mut,
                                        kind: VariableKind::Inited,
                                    },
                                    DerefLevel::Not,
                                ));
                            }
                        }
                    }
                    TypeKind::Enum { fields: _ } => {}
                    TypeKind::Trait {
                        methods: _,
                        constants: _,
                    } => {}
                }

                Err(SemanticError::NonProvidedField)
            }
            ResolvedTy::Ref(resolved_ty, mutability) => {
                let derefed_id = self.intern_type(resolved_ty.as_ref().clone());

                let (var, deref_level) = self.get_type_fields(derefed_id, symbol)?;

                Ok((var, deref_level.merge(DerefLevel::Deref(*mutability))))
            }
            ResolvedTy::ImplicitSelf => self.get_type_fields(self.get_self_type()?, symbol),
            _ => Err(SemanticError::NonProvidedField),
        }
    }

    fn get_type_items_noderef(
        &self,
        id: TypeId,
        is_methods_call: bool,
        name: &Symbol,
    ) -> Result<Option<TypeId>, SemanticError> {
        let ty = self.get_type_by_id(id);

        // builtin 在 get_impl 中返回，从而此处不需要特殊处理
        // visit_impl_item 时保证了 implinfo 中的 fn 只有第一位可能出现 Self 类型，其他位置的 Self 都被展开了
        if let Some((inherent_impl, trait_impls)) = self.get_impl(&id) {
            match inherent_impl.get(name) {
                Some(ImplInfoItem::Constant(constant)) => {
                    if !is_methods_call {
                        return Ok(Some(constant.ty));
                    }
                }
                Some(ImplInfoItem::Method(sig)) => {
                    let fn_ty = self.get_type_by_id(sig.type_id);

                    match (is_methods_call, fn_ty.is_method()) {
                        (true, true) => return Ok(Some(sig.type_id)),
                        (true, false) => {}
                        (false, true) => {
                            return Ok(Some(self.intern_type(fn_ty.method_to_func(&ty))));
                        }
                        (false, false) => return Ok(Some(sig.type_id)),
                    }
                }
                None => {}
            }

            let mut candidate = None;

            for (_, trait_impl) in trait_impls {
                match trait_impl.get(name) {
                    Some(ImplInfoItem::Constant(constant)) => {
                        if !is_methods_call {
                            if candidate.replace(constant.ty).is_some() {
                                return Err(SemanticError::MultipleApplicable);
                            }
                        }
                    }
                    Some(ImplInfoItem::Method(sig)) => {
                        let fn_ty = self.get_type_by_id(sig.type_id);

                        match (is_methods_call, fn_ty.is_method()) {
                            (true, true) => return Ok(Some(sig.type_id)),
                            (true, false) => {}
                            (false, true) => {
                                return Ok(Some(self.intern_type(fn_ty.method_to_func(&ty))));
                            }
                            (false, false) => return Ok(Some(sig.type_id)),
                        }
                    }
                    None => {}
                }
            }

            Ok(candidate)
        } else {
            Ok(None)
        }
    }

    // item 包含 method
    fn get_type_items(
        &self,
        mut id: TypeId,
        is_methods_call: bool,
        name: Symbol,
    ) -> Result<(TypeId, DerefLevel), SemanticError> {
        let mut deref_level = DerefLevel::Not;

        loop {
            if let Some(ret) = self.get_type_items_noderef(id, is_methods_call, &name)? {
                return Ok((ret, deref_level));
            }
            let ty = self.get_type_by_id(id).clone();
            match ty {
                ResolvedTy::Ref(resolved_ty, mutability) => {
                    id = self.intern_type(resolved_ty.as_ref().clone());
                    deref_level = deref_level.merge(DerefLevel::Deref(mutability))
                }
                _ => return Err(SemanticError::NonMethodCall),
            }
        }
    }

    fn search_type_mut(
        &mut self,
        ident: &Symbol,
    ) -> Result<(NodeId, &mut TypeInfo), SemanticError> {
        self.search_type_from_mut(ident, self.current_scope)
    }

    fn search_type_by_path(
        &self,
        qself: &Option<Box<QSelf>>,
        path: &Path,
    ) -> Result<(NodeId, &TypeInfo), SemanticError> {
        if qself.is_some() {
            return Err(SemanticError::Unimplemented);
        }

        if path.segments.len() > 1 {
            return Err(SemanticError::Unimplemented);
        }

        let first = path.segments.first().unwrap();
        if first.args.is_some() {
            return Err(SemanticError::Unimplemented);
        }

        self.search_type(&first.ident.symbol)
    }

    fn search_value_by_path(
        &self,
        qself: &Option<Box<QSelf>>,
        path: &Path,
    ) -> Result<(NodeId, &Variable), SemanticError> {
        if qself.is_some() {
            return Err(SemanticError::Unimplemented);
        }

        if path.segments.len() > 1 {
            return Err(SemanticError::Unimplemented);
        }

        let first = path.segments.first().unwrap();
        if first.args.is_some() {
            return Err(SemanticError::Unimplemented);
        }

        self.search_value(&first.ident.symbol)
    }

    fn search_type(&self, ident: &Symbol) -> Result<(NodeId, &TypeInfo), SemanticError> {
        self.search_type_from(ident, self.current_scope)
    }

    // 返回值为 (Scope Id, 变量类型)
    fn search_value(&self, ident: &Symbol) -> Result<(NodeId, &Variable), SemanticError> {
        self.search_value_from(ident, self.current_scope, true)
    }

    fn get_type_info(&self, full_name: &FullName) -> &TypeInfo {
        let symbols = &full_name.0;
        debug_assert!(symbols.len() >= 2);
        let scope_symbol = &symbols[symbols.len() - 2];
        let scope_id: NodeId = scope_symbol.0.strip_prefix("$").unwrap().parse().unwrap();
        let scope = self.scopes.get(&scope_id).unwrap();
        scope.types.get(symbols.last().unwrap()).unwrap()
    }

    pub fn visit(&mut self, krate: &crate::ast::Crate) -> Result<(), SemanticError> {
        for stage in [
            AnalyzeStage::SymbolCollect,
            AnalyzeStage::Definition,
            AnalyzeStage::Impl,
            AnalyzeStage::Body,
        ] {
            self.stage = stage;
            self.visit_crate(krate)?;
        }
        Ok(())
    }

    fn get_prefix_name_from(&self, mut id: NodeId) -> Vec<Symbol> {
        let mut ret = Vec::new();
        loop {
            let scope = self.scopes.get(&id).unwrap();
            ret.push(Symbol(format!("${}", scope.id)));
            if matches!(scope.kind, ScopeKind::Crate) {
                break;
            } else {
                id = scope.father;
            }
        }

        ret.reverse();
        ret
    }

    fn get_prefix_name(&self) -> Vec<Symbol> {
        self.get_prefix_name_from(self.current_scope)
    }

    fn get_full_name(&self, s: Symbol) -> FullName {
        let mut prefix = self.get_prefix_name();
        prefix.push(s);
        FullName(prefix)
    }

    fn get_full_name_from(&self, id: NodeId, s: Symbol) -> FullName {
        let mut prefix = self.get_prefix_name_from(id);
        prefix.push(s);
        FullName(prefix)
    }

    fn is_builtin_type(&self, ident: &Symbol, arg_num: usize) -> bool {
        const BUILTINS: [(&str, usize); 8] = [
            ("bool", 0),
            ("u32", 0),
            ("i32", 0),
            ("usize", 0),
            ("isize", 0),
            ("char", 0),
            ("str", 0),
            ("String", 0),
        ];

        for (bs, n) in BUILTINS {
            if ident.0 == bs && arg_num == n {
                return true;
            }
        }
        false
    }

    fn get_self_type_from(&self, mut id: NodeId) -> Result<TypeId, SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            match scope.kind {
                ScopeKind::Crate => return Err(SemanticError::UnknownType),
                ScopeKind::Trait(type_id) | ScopeKind::Impl(type_id) => return Ok(type_id),
                _ => id = scope.father,
            }
        }
    }

    fn get_self_type(&self) -> Result<TypeId, SemanticError> {
        self.get_self_type_from(self.current_scope)
    }

    fn resolve_ty_self_kind_specified(
        &self,
        ty: &Ty,
        expand_self: bool,
    ) -> Result<ResolvedTy, SemanticError> {
        match &ty.kind {
            TyKind::Slice(slice_ty) => Ok(ResolvedTy::Slice(Box::new(
                self.resolve_ty_self_kind_specified(&slice_ty.0, expand_self)?,
            ))),
            TyKind::Array(array_ty) => Ok(ResolvedTy::Array(
                Box::new(self.resolve_ty_self_kind_specified(&array_ty.0, expand_self)?),
                self.const_eval(&ResolvedTy::usize(), &array_ty.1.value)?
                    .into_u_size()
                    .unwrap(),
            )),
            TyKind::Ref(RefTy(MutTy { ty: ty2, mutbl })) => Ok(ResolvedTy::Ref(
                Box::new(self.resolve_ty_self_kind_specified(ty2, expand_self)?),
                *mutbl,
            )),
            TyKind::Tup(tup_ty) => Ok(ResolvedTy::Tup(
                tup_ty
                    .0
                    .iter()
                    .map(|x| self.resolve_ty_self_kind_specified(x, expand_self))
                    .collect::<Result<Vec<_>, SemanticError>>()?,
            )),
            TyKind::Path(path_ty) => {
                let PathTy(qself, path) = path_ty;
                if qself.is_some() {
                    return Err(SemanticError::Unimplemented);
                }

                if path.segments.len() > 1 {
                    return Err(SemanticError::InvaildPath);
                }

                let seg = path.segments.get(0).unwrap();
                let s = &seg.ident.symbol;
                if s.is_path_segment() {
                    if s.is_big_self() {
                        if seg.args.is_some() {
                            return Err(SemanticError::InvaildPath);
                        }
                        return Ok(if expand_self {
                            self.get_type_by_id(self.get_self_type()?).clone()
                        } else {
                            ResolvedTy::big_self()
                        });
                    }
                    return Err(SemanticError::InvaildPath);
                }
                match self.search_type(s) {
                    Ok((id, _)) => Ok(self.resolve_ty_in_scope_by_symbol(s, id)),
                    Err(err) => {
                        if self.is_builtin_type(
                            &seg.ident.symbol,
                            seg.args.as_ref().map_or(0, |x| match x.as_ref() {
                                crate::ast::generic::GenericArgs::AngleBracketed(
                                    angle_bracketed_args,
                                ) => angle_bracketed_args.args.len(),
                            }),
                        ) {
                            let mut args_v = Vec::new();
                            if let Some(args) = &seg.args {
                                match args.as_ref() {
                                    crate::ast::generic::GenericArgs::AngleBracketed(
                                        angle_bracketed_args,
                                    ) => {
                                        for x in &angle_bracketed_args.args {
                                            match x {
                                                crate::ast::generic::AngleBracketedArg::Arg(
                                                    generic_arg,
                                                ) => match generic_arg {
                                                    crate::ast::generic::GenericArg::Type(ty) => {
                                                        args_v.push(
                                                            self.resolve_ty_self_kind_specified(
                                                                ty,
                                                                expand_self,
                                                            )?,
                                                        )
                                                    }
                                                },
                                            }
                                        }
                                    }
                                }
                            }
                            Ok(ResolvedTy::BulitIn(s.clone(), args_v))
                        } else {
                            Err(err)
                        }
                    }
                }
            }
            TyKind::TraitObject(_) => Err(SemanticError::Unimplemented),
            TyKind::ImplTrait(_) => Err(SemanticError::Unimplemented),
            TyKind::Infer(_) => Ok(ResolvedTy::Infer),
            TyKind::ImplicitSelf => Ok(ResolvedTy::implicit_self()),
        }
    }

    // 这个函数不会展开 self（为了保留函数参数中的 self），但是会展开 Self
    fn resolve_ty(&self, ty: &Ty) -> Result<ResolvedTy, SemanticError> {
        self.resolve_ty_self_kind_specified(ty, true)
    }

    fn resolve_ty_in_scope_by_symbol(&self, s: &Symbol, id: usize) -> ResolvedTy {
        ResolvedTy::Named(self.get_full_name_from(id, s.clone()))
    }

    fn intern_type(&self, ty: ResolvedTy) -> TypeId {
        self.type_table.borrow_mut().intern(ty)
    }

    fn get_type_by_id(&self, id: TypeId) -> ResolvedTy {
        self.type_table.borrow().get(id).clone()
    }

    fn unit_type() -> TypeId {
        TypeId(0)
    }

    fn infer_type() -> TypeId {
        TypeId(1)
    }

    fn never_type() -> TypeId {
        TypeId(2)
    }

    fn unit_expr_result() -> ExprResult {
        ExprResult {
            type_id: Self::unit_type(),
            category: ExprCategory::Not,
        }
    }

    fn never_expr_result() -> ExprResult {
        ExprResult {
            type_id: Self::never_type(),
            category: ExprCategory::Not,
        }
    }

    fn const_eval(&self, ty: &ResolvedTy, expr: &Expr) -> Result<ConstEvalValue, SemanticError> {
        if let ResolvedTy::BulitIn(ident, _) = ty {
            match ident.0.as_str() {
                "u32" => {
                    return Ok(ConstEvalValue::U32(expr.try_into()?));
                }
                "usize" => {
                    return Ok(ConstEvalValue::USize(expr.try_into()?));
                }
                _ => {}
            }
        };

        return Err(SemanticError::ConstEvalError(
            ConstEvalError::NotSupportedExpr,
        ));
    }

    // 尝试统一传入的 type （主要是为了 integer）
    fn utilize_ty(&self, types: Vec<TypeId>) -> Result<TypeId, SemanticError> {
        let types: Vec<TypeId> = types
            .into_iter()
            .filter(|x| *x != Self::infer_type())
            .collect();

        if types.is_empty() {
            return Ok(Self::infer_type());
        }

        let mut ret_ty = self.get_type_by_id(*types.first().unwrap());
        for x in types {
            let t = self.get_type_by_id(x);
            if !t.can_trans_to_target_type(&ret_ty) {
                if ret_ty.can_trans_to_target_type(&t) {
                    ret_ty = t;
                } else {
                    return Err(SemanticError::TypeMismatch);
                }
            }
        }

        Ok(self.intern_type(ret_ty.clone()))
    }

    fn utilize_category(cats: Vec<ExprCategory>) -> Result<ExprCategory, SemanticError> {
        let has_only = cats.iter().any(|x| matches!(x, ExprCategory::Only));
        let has_value = cats.iter().any(|x| matches!(x, ExprCategory::Not));
        let has_immut = cats
            .iter()
            .any(|x| matches!(x, ExprCategory::Place(Mutability::Not)));

        match (has_only, has_value, has_immut) {
            (true, true, _) | (true, false, true) => Err(SemanticError::ConflictAssignee),
            (true, false, false) => Ok(ExprCategory::Only),
            (false, true, _) => Ok(ExprCategory::Not),
            (false, false, true) => Ok(ExprCategory::Place(Mutability::Not)),
            (false, false, false) => Ok(ExprCategory::Place(Mutability::Mut)),
        }
    }

    fn visit_block_expr_with_kind(
        &mut self,
        expr: &BlockExpr,
        kind: ScopeKind,
    ) -> Result<Option<ExprResult>, SemanticError> {
        let old_state = self.state;
        self.state = AnalyzerState {
            current_ast_id: expr.id,
            current_span: expr.span,
        };
        let mut ret;
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(kind)?;
                ret = None
            }
            AnalyzeStage::Definition | AnalyzeStage::Impl => {
                ret = None;
            }
            AnalyzeStage::Body => ret = Some(Self::unit_expr_result()),
        }

        self.enter_scope()?;
        let stmt_len = expr.stmts.len();
        for (index, stmt) in expr.stmts.iter().enumerate() {
            ret = self.visit_stmt(stmt)?;
            if let Some(r) = &ret {
                if index + 1 != stmt_len && r.type_id != Self::unit_type() {
                    return Err(SemanticError::TypeMismatch);
                }
                if matches!(r.category, ExprCategory::Only) {
                    return Err(SemanticError::AssigneeOnlyExpr);
                }
            }
        }

        if let Some(ret_res) = &mut ret {
            match &self.get_scope().kind {
                ScopeKind::Loop { ret_ty } => {
                    if ret_res.type_id != Self::unit_type() {
                        return Err(SemanticError::TypeMismatch);
                    }
                    if let Some(ret_ty) = ret_ty {
                        ret_res.type_id = *ret_ty;
                    }
                }
                _ => {}
            }
        }

        self.exit_scope()?;

        self.state = old_state;
        Ok(ret.map(|x| ExprResult {
            type_id: x.type_id,
            category: ExprCategory::Not,
        }))
    }

    fn is_free_scope(&self) -> bool {
        !matches!(
            self.get_scope().kind,
            ScopeKind::Trait(_) | ScopeKind::Impl(_)
        )
    }

    fn resolve_assoc_items(
        &mut self,
        items: &Vec<Box<Item<AssocItemKind>>>,
        is_trait_item: bool,
    ) -> Result<(HashMap<Symbol, FnSig>, HashMap<Symbol, Constant>), SemanticError> {
        let mut methods = HashMap::new();
        let mut constants = HashMap::new();
        let expand_self = !is_trait_item;

        for item in items {
            let old_state = self.state;
            self.state = AnalyzerState {
                current_ast_id: item.id,
                current_span: item.span,
            };
            match &item.kind {
                AssocItemKind::Const(ConstItem { ident, ty, expr }) => {
                    let resloved_ty = self.resolve_ty_self_kind_specified(&ty, expand_self)?;
                    let value = match expr {
                        Some(e) => self.const_eval(&resloved_ty, e)?,
                        None => {
                            if is_trait_item {
                                ConstEvalValue::Placeholder
                            } else {
                                return Err(SemanticError::ConstantWithoutBody);
                            }
                        }
                    };
                    let tyid = self.intern_type(resloved_ty);

                    if methods.contains_key(&ident.symbol) || constants.contains_key(&ident.symbol)
                    {
                        return Err(SemanticError::MultiDefined);
                    }

                    constants.insert(ident.symbol.clone(), Constant { ty: tyid, value });
                }
                AssocItemKind::Fn(FnItem {
                    ident,
                    generics: _,
                    sig,
                    body,
                }) => {
                    let param_tys = sig
                        .decl
                        .inputs
                        .iter()
                        .map(|x| self.resolve_ty_self_kind_specified(&x.ty, expand_self))
                        .collect::<Result<Vec<_>, SemanticError>>()?;
                    let ret_ty = match &sig.decl.output {
                        FnRetTy::Default => ResolvedTy::Tup(Vec::new()),
                        FnRetTy::Ty(ty) => self.resolve_ty_self_kind_specified(&ty, expand_self)?,
                    };
                    let fn_type = ResolvedTy::Fn(param_tys, Box::new(ret_ty));
                    let fn_type_id = self.intern_type(fn_type);

                    if methods.contains_key(&ident.symbol) || constants.contains_key(&ident.symbol)
                    {
                        return Err(SemanticError::MultiDefined);
                    }

                    if body.is_none() && !is_trait_item {
                        return Err(SemanticError::FnWithoutBody);
                    }

                    methods.insert(
                        ident.symbol.clone(),
                        FnSig {
                            type_id: fn_type_id,
                            is_placeholder: body.is_none(),
                        },
                    );
                }
            }
            self.state = old_state;
        }
        Ok((methods, constants))
    }

    fn get_impl(&self, id: &TypeId) -> Option<&Impls> {
        if let Some(i) = self.impls.get(id) {
            Some(i)
        } else {
            let ty = self.get_type_by_id(*id);

            if ty == ResolvedTy::u32() || ty == ResolvedTy::i32() {
                Some(&self.builtin_impls.u32_and_usize)
            } else if ty == ResolvedTy::string() {
                Some(&self.builtin_impls.string)
            } else if ty == ResolvedTy::str() {
                Some(&self.builtin_impls.str)
            } else if ty.is_array() || ty.is_slice() {
                Some(&self.builtin_impls.array_and_slice)
            } else {
                None
            }
        }
    }

    fn get_impl_mut(&mut self, id: &TypeId) -> &mut Impls {
        if !self.impls.contains_key(&id) {
            self.impls.insert(
                *id,
                self.get_impl(id).cloned().unwrap_or((
                    ImplInfo {
                        methods: HashMap::new(),
                        constants: HashMap::new(),
                    },
                    HashMap::new(),
                )),
            );
        }

        self.impls.get_mut(&id).unwrap()
    }

    fn add_bindings(
        &mut self,
        pat_res_es: Vec<PatResult>,
        kind: VariableKind,
    ) -> Result<(), SemanticError> {
        let mut set: HashSet<Symbol> = HashSet::new();

        for pat_res in pat_res_es {
            for (symbol, type_id, mutbl) in pat_res.bindings {
                if !set.insert(symbol.clone()) {
                    return Err(SemanticError::MultiBinding);
                }

                // bindings 不能遮蔽 const, 即使不同作用域
                // binding 还应该检查没有重名
                if let Ok((_, var)) = self.search_value(&symbol)
                    && var.kind.is_constant()
                {
                    return Err(SemanticError::ShadowedConstantByBinding);
                }

                self.add_value(
                    symbol,
                    Variable {
                        ty: type_id,
                        mutbl,
                        kind: kind.clone(),
                    },
                    true,
                )?;
            }
        }
        Ok(())
    }
}

impl Visitor for SemanticAnalyzer {
    type DefaultRes = Result<(), SemanticError>;
    type ExprRes = Result<Option<ExprResult>, SemanticError>;
    type PatRes = Result<PatResult, SemanticError>;

    fn visit_crate(&mut self, krate: &Crate) -> Result<(), SemanticError> {
        let old_state = self.state;
        self.state.current_ast_id = krate.id;

        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(ScopeKind::Crate)?;
            }
            AnalyzeStage::Definition => {}
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {}
        }

        self.enter_scope()?;
        for item in &krate.items {
            self.visit_item(item)?
        }
        self.exit_scope()?;
        self.state = old_state;
        Ok(())
    }

    fn visit_item(&mut self, item: &Item) -> Result<(), SemanticError> {
        let old_state = self.state;
        self.state = AnalyzerState {
            current_ast_id: item.id,
            current_span: item.span,
        };
        match &item.kind {
            ItemKind::Const(const_item) => self.visit_const_item(const_item)?,
            ItemKind::Fn(fn_item) => self.visit_fn_item(fn_item)?,
            ItemKind::Mod(mod_item) => self.visit_mod_item(mod_item)?,
            ItemKind::Enum(enum_item) => self.visit_enum_item(enum_item)?,
            ItemKind::Struct(struct_item) => self.visit_struct_item(struct_item)?,
            ItemKind::Trait(trait_item) => self.visit_trait_item(trait_item)?,
            ItemKind::Impl(impl_item) => self.visit_impl_item(impl_item)?,
        }
        self.state = old_state;
        Ok(())
    }

    fn visit_associate_item(&mut self, item: &Item<AssocItemKind>) -> Result<(), SemanticError> {
        let old_state = self.state;
        self.state = AnalyzerState {
            current_ast_id: item.id,
            current_span: item.span,
        };
        match &item.kind {
            AssocItemKind::Const(const_item) => self.visit_const_item(const_item)?,
            AssocItemKind::Fn(fn_item) => self.visit_fn_item(fn_item)?,
        }
        self.state = old_state;
        Ok(())
    }

    fn visit_const_item(
        &mut self,
        ConstItem { ident, ty, expr }: &ConstItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {}
            AnalyzeStage::Definition => {
                if self.is_free_scope() {
                    let ty = self.resolve_ty(&ty)?;
                    let value = self.const_eval(
                        &ty,
                        expr.as_ref().ok_or(SemanticError::ConstantWithoutBody)?,
                    )?;
                    let tyid = self.intern_type(ty);
                    self.add_value(
                        ident.symbol.clone(),
                        Variable {
                            ty: tyid,
                            mutbl: Mutability::Not,
                            kind: VariableKind::Constant(value),
                        },
                        false,
                    )?;
                }
            }
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {}
        }

        Ok(())
    }

    fn visit_fn_item(
        &mut self,
        FnItem {
            ident,
            generics: _,
            sig,
            body,
        }: &FnItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(ScopeKind::Fn { ret_ty: TypeId(0) })?;
            }
            AnalyzeStage::Definition => {
                let ret_ty = match &sig.decl.output {
                    FnRetTy::Default => ResolvedTy::Tup(Vec::new()),
                    FnRetTy::Ty(ty) => self.resolve_ty(&ty)?,
                };

                let param_tys = sig
                    .decl
                    .inputs
                    .iter()
                    .map(|x| self.resolve_ty(&x.ty))
                    .collect::<Result<Vec<_>, SemanticError>>()?;

                let params_ty_ids = param_tys
                    .iter()
                    .map(|x| self.intern_type(x.clone()))
                    .collect::<Vec<_>>();
                let bindings = sig
                    .decl
                    .inputs
                    .iter()
                    .zip(params_ty_ids.into_iter())
                    .map(|(param, id)| self.visit_pat(&param.pat, id))
                    .collect::<Result<Vec<_>, SemanticError>>()?;

                if self.is_free_scope() {
                    let tyid =
                        self.intern_type(ResolvedTy::Fn(param_tys, Box::new(ret_ty.clone())));

                    self.add_value(
                        ident.symbol.clone(),
                        Variable {
                            ty: tyid,
                            mutbl: Mutability::Not,
                            kind: VariableKind::Fn,
                        },
                        false,
                    )?;
                }

                self.enter_scope()?;

                let ret_ty_id = self.intern_type(ret_ty);
                // local 函数参数能被 item 遮蔽，从而应该是在此处添加到 scope 的
                self.add_bindings(bindings, VariableKind::Inited)?;
                *self.get_scope_mut().kind.as_fn_mut().unwrap() = ret_ty_id;

                self.exit_scope()?;
            }
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {}
        }
        self.enter_scope()?;
        if let Some(b) = body {
            let res = self.visit_block_expr(b)?;
            if let Some(res) = res {
                no_assignee!(res.category);
                // TODO: 如何兼容 block 的尾随返回和 return expr？重新引入 Never Type？
                let target_ty_id = self.get_scope().kind.as_fn().unwrap();
                let target_ty = self.get_type_by_id(*target_ty_id);
                let ret_ty = self.get_type_by_id(res.type_id);
                if !ret_ty.can_trans_to_target_type(&target_ty) {
                    return Err(SemanticError::TypeMismatch);
                }
            }
        }
        self.exit_scope()?;
        Ok(())
    }

    fn visit_mod_item(&mut self, _: &crate::ast::item::ModItem) -> Result<(), SemanticError> {
        Err(SemanticError::Unimplemented)
    }

    fn visit_enum_item(
        &mut self,
        EnumItem(ident, _, variants): &EnumItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_type(
                    ident.symbol.clone(),
                    TypeInfo {
                        name: ident.symbol.clone(),
                        kind: TypeKind::Enum {
                            fields: HashSet::new(),
                        },
                    },
                )?;
            }
            AnalyzeStage::Definition => {
                let fields = variants
                    .iter()
                    .map(|x| {
                        if !matches!(x.data, crate::ast::item::VariantData::Unit)
                            || x.disr_expr.is_some()
                        {
                            Err(SemanticError::Unimplemented)
                        } else {
                            Ok(x.ident.symbol.clone())
                        }
                    })
                    .collect::<Result<Vec<_>, SemanticError>>()?;

                let vec_len = fields.len();
                let fields: HashSet<Symbol> = HashSet::from_iter(fields.into_iter());
                if vec_len != fields.len() {
                    return Err(SemanticError::MultiDefined);
                }

                *self
                    .search_type_mut(&ident.symbol)?
                    .1
                    .kind
                    .as_enum_mut()
                    .unwrap() = fields;
            }
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {}
        }
        Ok(())
    }

    fn visit_struct_item(
        &mut self,
        StructItem(ident, _, variant_data): &crate::ast::item::StructItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_type(
                    ident.symbol.clone(),
                    TypeInfo {
                        name: ident.symbol.clone(),
                        kind: TypeKind::Struct {
                            fields: HashMap::new(),
                        },
                    },
                )?;
            }
            AnalyzeStage::Definition => {
                let fields = match variant_data {
                    crate::ast::item::VariantData::Struct { fields } => fields
                        .iter()
                        .map(|x| {
                            Ok((
                                x.ident
                                    .as_ref()
                                    .ok_or(SemanticError::Unimplemented)?
                                    .symbol
                                    .clone(),
                                self.intern_type(self.resolve_ty(&x.ty)?),
                            ))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?,
                    crate::ast::item::VariantData::Tuple(_) => {
                        return Err(SemanticError::Unimplemented);
                    }
                    crate::ast::item::VariantData::Unit => Vec::new(),
                };

                let vec_len = fields.len();
                let fields: HashMap<Symbol, TypeId> = HashMap::from_iter(fields.into_iter());
                if vec_len != fields.len() {
                    return Err(SemanticError::MultiDefined);
                }

                let (id, info) = self.search_type_mut(&ident.symbol)?;
                *info.kind.as_struct_mut().unwrap() = fields;

                // unit struct 隐式添加了一个 constant
                if variant_data.is_unit() {
                    let self_ty = self.resolve_ty_in_scope_by_symbol(&ident.symbol, id);
                    let self_ty_id = self.intern_type(self_ty);
                    self.add_value(
                        ident.symbol.clone(),
                        Variable {
                            ty: self_ty_id,
                            mutbl: Mutability::Not,
                            kind: VariableKind::Constant(ConstEvalValue::UnitStruct),
                        },
                        false,
                    )?;
                }
            }
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {} // 先认为 body 阶段什么也不用做，除非在定义 array 长度时做些什么
        }
        Ok(())
    }

    fn visit_trait_item(
        &mut self,
        TraitItem {
            ident,
            generics: _,
            bounds: _,
            items,
        }: &crate::ast::item::TraitItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let id = self.add_type(
                    ident.symbol.clone(),
                    TypeInfo {
                        name: ident.symbol.clone(),
                        kind: TypeKind::Trait {
                            methods: HashMap::new(),
                            constants: HashMap::new(),
                        },
                    },
                )?;
                self.add_scope(ScopeKind::Trait(id))?;
            }
            AnalyzeStage::Definition => {
                let (methods, constants) = self.resolve_assoc_items(items, true)?;
                self.search_type_mut(&ident.symbol)?.1.kind =
                    TypeKind::Trait { methods, constants };
            }
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {} // 应该也什么也不用做？
        }
        self.enter_scope()?;
        for item in items {
            self.visit_associate_item(item)?;
        }
        self.exit_scope()?;
        Ok(())
    }

    fn visit_impl_item(
        &mut self,
        ImplItem {
            generics: _,
            of_trait,
            self_ty,
            items,
        }: &ImplItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                // 0 type 作为 self ty 的占位符
                self.add_scope(ScopeKind::Impl(TypeId(0)))?;
            }
            AnalyzeStage::Definition => {
                let self_ty = self.resolve_ty(&self_ty)?;
                let self_ty_id = self.intern_type(self_ty);

                self.enter_scope()?;
                *self.get_scope_mut().kind.as_impl_mut().unwrap() = self_ty_id;
                self.exit_scope()?;
            }
            AnalyzeStage::Impl => {
                self.enter_scope()?;
                let self_ty_id = self.get_scope().kind.as_impl().unwrap().clone();
                let self_ty = self.get_type_by_id(self_ty_id);

                // trait 中使用 Self 作为类型，送到 impl 时替换为具体的 Type
                let (methods, constants) = self.resolve_assoc_items(items, false)?;

                match of_trait {
                    Some(TraitRef { path: trait_path }) => {
                        let (id, info) = self.search_type_by_path(&None, trait_path)?;
                        let full_name =
                            self.get_full_name_from(id, trait_path.get_symbol().clone());

                        let TypeKind::Trait {
                            methods: trait_methods,
                            constants: trait_constants,
                        } = &info.kind
                        else {
                            return Err(SemanticError::NotTrait);
                        };

                        let mut mustes = HashSet::new();
                        mustes.extend(trait_methods.iter().filter_map(|(symbol, sig)| {
                            if sig.is_placeholder {
                                Some(symbol)
                            } else {
                                None
                            }
                        }));
                        mustes.extend(trait_constants.iter().filter_map(|(symbol, constant)| {
                            if matches!(constant.value, ConstEvalValue::Placeholder) {
                                Some(symbol)
                            } else {
                                None
                            }
                        }));

                        for (symbol, sig) in &methods {
                            let Some(trait_sig) = trait_methods.get(&symbol).map(|x| FnSig {
                                type_id: self.intern_type(
                                    self.get_type_by_id(x.type_id).expand_self(&self_ty),
                                ),
                                ..*x
                            }) else {
                                return Err(SemanticError::NotTraitMember);
                            };

                            if sig.type_id != trait_sig.type_id {
                                return Err(SemanticError::IncompatibleFn);
                            }

                            mustes.remove(&symbol);
                        }

                        for (symbol, constant) in &constants {
                            let Some(trait_constant) =
                                trait_constants.get(&symbol).map(|x| Constant {
                                    ty: self.intern_type(
                                        self.get_type_by_id(x.ty).expand_self(&self_ty),
                                    ),
                                    value: x.value.clone(),
                                })
                            else {
                                return Err(SemanticError::NotTraitMember);
                            };

                            if constant.ty != trait_constant.ty {
                                return Err(SemanticError::TypeMismatch);
                            }

                            mustes.remove(&symbol);
                        }

                        if !mustes.is_empty() {
                            return Err(SemanticError::NotAllTraitItemsImplemented);
                        }

                        let (_, trait_impls) = self.get_impl_mut(&self_ty_id);

                        if trait_impls.contains_key(&full_name) {
                            return Err(SemanticError::MultiImplemented);
                        }

                        trait_impls.insert(
                            full_name,
                            ImplInfo {
                                methods: methods,
                                constants: constants,
                            },
                        );
                    }
                    None => {
                        let (inherent_impl, _) = self.get_impl_mut(&self_ty_id);

                        for (symbol, sig) in methods {
                            if inherent_impl.contains_key(&symbol) {
                                return Err(SemanticError::MultiDefined);
                            }
                            inherent_impl.methods.insert(symbol, sig);
                        }

                        for (symbol, constant) in constants {
                            if inherent_impl.contains_key(&symbol) {
                                return Err(SemanticError::MultiDefined);
                            }
                            inherent_impl.constants.insert(symbol, constant);
                        }
                    }
                }

                self.exit_scope()?;
            }
            AnalyzeStage::Body => {}
        }
        self.enter_scope()?;
        for item in items {
            self.visit_associate_item(item)?;
        }
        self.exit_scope()?;
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::stmt::Stmt) -> Self::ExprRes {
        let old_state = self.state;
        self.state.current_span = stmt.span;

        let default_ret = match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => Ok(Some(Self::unit_expr_result())),
        };

        let ret = match &stmt.kind {
            StmtKind::Let(local_stmt) => {
                self.visit_let_stmt(local_stmt)?;
                default_ret
            }
            StmtKind::Item(item) => {
                self.visit_item(item)?;
                default_ret
            }
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Semi(expr) => self
                .visit_expr(expr)
                .map(|x| x.map(|_| Self::unit_expr_result())),
            StmtKind::Empty(_) => default_ret,
        };

        if matches!(ret, Ok(_)) {
            self.state = old_state;
        }

        ret
    }

    fn visit_let_stmt(&mut self, stmt: &crate::ast::stmt::LocalStmt) -> Result<(), SemanticError> {
        let Some(ty) = &stmt.ty else {
            return Err(SemanticError::Unimplemented);
        };

        match &stmt.kind {
            LocalKind::Decl => {
                if matches!(self.stage, AnalyzeStage::Body) {
                    // 从下面复制上来的，因为不用检查变量是否初始化
                    let expected_ty = self.resolve_ty(&ty)?;
                    let expected_ty_id = self.intern_type(expected_ty.clone());
                    let pat_res = self.visit_pat(&stmt.pat, expected_ty_id)?;
                    self.add_bindings(vec![pat_res], VariableKind::Inited)?;
                }
            }
            LocalKind::Init(expr) => {
                let res = self.visit_expr(&expr)?;
                match res {
                    Some(ExprResult { type_id, category }) => {
                        debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                        no_assignee!(category);
                        let expected_ty = self.resolve_ty(&ty)?;
                        let expected_ty_id = self.intern_type(expected_ty.clone());
                        let expr_ty = self.get_type_by_id(type_id);
                        if expr_ty.can_trans_to_target_type(&expected_ty) {
                            let pat_res = self.visit_pat(&stmt.pat, expected_ty_id)?;
                            self.add_bindings(vec![pat_res], VariableKind::Inited)?;
                        } else {
                            return Err(SemanticError::TypeMismatch);
                        }
                    }
                    None => {}
                }
            }
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprRes {
        let old_state = self.state;
        self.state.current_span = expr.span;
        let res = match &expr.kind {
            ExprKind::Array(array_expr) => self.visit_array_expr(array_expr),
            ExprKind::ConstBlock(const_block_expr) => self.visit_const_block_expr(const_block_expr),
            ExprKind::Call(call_expr) => self.visit_call_expr(call_expr),
            ExprKind::MethodCall(method_call_expr) => self.visit_method_call_expr(method_call_expr),
            ExprKind::Tup(tup_expr) => self.visit_tup_expr(tup_expr),
            ExprKind::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            ExprKind::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
            ExprKind::Lit(lit_expr) => self.visit_lit_expr(lit_expr),
            ExprKind::Cast(cast_expr) => self.visit_cast_expr(cast_expr),
            ExprKind::Let(let_expr) => self.visit_let_expr(let_expr),
            ExprKind::If(if_expr) => self.visit_if_expr(if_expr),
            ExprKind::While(while_expr) => self.visit_while_expr(while_expr),
            ExprKind::ForLoop(for_loop_expr) => self.visit_for_loop_expr(for_loop_expr),
            ExprKind::Loop(loop_expr) => self.visit_loop_expr(loop_expr),
            ExprKind::Match(match_expr) => self.visit_match_expr(match_expr),
            ExprKind::Block(block_expr) => self.visit_block_expr(block_expr),
            ExprKind::Assign(assign_expr) => self.visit_assign_expr(assign_expr),
            ExprKind::AssignOp(assign_op_expr) => self.visit_assign_op_expr(assign_op_expr),
            ExprKind::Field(field_expr) => self.visit_field_expr(field_expr),
            ExprKind::Index(index_expr) => self.visit_index_expr(index_expr),
            ExprKind::Range(range_expr) => self.visit_range_expr(range_expr),
            ExprKind::Underscore(underscore_expr) => self.visit_underscore_expr(underscore_expr),
            ExprKind::Path(path_expr) => self.visit_path_expr(path_expr),
            ExprKind::AddrOf(addr_of_expr) => self.visit_addr_of_expr(addr_of_expr),
            ExprKind::Break(break_expr) => self.visit_break_expr(break_expr),
            ExprKind::Continue(continue_expr) => self.visit_continue_expr(continue_expr),
            ExprKind::Ret(ret_expr) => self.visit_ret_expr(ret_expr),
            ExprKind::Struct(struct_expr) => self.visit_struct_expr(struct_expr),
            ExprKind::Repeat(repeat_expr) => self.visit_repeat_expr(repeat_expr),
        }?;
        self.state = old_state;
        Ok(res)
    }

    fn visit_array_expr(&mut self, expr: &crate::ast::expr::ArrayExpr) -> Self::ExprRes {
        let expr_res = expr
            .0
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<Option<ExprResult>>, SemanticError>>()?;

        if !matches!(self.stage, AnalyzeStage::Body) {
            debug_assert!(expr_res.iter().all(|x| x.is_none()));
            return Ok(None);
        }

        let (types, cats): (Vec<_>, Vec<_>) = expr_res
            .into_iter()
            .map(|x| {
                let ExprResult { type_id, category } = x.unwrap();
                (type_id, category)
            })
            .unzip();

        let unified_type_id = self.utilize_ty(types)?;
        let unified_ty = self.get_type_by_id(unified_type_id);
        let cat = Self::utilize_category(cats)?;

        let ret_ty = ResolvedTy::Array(Box::new(unified_ty.clone()), expr.0.len() as u32);

        Ok(Some(ExprResult {
            type_id: self.intern_type(ret_ty),
            category: cat,
        }))
    }

    fn visit_const_block_expr(
        &mut self,
        _expr: &crate::ast::expr::ConstBlockExpr,
    ) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_call_expr(&mut self, CallExpr(expr, params): &CallExpr) -> Self::ExprRes {
        let res = self.visit_expr(expr)?;
        let params_res = params
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<_>, SemanticError>>()?;

        match res {
            Some(res) => {
                no_assignee!(res.category);
                let params_res = params_res.into_iter().collect::<Option<Vec<_>>>().unwrap();
                for x in &params_res {
                    no_assignee!(x.category);
                }

                let ty = self.get_type_by_id(res.type_id);
                if let ResolvedTy::Fn(required, ret_ty) = ty {
                    if required.len() != params_res.len() {
                        return Err(SemanticError::MismatchArgNum);
                    }

                    for (income, target) in params_res.into_iter().zip(required.iter()) {
                        debug_assert!(!target.is_implicit_self_or_ref_implicit_self());
                        let income_ty = self.get_type_by_id(income.type_id);

                        if !income_ty.can_trans_to_target_type(target) {
                            return Err(SemanticError::TypeMismatch);
                        }
                    }

                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ret_ty.as_ref().clone()),
                        category: ExprCategory::Not,
                    }))
                } else {
                    Err(SemanticError::NonFunctionCall)
                }
            }
            None => {
                debug_assert!(params_res.iter().all(|x| x.is_none()));
                Ok(None)
            }
        }
    }

    fn visit_method_call_expr(
        &mut self,
        MethodCallExpr {
            seg,
            receiver,
            args,
            span: _,
        }: &MethodCallExpr,
    ) -> Self::ExprRes {
        let res = self.visit_expr(&receiver)?;
        let params_res = args
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<_>, SemanticError>>()?;

        match res {
            Some(ExprResult { type_id, category }) => {
                no_assignee!(category);
                let params_res = params_res.into_iter().collect::<Option<Vec<_>>>().unwrap();
                for x in &params_res {
                    no_assignee!(x.category);
                }
                let ident = &seg.ident;
                if seg.args.is_some() {
                    return Err(SemanticError::Unimplemented);
                }

                let (fn_id, devel_level) = self.get_type_items(
                    type_id,
                    true, /* 此处保证 items 中均为 Fn Variant 且首个参数为 self */
                    ident.symbol.clone(),
                )?;

                let fn_ty = self.get_type_by_id(fn_id);
                let (required_tys, ret_ty) = fn_ty.into_fn().unwrap();

                if required_tys.len() != params_res.len() + 1 {
                    return Err(SemanticError::MismatchArgNum);
                }

                match required_tys.first().unwrap() {
                    ResolvedTy::Ref(_, target_mut) => {
                        let self_mut = get_mutbl!(category).merge(devel_level.get_mutbl());
                        if !self_mut.can_trans_to(target_mut) {
                            return Err(SemanticError::ImmutableVar);
                        }
                    }
                    ResolvedTy::ImplicitSelf => {
                        if matches!(devel_level, DerefLevel::Deref(_)) {
                            return Err(SemanticError::UnDereferenceable);
                        }
                    }
                    _ => panic!("Impossible"),
                }

                for (income, target) in params_res.iter().zip(required_tys[1..].iter()) {
                    debug_assert!(!target.is_implicit_self_or_ref_implicit_self());
                    let income_ty = self.get_type_by_id(income.type_id);

                    if !income_ty.can_trans_to_target_type(target) {
                        return Err(SemanticError::TypeMismatch);
                    }
                }

                Ok(Some(ExprResult {
                    type_id: self.intern_type(ret_ty.as_ref().clone()),
                    category: ExprCategory::Not,
                }))
            }
            None => {
                debug_assert!(params_res.iter().all(|x| x.is_none()));
                Ok(None)
            }
        }
    }

    fn visit_tup_expr(&mut self, expr: &crate::ast::expr::TupExpr) -> Self::ExprRes {
        match &expr.0[..] {
            [] => Ok(if matches!(self.stage, AnalyzeStage::Body) {
                Some(ExprResult {
                    type_id: Self::unit_type(),
                    category: ExprCategory::Place(Mutability::Mut),
                })
            } else {
                None
            }),
            [e] => self.visit_expr(e),
            _ => Err(SemanticError::Unimplemented),
        }
    }

    fn visit_binary_expr(
        &mut self,
        BinaryExpr(bin_op, expr1, expr2): &BinaryExpr,
    ) -> Self::ExprRes {
        let res1 = self.visit_expr(expr1)?;
        let res2 = self.visit_expr(expr2)?;

        match (res1, res2) {
            (Some(res1), Some(res2)) => {
                debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                no_assignee!(res1.category);
                no_assignee!(res2.category);

                // TODO: &1 + &2 之类的
                let ty1 = self.get_type_by_id(res1.type_id);
                let ty2 = self.get_type_by_id(res2.type_id);

                macro_rules! general_bin_op {
                    ($op_exp:expr, $ty1:expr, $ty2:expr, $op:pat, ($($ty:expr), *)) => {
                        matches!($op_exp, $op) && ($(($ty1 == $ty && $ty2.can_trans_to_target_type(&$ty1)))||*)
                    };
                }

                macro_rules! shift_bin_op {
                    ($op_exp:expr, $ty1:expr, $ty2:expr, $op:pat, ($($ty:expr), *)) => {
                        matches!($op_exp, $op) && $ty2.is_number_type() && ($($ty1 == $ty)||*)
                    };
                }

                if (matches!(bin_op, BinOp::Add))
                    && ty1 == ResolvedTy::string()
                    && ty2 == ResolvedTy::ref_str()
                {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ResolvedTy::string()),
                        category: ExprCategory::Not,
                    }))
                } else if (matches!(bin_op, BinOp::Eq | BinOp::Ne)
                    && ((ty1 == ResolvedTy::string() || ty1 == ResolvedTy::ref_str())
                        && (ty2 == ResolvedTy::ref_str() || ty2 == ResolvedTy::string())))
                    || general_bin_op!(
                        bin_op,
                        ty1,
                        ty2,
                        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Ge | BinOp::Gt,
                        (
                            ResolvedTy::integer(),
                            ResolvedTy::signed_integer(),
                            ResolvedTy::i32(),
                            ResolvedTy::u32(),
                            ResolvedTy::isize(),
                            ResolvedTy::usize(),
                            ResolvedTy::bool(),
                            ResolvedTy::char()
                        )
                    )
                {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ResolvedTy::bool()),
                        category: ExprCategory::Not,
                    }))
                } else if general_bin_op!(
                    bin_op,
                    ty1,
                    ty2,
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem,
                    (
                        ResolvedTy::integer(),
                        ResolvedTy::signed_integer(),
                        ResolvedTy::i32(),
                        ResolvedTy::u32(),
                        ResolvedTy::isize(),
                        ResolvedTy::usize()
                    )
                ) || general_bin_op!(
                    bin_op,
                    ty1,
                    ty2,
                    BinOp::And | BinOp::Or,
                    (ResolvedTy::bool())
                ) || general_bin_op!(
                    bin_op,
                    ty1,
                    ty2,
                    BinOp::BitXor | BinOp::BitAnd | BinOp::BitOr,
                    (
                        ResolvedTy::integer(),
                        ResolvedTy::signed_integer(),
                        ResolvedTy::i32(),
                        ResolvedTy::u32(),
                        ResolvedTy::isize(),
                        ResolvedTy::usize(),
                        ResolvedTy::bool()
                    )
                ) || shift_bin_op!(
                    bin_op,
                    ty1,
                    ty2,
                    BinOp::Shl | BinOp::Shr,
                    (
                        ResolvedTy::integer(),
                        ResolvedTy::signed_integer(),
                        ResolvedTy::i32(),
                        ResolvedTy::u32(),
                        ResolvedTy::isize(),
                        ResolvedTy::usize()
                    )
                ) {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ty1.clone()),
                        category: ExprCategory::Not,
                    }))
                } else if general_bin_op!(
                    bin_op,
                    ty2,
                    ty1,
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem,
                    (
                        ResolvedTy::integer(),
                        ResolvedTy::signed_integer(),
                        ResolvedTy::i32(),
                        ResolvedTy::u32(),
                        ResolvedTy::isize(),
                        ResolvedTy::usize()
                    )
                ) || general_bin_op!(
                    bin_op,
                    ty2,
                    ty1,
                    BinOp::And | BinOp::Or,
                    (ResolvedTy::bool())
                ) || general_bin_op!(
                    bin_op,
                    ty2,
                    ty1,
                    BinOp::BitXor | BinOp::BitAnd | BinOp::BitOr,
                    (
                        ResolvedTy::integer(),
                        ResolvedTy::signed_integer(),
                        ResolvedTy::i32(),
                        ResolvedTy::u32(),
                        ResolvedTy::isize(),
                        ResolvedTy::usize(),
                        ResolvedTy::bool()
                    )
                ) || shift_bin_op!(
                    bin_op,
                    ty2,
                    ty1,
                    BinOp::Shl | BinOp::Shr,
                    (
                        ResolvedTy::integer(),
                        ResolvedTy::signed_integer(),
                        ResolvedTy::i32(),
                        ResolvedTy::u32(),
                        ResolvedTy::isize(),
                        ResolvedTy::usize()
                    )
                ) {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ty2.clone()),
                        category: ExprCategory::Not,
                    }))
                } else {
                    Err(SemanticError::Unimplemented)
                }
            }
            (None, Some(_)) | (Some(_), None) => panic!("Impossible!"),
            (None, None) => Ok(None),
        }
    }

    fn visit_unary_expr(&mut self, expr: &crate::ast::expr::UnaryExpr) -> Self::ExprRes {
        let res = self.visit_expr(&expr.1)?;
        match res {
            Some(ExprResult { type_id, category }) => {
                debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                no_assignee!(category);
                let ty = self.get_type_by_id(type_id);
                match expr.0 {
                    UnOp::Deref => {
                        // 在 rust 中，貌似任意 value expr 都可以取其 ref，并且再解引用后可以得到 place value
                        if let ResolvedTy::Ref(t, multb) = ty {
                            Ok(Some(ExprResult {
                                category: ExprCategory::Place(multb),
                                type_id: self.intern_type(t.as_ref().clone()),
                            }))
                        } else {
                            Err(SemanticError::UnDereferenceable)
                        }
                    }
                    UnOp::Not => {
                        let ty = ty.try_deref().0;
                        if ty == ResolvedTy::bool() || ty.is_number_type() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ty.clone()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                    UnOp::Neg => {
                        let ty = ty.try_deref().0;
                        if ty.is_signed_number_type() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ty),
                                category: ExprCategory::Not,
                            }))
                        } else if ty == ResolvedTy::integer() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::signed_integer()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                }
            }
            None => Ok(None),
        }
    }

    fn visit_lit_expr(&mut self, expr: &crate::ast::expr::LitExpr) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => Ok(Some(ExprResult {
                type_id: match expr.kind {
                    LitKind::Bool => self.intern_type(ResolvedTy::bool()),
                    LitKind::Char => self.intern_type(ResolvedTy::char()),
                    LitKind::Integer => match &expr.suffix {
                        Some(s) => {
                            if s == "u32" {
                                self.intern_type(ResolvedTy::u32())
                            } else if s == "i32" {
                                self.intern_type(ResolvedTy::i32())
                            } else {
                                return Err(SemanticError::UnknownSuffix);
                            }
                        }
                        None => self.intern_type(ResolvedTy::integer()),
                    },
                    LitKind::Str | LitKind::StrRaw(_) => {
                        if expr.suffix.is_none() {
                            self.intern_type(ResolvedTy::ref_str())
                        } else {
                            return Err(SemanticError::UnknownSuffix);
                        }
                    }
                    _ => return Err(SemanticError::Unimplemented),
                },
                category: ExprCategory::Not,
            })),
        }
    }

    fn visit_cast_expr(&mut self, expr: &crate::ast::expr::CastExpr) -> Self::ExprRes {
        let res = self.visit_expr(&expr.0)?;
        match res {
            Some(ExprResult { type_id, category }) => {
                debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                no_assignee!(category);

                let expr_ty = self.get_type_by_id(type_id);
                let target_ty = self.resolve_ty(&expr.1)?;

                if (expr_ty == ResolvedTy::i32()
                    || expr_ty == ResolvedTy::u32()
                    || expr_ty == ResolvedTy::integer()
                    || expr_ty == ResolvedTy::char()
                    || expr_ty == ResolvedTy::bool())
                    && (target_ty == ResolvedTy::i32() || target_ty == ResolvedTy::u32())
                {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(target_ty),
                        category: ExprCategory::Not,
                    }))
                } else {
                    Err(SemanticError::IncompatibleCast)
                }
            }
            None => Ok(None),
        }
    }

    fn visit_let_expr(&mut self, _: &crate::ast::expr::LetExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_if_expr(&mut self, IfExpr(condition, take, els): &IfExpr) -> Self::ExprRes {
        let con_res = self.visit_expr(&condition)?;
        let take_res = self.visit_block_expr(&take)?;
        let els_res = match els {
            Some(els) => self.visit_expr(els)?,
            None => None,
        };

        match (con_res, take_res) {
            (Some(con_res), Some(take_res)) => {
                no_assignee!(con_res.category);
                no_assignee!(take_res.category);

                let con_ty = self.get_type_by_id(con_res.type_id);
                if con_ty != ResolvedTy::bool() {
                    return Err(SemanticError::TypeMismatch);
                }

                if let Some(els_res) = els_res {
                    no_assignee!(els_res.category);
                    Ok(Some(ExprResult {
                        type_id: self.utilize_ty(vec![take_res.type_id, els_res.type_id])?,
                        category: ExprCategory::Not,
                    }))
                } else {
                    if take_res.type_id == Self::unit_type() {
                        Ok(Some(Self::unit_expr_result()))
                    } else {
                        Err(SemanticError::TypeMismatch)
                    }
                }
            }
            (None, None) => Ok(None),
            _ => panic!("Impossible"),
        }
    }

    fn visit_while_expr(&mut self, WhileExpr(condition, body): &WhileExpr) -> Self::ExprRes {
        let con_res = self.visit_expr(&condition)?;
        let body_res = self.visit_block_expr_with_kind(&body, ScopeKind::CycleExceptLoop)?;

        match (con_res, body_res) {
            (Some(con_res), Some(body_res)) => {
                no_assignee!(con_res.category);
                no_assignee!(body_res.category);

                let con_ty = self.get_type_by_id(con_res.type_id);
                if con_ty != ResolvedTy::bool() {
                    return Err(SemanticError::TypeMismatch);
                }

                if body_res.type_id == Self::unit_type() {
                    Ok(Some(Self::unit_expr_result()))
                } else {
                    Err(SemanticError::TypeMismatch)
                }
            }
            (None, None) => Ok(None),
            _ => panic!("Impossible"),
        }
    }

    fn visit_for_loop_expr(&mut self, _: &crate::ast::expr::ForLoopExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_loop_expr(&mut self, LoopExpr(block): &LoopExpr) -> Self::ExprRes {
        self.visit_block_expr_with_kind(&block, ScopeKind::Loop { ret_ty: None })
    }

    fn visit_match_expr(&mut self, _: &crate::ast::expr::MatchExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_block_expr(&mut self, expr: &crate::ast::expr::BlockExpr) -> Self::ExprRes {
        self.visit_block_expr_with_kind(expr, ScopeKind::Lambda)
    }

    // TODO: struct A; A = A;
    fn visit_assign_expr(&mut self, AssignExpr(left, right): &AssignExpr) -> Self::ExprRes {
        let right_res = self.visit_expr(&right)?;
        let left_res = self.visit_expr(&left)?;

        match (left_res, right_res) {
            (None, None) => Ok(None),
            (None, Some(_)) | (Some(_), None) => panic!("Impossible"),
            (Some(left_res), Some(right_res)) => {
                match left_res.category {
                    ExprCategory::Place(Mutability::Not) => {
                        return Err(SemanticError::ImmutableVar);
                    }
                    ExprCategory::Not => return Err(SemanticError::NonAssigneeExpr),
                    ExprCategory::Only | ExprCategory::Place(Mutability::Mut) => {}
                }
                no_assignee!(right_res.category);

                let left_ty = self.get_type_by_id(left_res.type_id);
                let right_ty = self.get_type_by_id(right_res.type_id);
                if !right_ty.can_trans_to_target_type(&left_ty) {
                    return Err(SemanticError::TypeMismatch);
                }

                Ok(Some(Self::unit_expr_result()))
            }
        }
    }

    fn visit_assign_op_expr(
        &mut self,
        AssignOpExpr(op, left, right): &AssignOpExpr,
    ) -> Self::ExprRes {
        let right_res = self.visit_expr(&right)?;
        let left_res = self.visit_expr(&left)?;

        match (left_res, right_res) {
            (None, None) => Ok(None),
            (None, Some(_)) | (Some(_), None) => panic!("Impossible"),
            (Some(left_res), Some(right_res)) => {
                match left_res.category {
                    ExprCategory::Place(Mutability::Not) => {
                        return Err(SemanticError::ImmutableVar);
                    }
                    ExprCategory::Only | ExprCategory::Not => {
                        return Err(SemanticError::NonPlaceExpr);
                    }
                    ExprCategory::Place(Mutability::Mut) => {}
                }
                no_assignee!(right_res.category);

                let left_ty = self.get_type_by_id(left_res.type_id);
                let right_ty = self.get_type_by_id(right_res.type_id);

                // 从 visit_binary_expr 复制
                macro_rules! general_bin_op {
                    ($op_exp:expr, $ty1:expr, $ty2:expr, $op:pat, ($($ty:expr), *)) => {
                        matches!($op_exp, $op) && ($(($ty1 == $ty && $ty2.can_trans_to_target_type(&$ty1)))||*)
                    };
                }

                macro_rules! shift_bin_op {
                    ($op_exp:expr, $ty1:expr, $ty2:expr, $op:pat, ($($ty:expr), *)) => {
                        matches!($op_exp, $op) && $ty2.is_number_type() && ($($ty1 == $ty)||*)
                    };
                }

                if ((matches!(op, AssignOp::AddAssign))
                    && left_ty == ResolvedTy::string()
                    && right_ty == ResolvedTy::ref_str())
                    || general_bin_op!(
                        op,
                        left_ty,
                        right_ty,
                        AssignOp::AddAssign
                            | AssignOp::SubAssign
                            | AssignOp::MulAssign
                            | AssignOp::DivAssign
                            | AssignOp::RemAssign,
                        (
                            ResolvedTy::integer(),
                            ResolvedTy::signed_integer(),
                            ResolvedTy::i32(),
                            ResolvedTy::u32(),
                            ResolvedTy::isize(),
                            ResolvedTy::usize()
                        )
                    )
                    || general_bin_op!(
                        op,
                        left_ty,
                        right_ty,
                        AssignOp::BitXorAssign | AssignOp::BitAndAssign | AssignOp::BitOrAssign,
                        (
                            ResolvedTy::integer(),
                            ResolvedTy::signed_integer(),
                            ResolvedTy::i32(),
                            ResolvedTy::u32(),
                            ResolvedTy::isize(),
                            ResolvedTy::usize(),
                            ResolvedTy::bool()
                        )
                    )
                    || shift_bin_op!(
                        op,
                        left_ty,
                        right_ty,
                        AssignOp::ShlAssign | AssignOp::ShrAssign,
                        (
                            ResolvedTy::integer(),
                            ResolvedTy::signed_integer(),
                            ResolvedTy::i32(),
                            ResolvedTy::u32(),
                            ResolvedTy::isize(),
                            ResolvedTy::usize()
                        )
                    )
                {
                    Ok(Some(Self::unit_expr_result()))
                } else {
                    Err(SemanticError::Unimplemented)
                }
            }
        }
    }

    fn visit_field_expr(
        &mut self,
        FieldExpr(expr, ident): &crate::ast::expr::FieldExpr,
    ) -> Self::ExprRes {
        let res = self.visit_expr(expr)?;

        match res {
            Some(res) => {
                let res_mut = get_mutbl!(res.category);
                let (field, deref_level) = self.get_type_fields(res.type_id, &ident.symbol)?;

                Ok(Some(ExprResult {
                    type_id: field.ty,
                    category: ExprCategory::Place(match deref_level {
                        DerefLevel::Not => res_mut,
                        DerefLevel::Deref(mutability) => mutability, // 当发生解引用时，可变性就只和引用的可变性有关
                    }),
                }))
            }
            None => Ok(None),
        }
    }

    fn visit_index_expr(&mut self, expr: &crate::ast::expr::IndexExpr) -> Self::ExprRes {
        let res1 = self.visit_expr(&expr.0)?;
        let res2 = self.visit_expr(&expr.1)?;

        match (res1, res2) {
            (Some(res1), Some(res2)) => {
                let res1_mut = match res1.category {
                    ExprCategory::Place(mutability) => mutability,
                    ExprCategory::Not => Mutability::Mut,
                    ExprCategory::Only => return Err(SemanticError::AssigneeOnlyExpr),
                };
                no_assignee!(res2.category);
                let (ty1, level) = self.get_type_by_id(res1.type_id).deref_all();
                let ty2 = self.get_type_by_id(res2.type_id);

                if !ty2.can_trans_to_target_type(&ResolvedTy::usize()) {
                    return Err(SemanticError::TypeMismatch);
                }

                if let ResolvedTy::Array(ele_ty, _) | ResolvedTy::Slice(ele_ty) = ty1 {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ele_ty.as_ref().clone()),
                        category: ExprCategory::Place(match level {
                            DerefLevel::Not => res1_mut,
                            DerefLevel::Deref(mutability) => mutability,
                        }),
                    }))
                } else {
                    Err(SemanticError::TypeMismatch)
                }
            }
            (None, None) => Ok(None),
            (None, Some(_)) | (Some(_), None) => panic!("Impossible"),
        }
    }

    fn visit_range_expr(&mut self, _: &crate::ast::expr::RangeExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_underscore_expr(&mut self, _: &crate::ast::expr::UnderscoreExpr) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => Ok(Some(ExprResult {
                type_id: self.intern_type(ResolvedTy::Infer),
                category: ExprCategory::Only,
            })),
        }
    }

    fn visit_path_expr(&mut self, PathExpr(qself, path): &PathExpr) -> Self::ExprRes {
        if qself.is_some() {
            return Err(SemanticError::Unimplemented);
        }

        if matches!(self.stage, AnalyzeStage::Body) {
            // 在没有 module 时，path expr 应该只可能为 value 或 ty::value 格式
            match &path.segments[..] {
                [value_seg] => {
                    let (_scope_id, var) = self.search_value(&value_seg.ident.symbol)?;

                    Ok(Some(ExprResult {
                        type_id: var.ty,
                        category: ExprCategory::Place(var.mutbl),
                    }))
                }
                [type_seg, value_seg] => {
                    let ty = self.resolve_ty(&Ty {
                        kind: TyKind::Path(PathTy(
                            None,
                            Path {
                                segments: vec![PathSegment {
                                    ident: type_seg.ident.clone(),
                                    args: None,
                                }],
                                span: path.span.clone(),
                            },
                        )),
                        id: 0,
                        span: path.span.clone(),
                    })?;
                    let type_id = self.intern_type(ty);
                    let value =
                        self.get_type_items_noderef(type_id, false, &value_seg.ident.symbol)?;

                    if let Some(value) = value {
                        Ok(Some(ExprResult {
                            type_id: value,
                            category: ExprCategory::Not,
                        }))
                    } else {
                        Err(SemanticError::UnknownVariable)
                    }
                }
                _ => return Err(SemanticError::InvaildPath),
            }
        } else {
            Ok(None)
        }
    }

    fn visit_addr_of_expr(&mut self, expr: &crate::ast::expr::AddrOfExpr) -> Self::ExprRes {
        match self.visit_expr(&expr.1)? {
            Some(ExprResult { type_id, category }) => {
                no_assignee!(category);
                let ty = self.get_type_by_id(type_id);
                let ret_ty = ResolvedTy::Ref(Box::new(ty.clone()), expr.0);
                Ok(Some(ExprResult {
                    type_id: self.intern_type(ret_ty),
                    category: ExprCategory::Not,
                }))
            }
            None => Ok(None),
        }
    }

    fn visit_break_expr(&mut self, expr: &crate::ast::expr::BreakExpr) -> Self::ExprRes {
        let res = if let Some(expr) = &expr.0 {
            self.visit_expr(expr)?
        } else {
            None
        };

        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => {
                debug_assert!(matches!(res, None));
                Ok(None)
            }
            AnalyzeStage::Body => {
                let target_scope = self.get_cycle_scope_mut()?;

                match &mut target_scope.kind {
                    ScopeKind::Loop { ret_ty } => match (res, &ret_ty) {
                        (None, None) => {}
                        (None, Some(exist)) => {
                            if *exist != Self::unit_type() {
                                return Err(SemanticError::TypeMismatch);
                            }
                        }
                        (Some(new), None) => {
                            no_assignee!(new.category);
                            *ret_ty = Some(new.type_id)
                        }
                        (Some(new), Some(exist)) => {
                            no_assignee!(new.category);
                            if new.type_id != *exist {
                                return Err(SemanticError::TypeMismatch);
                            }
                        }
                    },
                    ScopeKind::CycleExceptLoop => {
                        if expr.0.is_some() {
                            return Err(SemanticError::BreakWithValue);
                        }
                    }
                    _ => panic!("Impossible"),
                }

                Ok(Some(Self::unit_expr_result())) // 没有 Never Type，暂时使用 Unit Type 代替
            }
        }
    }

    fn visit_continue_expr(&mut self, _: &crate::ast::expr::ContinueExpr) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => {
                self.get_cycle_scope_mut()?;
                Ok(Some(Self::unit_expr_result()))
            }
        }
    }

    fn visit_ret_expr(&mut self, RetExpr(expr): &RetExpr) -> Self::ExprRes {
        let res = if let Some(expr) = expr {
            self.visit_expr(expr)?
        } else {
            match self.stage {
                AnalyzeStage::Body => Some(Self::unit_expr_result()),
                _ => None,
            }
        };

        match res {
            Some(res) => {
                no_assignee!(res.category);
                let scope = self.get_fn_scope()?;
                let target_id = scope.kind.as_fn().unwrap();

                let ret_ty = self.get_type_by_id(res.type_id);
                let target_ty = self.get_type_by_id(*target_id);

                if !ret_ty.can_trans_to_target_type(&target_ty) {
                    Err(SemanticError::TypeMismatch)
                } else {
                    Ok(Some(Self::never_expr_result()))
                }
            }
            None => Ok(None),
        }
    }

    fn visit_struct_expr(
        &mut self,
        StructExpr {
            qself,
            path,
            fields,
            rest,
        }: &StructExpr,
    ) -> Self::ExprRes {
        if !matches!(rest, StructRest::None) {
            return Err(SemanticError::Unimplemented);
        };

        let exp_fields = fields
            .iter()
            .map(|x| -> Result<(Symbol, Option<ExprResult>), SemanticError> {
                Ok((x.ident.symbol.clone(), self.visit_expr(&x.expr)?))
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;

        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => {
                let (id, struct_info) = self.search_type_by_path(qself, path)?;
                match &struct_info.kind {
                    TypeKind::Placeholder => panic!("Impossible"),
                    TypeKind::Struct { fields } => {
                        let exp_fields = exp_fields
                            .into_iter()
                            .map(|(s, e)| match e {
                                Some(e) => Some((s, e)),
                                None => None,
                            })
                            .collect::<Option<Vec<_>>>()
                            .unwrap();

                        let cat = Self::utilize_category(
                            exp_fields.iter().map(|x| x.1.category).collect(),
                        )?;

                        let mut dic: HashMap<Symbol, ExprResult> = HashMap::new();

                        for x in exp_fields.into_iter() {
                            if let Some(_) = dic.insert(x.0, x.1) {
                                return Err(SemanticError::MultiSpecifiedField);
                            }
                        }

                        for (field_ident, field_type_id) in fields {
                            if let Some(res) = dic.get(&field_ident) {
                                let res_ty = self.get_type_by_id(res.type_id);
                                let field_ty = self.get_type_by_id(*field_type_id);
                                if !res_ty.can_trans_to_target_type(&field_ty) {
                                    return Err(SemanticError::TypeMismatch);
                                }
                            }
                        }

                        Ok(Some(ExprResult {
                            type_id: self.intern_type(
                                self.resolve_ty_in_scope_by_symbol(&struct_info.name, id),
                            ),
                            category: cat,
                        }))
                    }

                    TypeKind::Enum { fields: _ }
                    | TypeKind::Trait {
                        methods: _,
                        constants: _,
                    } => Err(SemanticError::NotStructType),
                }
            }
        }
    }

    fn visit_repeat_expr(&mut self, RepeatExpr(expr, const_expr): &RepeatExpr) -> Self::ExprRes {
        let res = self.visit_expr(expr)?;
        let size = self.const_eval(&ResolvedTy::usize(), &const_expr.value)?;

        match res {
            Some(res) => {
                no_assignee!(res.category);

                Ok(Some(ExprResult {
                    type_id: self.intern_type(ResolvedTy::Array(
                        Box::new(self.get_type_by_id(res.type_id)),
                        size.into_u_size().unwrap(),
                    )),
                    category: ExprCategory::Not,
                }))
            }
            None => Ok(None),
        }
    }

    fn visit_wild_pat(
        &mut self,
        _pat: &crate::ast::pat::WildPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Ok(PatResult {
            bindings: Vec::new(),
        })
    }

    fn visit_ident_pat(
        &mut self,
        IdentPat(mode, ident, guarder): &IdentPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        if guarder.is_some() {
            return Err(SemanticError::Unimplemented);
        }

        let (target_ty, mutbl) = match mode.0 {
            crate::ast::ByRef::Yes(mutability) => {
                let t = ResolvedTy::Ref(
                    Box::new(self.get_type_by_id(expected_ty).clone()),
                    mutability,
                );
                (self.intern_type(t), Mutability::Not)
            }
            crate::ast::ByRef::No => (expected_ty, mode.1),
        };

        let bindings = vec![(ident.symbol.clone(), target_ty, mutbl)];

        Ok(PatResult { bindings })
    }

    fn visit_struct_pat(
        &mut self,
        _pat: &crate::ast::pat::StructPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_or_pat(
        &mut self,
        _pat: &crate::ast::pat::OrPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_path_pat(
        &mut self,
        PathPat(qself, path): &PathPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        if let Ok((_, var)) = self.search_value_by_path(qself, path) {
            if var.kind.is_constant() {
                return Err(SemanticError::Unimplemented);
            }
        }

        self.visit_ident_pat(
            &IdentPat(
                BindingMode(crate::ast::ByRef::No, Mutability::Not),
                path.get_ident().clone(),
                None,
            ),
            expected_ty,
        )
    }

    fn visit_tuple_pat(
        &mut self,
        _pat: &crate::ast::pat::TuplePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_ref_pat(&mut self, RefPat(pat, mutbl): &RefPat, expected_ty: TypeId) -> Self::PatRes {
        let expected_ty = self.get_type_by_id(expected_ty);

        match &expected_ty {
            ResolvedTy::Ref(resolved_ty, mutability) => {
                if mutbl == mutability {
                    self.visit_pat(pat, self.intern_type(resolved_ty.as_ref().clone()))
                } else {
                    Err(SemanticError::PatMismatch)
                }
            }
            _ => Err(SemanticError::PatMismatch),
        }
    }

    fn visit_lit_pat(
        &mut self,
        _pat: &crate::ast::pat::LitPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        todo!()
    }

    fn visit_range_pat(
        &mut self,
        _pat: &crate::ast::pat::RangePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_slice_pat(
        &mut self,
        _pat: &crate::ast::pat::SlicePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_rest_pat(
        &mut self,
        _pat: &crate::ast::pat::RestPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }
}
