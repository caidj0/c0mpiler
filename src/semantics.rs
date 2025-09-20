pub mod error;
pub mod primitives;
pub mod resolved_ty;
pub mod super_trait;
pub mod utils;
pub mod visitor;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter, vec,
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
    const_eval::{ConstEvalError, ConstEvalValue, ConstEvaler},
    lexer::TokenPosition,
    semantics::{error::SemanticError, resolved_ty::ResolvedTy, utils::*, visitor::Visitor},
};

macro_rules! no_assignee {
    ($id:expr) => {
        if matches!($id, ExprCategory::Only) {
            return Err(SemanticError::AssigneeOnlyExpr);
        }
    };
}

macro_rules! check_sized {
    ($analyzer:expr, $id:expr) => {
        if !$analyzer.has_sized_trait($id) {
            return Err(SemanticError::NotSizedType);
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
    pub(crate) state: AnalyzerState,

    builtin_impls: BuiltInImpls,
}

type AssocItemRes = Result<(HashMap<Symbol, FnSig>, HashMap<Symbol, Constant>), SemanticError>;

impl SemanticAnalyzer {
    pub fn visit(krate: &crate::ast::Crate) -> (Self, Result<(), SemanticError>) {
        let mut analyzer = Self::new();
        let result = || -> Result<(), SemanticError> {
            for stage in [
                AnalyzeStage::SymbolCollect,
                AnalyzeStage::Definition,
                AnalyzeStage::Impl,
                AnalyzeStage::Body,
            ] {
                analyzer.stage = stage;
                analyzer.visit_crate(krate)?;
            }
            Ok(())
        }();
        (analyzer, result)
    }

    fn new() -> Self {
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
                        Box::new(ResolvedTy::Never),
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

        let builtin_impls = BuiltInImpls::new(&mut type_table);

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

    fn get_scope_by_id(&self, id: NodeId) -> &Scope {
        self.scopes.get(&id).unwrap()
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
                | ScopeKind::Fn { .. } => return Err(SemanticError::NoLoopScope),
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
                ScopeKind::Fn { .. } => return Ok(scope),
            }
            id = scope.father;
        }
    }

    fn get_fn_scope_mut(&mut self) -> Result<&mut Scope, SemanticError> {
        let mut id = self.current_scope;

        loop {
            let scope = self.scopes.get(&id).unwrap();
            match scope.kind {
                ScopeKind::Lambda | ScopeKind::Loop { ret_ty: _ } | ScopeKind::CycleExceptLoop => {}
                ScopeKind::Root | ScopeKind::Crate | ScopeKind::Trait(_) | ScopeKind::Impl(_) => {
                    return Err(SemanticError::NoFnScope);
                }
                ScopeKind::Fn { .. } => return Ok(self.scopes.get_mut(&id).unwrap()),
            }
            id = scope.father;
        }
    }

    fn add_scope(&mut self, kind: ScopeKind) -> Result<(), SemanticError> {
        debug_assert!(matches!(self.stage, AnalyzeStage::SymbolCollect));

        let id = self.state.current_ast_id;

        if self.scopes.contains_key(&id) || self.get_scope().children.contains(&id) {
            return Err(SemanticError::InvalidScope);
        }

        self.get_scope_mut().children.insert(id);
        self.scopes.insert(
            id,
            Scope {
                id,
                kind,
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
            return Err(SemanticError::InvalidScope);
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

                if matches!(scope.kind, ScopeKind::Fn { .. }) {
                    include_local &= false;
                }
            }
        }
    }

    fn search_value_from_mut(
        &mut self,
        ident: &Symbol,
        mut id: NodeId,
        mut include_local: bool,
    ) -> Result<(NodeId, &mut Variable), SemanticError> {
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
                return Ok((
                    id,
                    self.scopes
                        .get_mut(&id)
                        .unwrap()
                        .values
                        .get_mut(ident)
                        .unwrap(),
                ));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownVariable);
            } else {
                id = scope.father;

                if matches!(scope.kind, ScopeKind::Fn { .. }) {
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
                let de_refed_id = self.intern_type(resolved_ty.as_ref().clone());

                let (var, deref_level) = self.get_type_fields(de_refed_id, symbol)?;

                Ok((var, deref_level.merge(DerefLevel::Deref(*mutability))))
            }
            ResolvedTy::ImplicitSelf => self.get_type_fields(self.get_self_type()?, symbol),
            _ => Err(SemanticError::NonProvidedField),
        }
    }

    fn get_type_items_no_deref(
        &self,
        id: TypeId,
        is_methods_call: bool,
        name: &Symbol,
    ) -> Result<Option<(TypeId, ImplInfoItem<'_>)>, SemanticError> {
        fn get_impl_item_type_id(
            analyzer: &SemanticAnalyzer,
            item: &ImplInfoItem,
            is_methods_call: bool,
            self_ty: &ResolvedTy,
        ) -> Option<TypeId> {
            match &item {
                ImplInfoItem::Method(fn_sig) => {
                    let fn_ty = analyzer.get_type_by_id(fn_sig.type_id);

                    match (is_methods_call, fn_ty.is_method()) {
                        (true, true) => Some(fn_sig.type_id),
                        (true, false) => None,
                        (false, true) => Some(analyzer.intern_type(fn_ty.method_to_func(self_ty))),
                        (false, false) => Some(fn_sig.type_id),
                    }
                }
                ImplInfoItem::Constant(constant) => {
                    if !is_methods_call {
                        Some(constant.ty)
                    } else {
                        None
                    }
                }
            }
        }

        let ty = self.get_type_by_id(id);

        // builtin 在 get_impl 中返回，从而此处不需要特殊处理
        // visit_impl_item 时保证了 implInfo 中的 fn 只有第一位可能出现 Self 类型，其他位置的 Self 都被展开了
        if let Some((inherent_impl, trait_impls)) = self.get_impl(&id) {
            if let Some(item) = inherent_impl.get(name) {
                let replacer = get_impl_item_type_id(self, &item, is_methods_call, &ty);
                if let Some(replacer) = replacer {
                    return Ok(Some((replacer, item)));
                }
            };

            let mut candidate = None;

            for trait_impl in trait_impls.values() {
                if let Some(item) = trait_impl.get(name) {
                    let replacer = get_impl_item_type_id(self, &item, is_methods_call, &ty);
                    if let Some(replacer) = replacer
                        && candidate.replace((replacer, item)).is_some()
                    {
                        return Err(SemanticError::MultipleApplicable);
                    }
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
            if let Some(ret) = self.get_type_items_no_deref(id, is_methods_call, &name)? {
                return Ok((ret.0, deref_level));
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

    pub(crate) fn search_type_by_path(
        &self,
        qself: &Option<Box<QSelf>>,
        path: &Path,
    ) -> Result<(NodeId, &TypeInfo), SemanticError> {
        // TODO: 将 resolve_ty 统一过来，实现 Self::Ty 的结构

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

    pub(crate) fn search_value_by_path(
        &mut self,
        qself: &Option<Box<QSelf>>,
        path: &Path,
    ) -> Result<ValueContainer<'_>, SemanticError> {
        if qself.is_some() {
            return Err(SemanticError::Unimplemented);
        }

        // 在没有 module 时，path expr 应该只可能为 value 或 ty::value 格式 (错误)
        // TODO: ty::ty::...::ty::value
        match &path.segments[..] {
            [value_seg] => self
                .search_value(&value_seg.ident.symbol)
                .map(ValueContainer::Variable),
            [type_seg, value_seg] => {
                let ty = self.resolve_ty(&Ty {
                    kind: TyKind::Path(PathTy(
                        None,
                        Path {
                            segments: vec![PathSegment {
                                ident: type_seg.ident.clone(),
                                args: None,
                            }],
                            span: path.span,
                        },
                    )),
                    id: 0,
                    span: path.span,
                })?;

                let type_id = self.intern_type(ty.clone());

                // 枚举Type::Variant，这个不能简单的直接在 inherit impl 中定义几个常量
                if let ResolvedTy::Named(fullname) = ty {
                    let info = self.get_type_info(&fullname);
                    if let TypeKind::Enum { fields } = &info.kind
                        && fields.contains(&value_seg.ident.symbol)
                    {
                        return Ok(ValueContainer::Temp(Variable {
                            ty: type_id,
                            mutbl: Mutability::Not,
                            kind: VariableKind::Constant(ConstEvalValue::Enum(
                                fullname.clone(),
                                value_seg.ident.symbol.clone(),
                            )),
                        }));
                    }
                }

                self.get_type_items_no_deref(type_id, false, &value_seg.ident.symbol)?
                    .map(|(id, v)| ValueContainer::ImplInfoItem(id, v))
                    .ok_or(SemanticError::UnknownVariable)
            }
            _ => Err(SemanticError::InvalidPath),
        }
    }

    fn search_type(&self, ident: &Symbol) -> Result<(NodeId, &TypeInfo), SemanticError> {
        self.search_type_from(ident, self.current_scope)
    }

    // 返回值为 (Scope Id, 变量类型)
    fn search_value(&self, ident: &Symbol) -> Result<&Variable, SemanticError> {
        self.search_value_from(ident, self.current_scope, true)
            .map(|x| x.1)
    }

    pub(crate) fn search_value_mut(
        &mut self,
        ident: &Symbol,
    ) -> Result<&mut Variable, SemanticError> {
        self.search_value_from_mut(ident, self.current_scope, true)
            .map(|x| x.1)
    }

    fn get_type_info(&self, full_name: &FullName) -> &TypeInfo {
        let symbols = &full_name.0;
        debug_assert!(symbols.len() >= 2);
        let scope_symbol = &symbols[symbols.len() - 2];
        let scope_id: NodeId = scope_symbol.0.strip_prefix("$").unwrap().parse().unwrap();
        let scope = self.scopes.get(&scope_id).unwrap();
        scope.types.get(symbols.last().unwrap()).unwrap()
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

    pub(crate) fn get_full_name_from(&self, id: NodeId, s: Symbol) -> FullName {
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
        &mut self,
        ty: &Ty,
        expand_self: bool,
    ) -> Result<ResolvedTy, SemanticError> {
        match &ty.kind {
            TyKind::Slice(slice_ty) => Ok(ResolvedTy::Slice(Box::new(
                self.resolve_ty_self_kind_specified(&slice_ty.0, expand_self)?,
            ))),
            TyKind::Array(array_ty) => Ok(ResolvedTy::Array(
                Box::new({
                    let inner = self.resolve_ty_self_kind_specified(&array_ty.0, expand_self)?;
                    check_sized!(self, &inner);
                    inner
                }),
                self.const_eval(ResolvedTy::usize(), &array_ty.1.value)?
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
                    return Err(SemanticError::InvalidPath);
                }

                let seg = path.segments.first().unwrap();
                let s = &seg.ident.symbol;
                if s.is_path_segment() {
                    if s.is_big_self() {
                        if seg.args.is_some() {
                            return Err(SemanticError::InvalidPath);
                        }
                        return Ok(if expand_self {
                            self.get_type_by_id(self.get_self_type()?).clone()
                        } else {
                            ResolvedTy::big_self()
                        });
                    }
                    return Err(SemanticError::InvalidPath);
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
                            Ok(ResolvedTy::BuiltIn(s.clone(), args_v))
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
    pub(crate) fn resolve_ty(&mut self, ty: &Ty) -> Result<ResolvedTy, SemanticError> {
        self.resolve_ty_self_kind_specified(ty, true)
    }

    fn resolve_ty_in_scope_by_symbol(&self, s: &Symbol, id: usize) -> ResolvedTy {
        ResolvedTy::Named(self.get_full_name_from(id, s.clone()))
    }

    pub(crate) fn intern_type(&self, ty: ResolvedTy) -> TypeId {
        self.type_table.borrow_mut().intern(ty)
    }

    pub(crate) fn get_type_by_id(&self, id: TypeId) -> ResolvedTy {
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

    fn unit_expr_result_int_specified(int_flow: InterruptControlFlow) -> ExprResult {
        ExprResult {
            type_id: Self::unit_type(),
            category: ExprCategory::Not,
            int_flow,
        }
    }

    fn unit_expr_result() -> ExprResult {
        Self::unit_expr_result_int_specified(InterruptControlFlow::Not)
    }

    // TODO: 将 block expr 在某些情形下的返回值都修改为 never
    fn never_expr_result_int_specified(int_flow: InterruptControlFlow) -> ExprResult {
        ExprResult {
            type_id: Self::never_type(),
            category: ExprCategory::Not,
            int_flow,
        }
    }

    pub(crate) fn const_eval(
        &mut self,
        ty: ResolvedTy,
        expr: &Expr,
    ) -> Result<ConstEvalValue, SemanticError> {
        match ConstEvaler::eval(self, expr) {
            Ok(x) => x.cast(&ty),
            Err(err) => Err(err),
        }
        .map_err(|x| match x {
            ConstEvalError::Semantic(s) => *s,
            _ => SemanticError::ConstEvalError(x),
        })
    }

    // 尝试统一传入的 type （主要是为了 integer）
    fn utilize_ty(&self, types: Vec<TypeId>) -> Result<TypeId, SemanticError> {
        let types: Vec<ResolvedTy> = types
            .into_iter()
            .filter(|x| *x != Self::infer_type())
            .map(|x| self.get_type_by_id(x))
            .collect();

        Ok(self.intern_type(ResolvedTy::utilize(types).ok_or(SemanticError::TypeMismatch)?))
    }

    fn utilize_category(cats: Vec<ExprCategory>) -> Result<ExprCategory, SemanticError> {
        let has_only = cats.iter().any(|x| matches!(x, ExprCategory::Only));
        let has_value = cats.iter().any(|x| matches!(x, ExprCategory::Not));
        let has_const = cats
            .iter()
            .any(|x| matches!(x, ExprCategory::Place(Mutability::Not)));

        match (has_only, has_value, has_const) {
            (true, true, _) => Err(SemanticError::ConflictAssignee),
            (true, false, true) => Err(SemanticError::ImmutableVar),
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
    ) -> Result<Option<StmtResult>, SemanticError> {
        let old_state = self.state;
        self.state = AnalyzerState {
            current_ast_id: expr.id,
            current_span: expr.span,
        };

        if let AnalyzeStage::SymbolCollect = self.stage {
            self.add_scope(kind.clone())?;
        };

        let mut ret: Option<StmtResult> = match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => None,
            AnalyzeStage::Body => Some(StmtResult::Else {
                int_flow: InterruptControlFlow::Not,
            }),
        };

        self.enter_scope()?;
        for (index, stmt) in expr.stmts.iter().enumerate() {
            if let Some(new_ret) = self.visit_stmt(stmt)?
                && !matches!(stmt.kind, StmtKind::Item(_))
            {
                let mut_ret = ret.as_mut().unwrap();
                let old_int_flow = match mut_ret {
                    StmtResult::Expr(expr_result) => expr_result.int_flow,
                    StmtResult::Else { int_flow } => *int_flow,
                };
                match new_ret {
                    StmtResult::Expr(expr_result) => {
                        if index + 1 != expr.stmts.len() {
                            return Err(SemanticError::TypeMismatch);
                        }

                        *mut_ret = StmtResult::Expr(ExprResult {
                            int_flow: old_int_flow.concat(expr_result.int_flow),
                            ..expr_result
                        })
                    }
                    StmtResult::Else { int_flow } => {
                        *mut_ret = StmtResult::Else {
                            int_flow: old_int_flow.concat(int_flow),
                        }
                    }
                }
            }
        }

        self.exit_scope()?;

        self.state = old_state;
        Ok(ret)
    }

    fn is_free_scope(&self) -> bool {
        !matches!(
            self.get_scope().kind,
            ScopeKind::Trait(_) | ScopeKind::Impl(_)
        )
    }

    // 这个函数应只在 Definition 阶段调用
    fn resolve_assoc_items(
        &mut self,
        items: &Vec<Item<AssocItemKind>>,
        is_trait_item: bool,
    ) -> AssocItemRes {
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
                    let resolved_ty = self.resolve_ty_self_kind_specified(ty, expand_self)?;
                    // TODO: 在此时 eval 不合适
                    let value = match expr {
                        Some(e) => self.const_eval(resolved_ty.clone(), e)?,
                        None => {
                            if is_trait_item {
                                ConstEvalValue::Placeholder
                            } else {
                                return Err(SemanticError::ConstantWithoutBody);
                            }
                        }
                    };
                    let ty_id = self.intern_type(resolved_ty);

                    if methods.contains_key(&ident.symbol) || constants.contains_key(&ident.symbol)
                    {
                        return Err(SemanticError::MultiDefined);
                    }

                    constants.insert(ident.symbol.clone(), Constant { ty: ty_id, value });
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
                        FnRetTy::Ty(ty) => self.resolve_ty_self_kind_specified(ty, expand_self)?,
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

            if ty == ResolvedTy::u32() || ty == ResolvedTy::i32() || ty == ResolvedTy::integer() {
                Some(&self.builtin_impls.u32_and_usize_and_integer)
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
        if !self.impls.contains_key(id) {
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

        self.impls.get_mut(id).unwrap()
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
                if let Ok(var) = self.search_value(&symbol)
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

impl<'ast> Visitor<'ast> for SemanticAnalyzer {
    type DefaultRes = Result<(), SemanticError>;
    type ExprRes = Result<Option<ExprResult>, SemanticError>;
    type PatRes = Result<PatResult, SemanticError>;
    type StmtRes = Result<Option<StmtResult>, SemanticError>;

    fn visit_crate(&mut self, krate: &'ast Crate) -> Result<(), SemanticError> {
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

    fn visit_item(&mut self, item: &'ast Item) -> Result<(), SemanticError> {
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

    fn visit_associate_item(
        &mut self,
        item: &'ast Item<AssocItemKind>,
    ) -> Result<(), SemanticError> {
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

    fn visit_const_item(&mut self, item: &'ast ConstItem) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                if self.is_free_scope() {
                    if item.expr.is_none() {
                        return Err(SemanticError::ConstantWithoutBody);
                    }

                    self.add_value(
                        item.ident.symbol.clone(),
                        Variable {
                            ty: TypeId(0),
                            mutbl: Mutability::Not,
                            kind: VariableKind::Constant(ConstEvalValue::UnEvaled(
                                self.state.current_ast_id,
                                self.state.current_span,
                                &raw const *item,
                            )),
                        },
                        false,
                    )?;
                }
            }
            AnalyzeStage::Definition => {}
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {
                if self.is_free_scope() {
                    let var = self.search_value_mut(&item.ident.symbol).unwrap();

                    if var.kind.as_constant().unwrap().is_un_evaled() {
                        let ty = self.resolve_ty(&item.ty)?;
                        check_sized!(self, &ty);
                        let value = self.const_eval(ty.clone(), item.expr.as_ref().unwrap())?;
                        let ty_id = self.intern_type(ty);

                        let var = self.search_value_mut(&item.ident.symbol).unwrap();
                        *var = Variable {
                            ty: ty_id,
                            mutbl: Mutability::Not,
                            kind: VariableKind::Constant(value),
                        }
                    }
                }
            }
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
        }: &'ast FnItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let is_main_fn =
                    ident.symbol.0.as_str() == "main" && self.get_scope().kind.is_crate(); // 现在只允许存在一个 Crate，因此满足这个条件的就是 main fn
                self.add_scope(ScopeKind::Fn {
                    ret_ty: TypeId(0),
                    main_fn: if is_main_fn {
                        MainFunctionState::UnExited
                    } else {
                        MainFunctionState::Not
                    },
                })?;
            }
            AnalyzeStage::Definition => {
                let mut ret_ty = match &sig.decl.output {
                    FnRetTy::Default => ResolvedTy::Tup(Vec::new()),
                    FnRetTy::Ty(ty) => self.resolve_ty(ty)?,
                };

                let mut param_tys = sig
                    .decl
                    .inputs
                    .iter()
                    .map(|x| self.resolve_ty(&x.ty))
                    .collect::<Result<Vec<_>, SemanticError>>()?;

                for x in param_tys.iter().chain(iter::once(&ret_ty)) {
                    check_sized!(self, x);
                }

                let is_free_scope = if let ScopeKind::Trait(self_ty) | ScopeKind::Impl(self_ty) =
                    self.get_scope().kind
                {
                    let self_ty = &self.get_type_by_id(self_ty);
                    ret_ty = ret_ty.expand_self(self_ty);
                    param_tys = param_tys
                        .into_iter()
                        .map(|x| x.expand_self(self_ty))
                        .collect();
                    false
                } else {
                    if ret_ty.is_implicit_self_or_ref_implicit_self()
                        || param_tys
                            .iter()
                            .any(|x| x.is_implicit_self_or_ref_implicit_self())
                    {
                        return Err(SemanticError::SelfInNoAssocFn);
                    }

                    true
                };

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

                if is_free_scope {
                    let ty_id =
                        self.intern_type(ResolvedTy::Fn(param_tys, Box::new(ret_ty.clone())));

                    self.add_value(
                        ident.symbol.clone(),
                        Variable {
                            ty: ty_id,
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
                *self.get_scope_mut().kind.as_fn_mut().unwrap().0 = ret_ty_id;

                self.exit_scope()?;
            }
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {}
        }
        self.enter_scope()?;
        if let Some(b) = body {
            let res = self.visit_block_expr_with_kind(b, ScopeKind::Lambda)?;
            if let Some(res) = res {
                let (target_ty_id, main_fn_state) = self.get_scope().kind.as_fn().unwrap();

                if main_fn_state.is_un_exited() {
                    return Err(SemanticError::MainFunctionNotExited);
                }

                match res {
                    StmtResult::Expr(expr_result) => {
                        let target_ty = self.get_type_by_id(*target_ty_id);
                        let ret_ty = self.get_type_by_id(expr_result.type_id);
                        if !ret_ty.can_trans_to_target_type(&target_ty) {
                            return Err(SemanticError::TypeMismatch);
                        }
                    }
                    StmtResult::Else { int_flow } => match int_flow {
                        InterruptControlFlow::Not => {
                            if *target_ty_id != Self::unit_type() {
                                return Err(SemanticError::NoReturnFunction);
                            }
                        }
                        InterruptControlFlow::Loop => panic!("Impossible!"),
                        InterruptControlFlow::Return => {}
                    },
                }
            }
        }
        self.exit_scope()?;
        Ok(())
    }

    fn visit_mod_item(&mut self, _: &'ast crate::ast::item::ModItem) -> Result<(), SemanticError> {
        Err(SemanticError::Unimplemented)
    }

    fn visit_enum_item(
        &mut self,
        EnumItem(ident, _, variants): &'ast EnumItem,
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
                let fields: HashSet<Symbol> = HashSet::from_iter(fields);
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
        StructItem(ident, _, variant_data): &'ast crate::ast::item::StructItem,
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
                                {
                                    let ty = self.resolve_ty(&x.ty)?;
                                    check_sized!(self, &ty); // 暂时让 struct 的成员均为 sized type
                                    self.intern_type(ty)
                                },
                            ))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?,
                    crate::ast::item::VariantData::Tuple(_) => {
                        return Err(SemanticError::Unimplemented);
                    }
                    crate::ast::item::VariantData::Unit => Vec::new(),
                };

                let vec_len = fields.len();
                let fields: HashMap<Symbol, TypeId> = HashMap::from_iter(fields);
                if vec_len != fields.len() {
                    return Err(SemanticError::MultiDefined);
                }

                let (id, info) = self.search_type_mut(&ident.symbol)?;
                *info.kind.as_struct_mut().unwrap() = fields;

                // unit struct 隐式添加了一个 constant
                if variant_data.is_unit() {
                    let self_ty = self.resolve_ty_in_scope_by_symbol(&ident.symbol, id);
                    let fullname = self_ty.as_named().unwrap().clone();
                    let self_ty_id = self.intern_type(self_ty);
                    self.add_value(
                        ident.symbol.clone(),
                        Variable {
                            ty: self_ty_id,
                            mutbl: Mutability::Not,
                            kind: VariableKind::Constant(ConstEvalValue::UnitStruct(fullname)),
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
        }: &'ast crate::ast::item::TraitItem,
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
        }: &'ast ImplItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                // 0 type 作为 self ty 的占位符
                self.add_scope(ScopeKind::Impl(TypeId(0)))?;
            }
            AnalyzeStage::Definition => {
                let self_ty = self.resolve_ty(self_ty)?;
                let self_ty_id = self.intern_type(self_ty);

                self.enter_scope()?;
                *self.get_scope_mut().kind.as_impl_mut().unwrap() = self_ty_id;
                self.exit_scope()?;
            }
            AnalyzeStage::Impl => {
                self.enter_scope()?;
                let self_ty_id = *self.get_scope().kind.as_impl().unwrap();
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

                        let mut musts = HashSet::new();
                        musts.extend(trait_methods.iter().filter_map(|(symbol, sig)| {
                            if sig.is_placeholder {
                                Some(symbol)
                            } else {
                                None
                            }
                        }));
                        musts.extend(trait_constants.iter().filter_map(|(symbol, constant)| {
                            if matches!(constant.value, ConstEvalValue::Placeholder) {
                                Some(symbol)
                            } else {
                                None
                            }
                        }));

                        for (symbol, sig) in &methods {
                            let Some(trait_sig) = trait_methods.get(symbol).map(|x| FnSig {
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

                            musts.remove(&symbol);
                        }

                        for (symbol, constant) in &constants {
                            let Some(trait_constant) =
                                trait_constants.get(symbol).map(|x| Constant {
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

                            musts.remove(&symbol);
                        }

                        if !musts.is_empty() {
                            return Err(SemanticError::NotAllTraitItemsImplemented);
                        }

                        let (_, trait_impls) = self.get_impl_mut(&self_ty_id);

                        if trait_impls.contains_key(&full_name) {
                            return Err(SemanticError::MultiImplemented);
                        }

                        trait_impls.insert(full_name, ImplInfo { methods, constants });
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

    // 确保了返回值不为 AssigneeOnly
    fn visit_stmt(&mut self, stmt: &'ast crate::ast::stmt::Stmt) -> Self::StmtRes {
        let old_state = self.state;
        self.state.current_span = stmt.span;

        let default_ret = match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => None,
            AnalyzeStage::Body => Some(StmtResult::Else {
                int_flow: InterruptControlFlow::Not,
            }),
        };

        let ret = match &stmt.kind {
            StmtKind::Let(local_stmt) => self.visit_let_stmt(local_stmt)?,
            StmtKind::Item(item) => {
                self.visit_item(item)?;
                default_ret
            }
            StmtKind::Expr(expr) => match self.visit_expr(expr)? {
                Some(res) => {
                    no_assignee!(res.category);
                    Some(
                        if expr.is_block()
                            && (res.type_id == Self::unit_type()
                                || res.type_id == Self::never_type())
                        {
                            StmtResult::Else {
                                int_flow: res.int_flow,
                            }
                        } else {
                            StmtResult::Expr(res)
                        },
                    )
                }
                None => None,
            },
            StmtKind::Semi(expr) => match self.visit_expr(expr)? {
                Some(res) => {
                    no_assignee!(res.category);
                    Some(StmtResult::Else {
                        int_flow: res.int_flow,
                    })
                }
                None => None,
            },
            StmtKind::Empty(_) => default_ret,
        };

        self.state = old_state;

        Ok(ret)
    }

    fn visit_let_stmt(&mut self, stmt: &'ast crate::ast::stmt::LocalStmt) -> Self::StmtRes {
        let Some(ty) = &stmt.ty else {
            return Err(SemanticError::Unimplemented);
        };

        let expr_res = if let LocalKind::Init(expr) = &stmt.kind {
            self.visit_expr(expr)?
        } else {
            None
        };

        if matches!(self.stage, AnalyzeStage::Body) {
            let expected_ty = self.resolve_ty(ty)?;
            check_sized!(self, &expected_ty);
            let expected_ty_id = self.intern_type(expected_ty.clone());
            let pat_res = self.visit_pat(&stmt.pat, expected_ty_id)?;

            let int_flow = match &stmt.kind {
                LocalKind::Decl => {
                    // TODO: 检查变量是否完成初始化，需要进行控制流分析，需要允许回退内层 scope 对外层 TypeTable 的修改
                    InterruptControlFlow::Not
                }
                LocalKind::Init(_) => {
                    let ExprResult {
                        type_id,
                        category,
                        int_flow,
                    } = expr_res.unwrap();

                    no_assignee!(category);
                    let expr_ty = self.get_type_by_id(type_id);
                    if !expr_ty.can_trans_to_target_type(&expected_ty) {
                        return Err(SemanticError::TypeMismatch);
                    }
                    int_flow
                }
            };

            self.add_bindings(vec![pat_res], VariableKind::Inited)?;

            Ok(Some(StmtResult::Else { int_flow }))
        } else {
            Ok(None)
        }
    }

    // 在 body 阶段返回 Ok(Some(...))，其他阶段返回 Ok(None)
    fn visit_expr(&mut self, expr: &'ast Expr) -> Self::ExprRes {
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

        debug_assert!((matches!(self.stage, AnalyzeStage::Body) && res.is_some()) || res.is_none());

        self.state = old_state;
        Ok(res)
    }

    fn visit_array_expr(&mut self, expr: &'ast crate::ast::expr::ArrayExpr) -> Self::ExprRes {
        let expr_res = expr
            .0
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<Option<ExprResult>>, SemanticError>>()?;

        if !matches!(self.stage, AnalyzeStage::Body) {
            debug_assert!(expr_res.iter().all(|x| x.is_none()));
            return Ok(None);
        }

        let (types, (cats, ints)): (Vec<_>, (Vec<_>, Vec<_>)) = expr_res
            .into_iter()
            .map(|x| {
                let ExprResult {
                    type_id,
                    category,
                    int_flow,
                } = x.unwrap();
                (type_id, (category, int_flow))
            })
            .unzip();

        let unified_type_id = self.utilize_ty(types)?;
        let unified_ty = self.get_type_by_id(unified_type_id);
        let cat = Self::utilize_category(cats)?;
        let int_flow = ints
            .iter()
            .fold(InterruptControlFlow::Not, |x, y| x.concat(*y));

        let ret_ty = ResolvedTy::Array(Box::new(unified_ty.clone()), expr.0.len() as u32);

        Ok(Some(ExprResult {
            type_id: self.intern_type(ret_ty),
            category: cat,
            int_flow,
        }))
    }

    fn visit_const_block_expr(
        &mut self,
        _expr: &'ast crate::ast::expr::ConstBlockExpr,
    ) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_call_expr(&mut self, CallExpr(expr, params): &'ast CallExpr) -> Self::ExprRes {
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

                    let mut int_flow = res.int_flow;
                    for (income, target) in params_res.into_iter().zip(required.iter()) {
                        debug_assert!(!target.is_implicit_self_or_ref_implicit_self());
                        let income_ty = self.get_type_by_id(income.type_id);
                        int_flow = int_flow.concat(income.int_flow);

                        if !income_ty.can_trans_to_target_type(target) {
                            return Err(SemanticError::TypeMismatch);
                        }
                    }

                    if let Ok(fn_scope) = self.get_fn_scope_mut() {
                        let (_, main_fn_state) = fn_scope.kind.as_fn_mut().unwrap();
                        match (&main_fn_state, ret_ty.is_never()) {
                            // 只有 Exit 函数返回类型为 Never
                            (MainFunctionState::Not, true) => {
                                return Err(SemanticError::NotMainFunction);
                            }
                            (MainFunctionState::UnExited, true) => {
                                *main_fn_state = MainFunctionState::Exited
                            }
                            (MainFunctionState::Exited, _) => {
                                return Err(SemanticError::ExprAfterExit);
                            }
                            _ => {}
                        }
                    }

                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ret_ty.as_ref().clone()),
                        category: ExprCategory::Not,
                        int_flow,
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
        }: &'ast MethodCallExpr,
    ) -> Self::ExprRes {
        let res = self.visit_expr(receiver)?;
        let params_res = args
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<_>, SemanticError>>()?;

        match res {
            Some(ExprResult {
                type_id,
                category,
                mut int_flow,
            }) => {
                no_assignee!(category);
                let params_res = params_res.into_iter().collect::<Option<Vec<_>>>().unwrap();
                for x in &params_res {
                    no_assignee!(x.category);
                }
                let ident = &seg.ident;
                if seg.args.is_some() {
                    return Err(SemanticError::Unimplemented);
                }

                let (fn_id, deref_level) = self.get_type_items(
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
                        let self_mut = get_mutbl!(category).merge_with_deref_level(deref_level);
                        if !self_mut.can_trans_to(target_mut) {
                            return Err(SemanticError::ImmutableVar);
                        }
                    }
                    ResolvedTy::ImplicitSelf => {
                        if matches!(deref_level, DerefLevel::Deref(_)) {
                            return Err(SemanticError::UnDereferenceable);
                        }
                    }
                    _ => panic!("Impossible"),
                }

                for (income, target) in params_res.iter().zip(required_tys[1..].iter()) {
                    debug_assert!(!target.is_implicit_self_or_ref_implicit_self());
                    let income_ty = self.get_type_by_id(income.type_id);
                    int_flow = int_flow.concat(income.int_flow);

                    if !income_ty.can_trans_to_target_type(target) {
                        return Err(SemanticError::TypeMismatch);
                    }
                }

                Ok(Some(ExprResult {
                    type_id: self.intern_type(ret_ty.as_ref().clone()),
                    category: ExprCategory::Not,
                    int_flow,
                }))
            }
            None => {
                debug_assert!(params_res.iter().all(|x| x.is_none()));
                Ok(None)
            }
        }
    }

    fn visit_tup_expr(&mut self, expr: &'ast crate::ast::expr::TupExpr) -> Self::ExprRes {
        match &expr.0[..] {
            [] => Ok(if matches!(self.stage, AnalyzeStage::Body) {
                Some(ExprResult {
                    type_id: Self::unit_type(),
                    category: ExprCategory::Place(Mutability::Mut),
                    int_flow: InterruptControlFlow::Not,
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
        BinaryExpr(bin_op, expr1, expr2): &'ast BinaryExpr,
    ) -> Self::ExprRes {
        let res1 = self.visit_expr(expr1)?;
        let res2 = self.visit_expr(expr2)?;

        match (res1, res2) {
            (Some(res1), Some(res2)) => {
                debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                no_assignee!(res1.category);
                no_assignee!(res2.category);

                // TODO: &'ast 1 + &2 之类的
                let ty1 = self.get_type_by_id(res1.type_id);
                let ty2 = self.get_type_by_id(res2.type_id);
                let int_flow = res1.int_flow.concat(res2.int_flow);

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

                if ty1 == ty2
                    && let ResolvedTy::Named(fullname) = &ty1
                    && self.get_type_info(fullname).kind.is_enum()
                {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ResolvedTy::bool()),
                        category: ExprCategory::Not,
                        int_flow,
                    }))
                } else if (matches!(bin_op, BinOp::Add))
                    && ty1 == ResolvedTy::string()
                    && ty2 == ResolvedTy::ref_str()
                {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ResolvedTy::string()),
                        category: ExprCategory::Not,
                        int_flow,
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
                        int_flow,
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
                        int_flow,
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
                        int_flow,
                    }))
                } else {
                    Err(SemanticError::NoBinaryOperation(*bin_op, ty1, ty2))
                }
            }
            (None, Some(_)) | (Some(_), None) => panic!("Impossible!"),
            (None, None) => Ok(None),
        }
    }

    fn visit_unary_expr(&mut self, expr: &'ast crate::ast::expr::UnaryExpr) -> Self::ExprRes {
        let res = self.visit_expr(&expr.1)?;
        match res {
            Some(ExprResult {
                type_id,
                category,
                int_flow,
            }) => {
                debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                no_assignee!(category);
                let ty = self.get_type_by_id(type_id);
                match expr.0 {
                    UnOp::Deref => {
                        // TODO: 检测是否实现 Copy Trait

                        // 在 rust 中，貌似任意 value expr 都可以取其 ref，并且再解引用后可以得到 place value
                        if let ResolvedTy::Ref(t, mutbl) = ty {
                            Ok(Some(ExprResult {
                                category: ExprCategory::Place(mutbl),
                                type_id: self.intern_type(t.as_ref().clone()),
                                int_flow,
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
                                int_flow,
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
                                int_flow,
                            }))
                        } else if ty == ResolvedTy::integer() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::signed_integer()),
                                category: ExprCategory::Not,
                                int_flow,
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

    fn visit_lit_expr(&mut self, expr: &'ast crate::ast::expr::LitExpr) -> Self::ExprRes {
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
                            } else if s == "isize" {
                                self.intern_type(ResolvedTy::isize())
                            } else if s == "usize" {
                                self.intern_type(ResolvedTy::usize())
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
                int_flow: InterruptControlFlow::Not,
            })),
        }
    }

    fn visit_cast_expr(&mut self, expr: &'ast crate::ast::expr::CastExpr) -> Self::ExprRes {
        let res = self.visit_expr(&expr.0)?;
        match res {
            Some(ExprResult {
                type_id,
                category,
                int_flow,
            }) => {
                debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                no_assignee!(category);

                let expr_ty = self.get_type_by_id(type_id);
                let target_ty = self.resolve_ty(&expr.1)?;

                if (expr_ty.is_number_type()
                    || expr_ty == ResolvedTy::char()
                    || expr_ty == ResolvedTy::bool())
                    && (target_ty == ResolvedTy::i32()
                        || target_ty == ResolvedTy::u32()
                        || target_ty == ResolvedTy::isize()
                        || target_ty == ResolvedTy::usize())
                {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(target_ty),
                        category: ExprCategory::Not,
                        int_flow,
                    }))
                } else {
                    Err(SemanticError::IncompatibleCast)
                }
            }
            None => Ok(None),
        }
    }

    fn visit_let_expr(&mut self, _: &'ast crate::ast::expr::LetExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_if_expr(&mut self, IfExpr(condition, take, els): &'ast IfExpr) -> Self::ExprRes {
        let con_res = self.visit_expr(condition)?;
        let take_res = self.visit_block_expr(take)?;
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
                        int_flow: con_res
                            .int_flow
                            .concat(take_res.int_flow.shunt(els_res.int_flow)),
                    }))
                } else if take_res.type_id == Self::unit_type()
                    || take_res.type_id == Self::never_type()
                {
                    Ok(Some(Self::unit_expr_result_int_specified(con_res.int_flow)))
                } else {
                    Err(SemanticError::TypeMismatch)
                }
            }
            (None, None) => Ok(None),
            _ => panic!("Impossible"),
        }
    }

    fn visit_while_expr(&mut self, WhileExpr(condition, body): &'ast WhileExpr) -> Self::ExprRes {
        let con_res = self.visit_expr(condition)?;
        let body_res = self.visit_block_expr_with_kind(body, ScopeKind::CycleExceptLoop)?;

        match (con_res, body_res) {
            (Some(con_res), Some(body_res)) => {
                no_assignee!(con_res.category);

                let con_ty = self.get_type_by_id(con_res.type_id);
                if con_ty != ResolvedTy::bool() {
                    return Err(SemanticError::TypeMismatch);
                }

                let body_int_flow = match body_res {
                    StmtResult::Expr(expr_result) => {
                        if expr_result.type_id != Self::unit_type() {
                            return Err(SemanticError::TypeMismatch);
                        }
                        expr_result.int_flow
                    }
                    StmtResult::Else { int_flow } => int_flow,
                };

                let out_of_cycle_int_flow = body_int_flow.out_of_cycle();

                Ok(Some(Self::unit_expr_result_int_specified(
                    con_res.int_flow.concat(out_of_cycle_int_flow),
                )))
            }
            (None, None) => Ok(None),
            _ => panic!("Impossible"),
        }
    }

    fn visit_for_loop_expr(&mut self, _: &'ast crate::ast::expr::ForLoopExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_loop_expr(&mut self, LoopExpr(block): &'ast LoopExpr) -> Self::ExprRes {
        let res = self.visit_block_expr_with_kind(block, ScopeKind::Loop { ret_ty: None })?;

        match res {
            Some(res) => {
                if res
                    .as_expr()
                    .is_some_and(|x| x.type_id != Self::unit_type())
                {
                    return Err(SemanticError::TypeMismatch);
                }

                let int_flow = match res {
                    StmtResult::Expr(expr_result) => {
                        if expr_result.type_id != Self::unit_type() {
                            return Err(SemanticError::TypeMismatch);
                        }
                        expr_result.int_flow
                    }
                    StmtResult::Else { int_flow } => int_flow,
                };

                if int_flow.is_not() {
                    Ok(Some(Self::never_expr_result_int_specified(
                        InterruptControlFlow::Return,
                    )))
                } else {
                    let out_of_cycle_int_flow = int_flow.out_of_cycle();

                    Ok(Some(ExprResult {
                        type_id: self
                            .get_scope_by_id(block.id)
                            .kind
                            .as_loop()
                            .unwrap()
                            .unwrap_or(Self::never_type()),
                        category: ExprCategory::Not,
                        int_flow: out_of_cycle_int_flow,
                    }))
                }
            }
            None => Ok(None),
        }
    }

    fn visit_match_expr(&mut self, _: &'ast crate::ast::expr::MatchExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_block_expr(&mut self, expr: &'ast crate::ast::expr::BlockExpr) -> Self::ExprRes {
        self.visit_block_expr_with_kind(expr, ScopeKind::Lambda)
            .map(|x| match x {
                Some(StmtResult::Expr(expr_result)) => Some(expr_result),
                Some(StmtResult::Else { int_flow }) => Some(if int_flow.is_not() {
                    Self::unit_expr_result_int_specified(InterruptControlFlow::Not)
                } else {
                    Self::never_expr_result_int_specified(int_flow)
                }),
                None => None,
            })
    }

    // TODO: struct A; A = A;
    fn visit_assign_expr(&mut self, AssignExpr(left, right): &'ast AssignExpr) -> Self::ExprRes {
        let right_res = self.visit_expr(right)?;
        let left_res = self.visit_expr(left)?;

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

                Ok(Some(Self::unit_expr_result_int_specified(
                    right_res.int_flow.concat(left_res.int_flow),
                )))
            }
        }
    }

    fn visit_assign_op_expr(
        &mut self,
        AssignOpExpr(op, left, right): &'ast AssignOpExpr,
    ) -> Self::ExprRes {
        let right_res = self.visit_expr(right)?;
        let left_res = self.visit_expr(left)?;

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
                    Ok(Some(Self::unit_expr_result_int_specified(
                        right_res.int_flow.concat(left_res.int_flow),
                    )))
                } else {
                    Err(SemanticError::Unimplemented)
                }
            }
        }
    }

    fn visit_field_expr(
        &mut self,
        FieldExpr(expr, ident): &'ast crate::ast::expr::FieldExpr,
    ) -> Self::ExprRes {
        let res = self.visit_expr(expr)?;

        match res {
            Some(res) => {
                let res_mut = get_mutbl!(res.category);
                let (field, deref_level) = self.get_type_fields(res.type_id, &ident.symbol)?;

                Ok(Some(ExprResult {
                    type_id: field.ty,
                    category: ExprCategory::Place(res_mut.merge_with_deref_level(deref_level)),
                    int_flow: res.int_flow,
                }))
            }
            None => Ok(None),
        }
    }

    fn visit_index_expr(&mut self, expr: &'ast crate::ast::expr::IndexExpr) -> Self::ExprRes {
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
                        int_flow: res1.int_flow.concat(res2.int_flow),
                    }))
                } else {
                    Err(SemanticError::TypeMismatch)
                }
            }
            (None, None) => Ok(None),
            (None, Some(_)) | (Some(_), None) => panic!("Impossible"),
        }
    }

    fn visit_range_expr(&mut self, _: &'ast crate::ast::expr::RangeExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_underscore_expr(
        &mut self,
        _: &'ast crate::ast::expr::UnderscoreExpr,
    ) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => Ok(Some(ExprResult {
                type_id: self.intern_type(ResolvedTy::Infer),
                category: ExprCategory::Only,
                int_flow: InterruptControlFlow::Not,
            })),
        }
    }

    fn visit_path_expr(&mut self, PathExpr(qself, path): &'ast PathExpr) -> Self::ExprRes {
        let old_state = self.state;

        if matches!(self.stage, AnalyzeStage::Body) {
            let value = self.search_value_by_path(qself, path)?;
            match value {
                ValueContainer::Variable(variable) => {
                    let variable: &Variable = match &variable.kind {
                        VariableKind::Constant(ConstEvalValue::UnEvaled(
                            node_id,
                            span,
                            item_ptr,
                        )) => {
                            let var_ptr = &raw const *variable;

                            // Copied from const_eval.rs @visit_path_expr

                            let item = unsafe { &**item_ptr };
                            self.state = AnalyzerState {
                                current_ast_id: *node_id,
                                current_span: *span,
                            };

                            let ty = self.resolve_ty(&item.ty)?;
                            let ty_id = self.intern_type(ty.clone());
                            let evaled_value = self.const_eval(ty, item.expr.as_ref().unwrap())?;

                            let var = self.search_value_mut(&item.ident.symbol)?;

                            assert!(std::ptr::eq(var_ptr, &raw const *var));

                            *var = Variable {
                                ty: ty_id,
                                mutbl: crate::ast::Mutability::Not,
                                kind: VariableKind::Constant(evaled_value.clone()),
                            };

                            self.state = old_state;

                            unsafe { &*var_ptr }
                        }
                        _ => variable,
                    };

                    Ok(Some(ExprResult {
                        type_id: variable.ty,
                        category: ExprCategory::Place(variable.mutbl),
                        int_flow: InterruptControlFlow::Not,
                    }))
                }
                ValueContainer::ImplInfoItem(type_id, _) => Ok(Some(ExprResult {
                    type_id,
                    category: ExprCategory::Place(Mutability::Not),
                    int_flow: InterruptControlFlow::Not,
                })),
                ValueContainer::Temp(variable) => {
                    debug_assert!(
                        variable
                            .kind
                            .as_constant()
                            .map(|x| !x.is_un_evaled())
                            .unwrap_or(true)
                    );
                    Ok(Some(ExprResult {
                        type_id: variable.ty,
                        category: ExprCategory::Place(variable.mutbl),
                        int_flow: InterruptControlFlow::Not,
                    }))
                }
            }
        } else {
            Ok(None)
        }
    }

    fn visit_addr_of_expr(&mut self, expr: &'ast crate::ast::expr::AddrOfExpr) -> Self::ExprRes {
        match self.visit_expr(&expr.1)? {
            Some(ExprResult {
                type_id,
                category,
                int_flow,
            }) => {
                no_assignee!(category);
                let ty = self.get_type_by_id(type_id);
                let ret_ty = ResolvedTy::Ref(Box::new(ty.clone()), expr.0);
                Ok(Some(ExprResult {
                    type_id: self.intern_type(ret_ty),
                    category: ExprCategory::Not,
                    int_flow,
                }))
            }
            None => Ok(None),
        }
    }

    fn visit_break_expr(&mut self, expr: &'ast crate::ast::expr::BreakExpr) -> Self::ExprRes {
        let res = if let Some(expr) = &expr.0 {
            self.visit_expr(expr)?
        } else {
            None
        };

        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => {
                debug_assert!(res.is_none());
                Ok(None)
            }
            AnalyzeStage::Body => {
                let target_scope = self.get_cycle_scope_mut()?;

                let mut int_flow = InterruptControlFlow::Not;

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
                            int_flow = new.int_flow;
                            *ret_ty = Some(new.type_id)
                        }
                        (Some(new), Some(exist)) => {
                            no_assignee!(new.category);
                            int_flow = new.int_flow;
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

                Ok(Some(Self::never_expr_result_int_specified(
                    int_flow.concat(InterruptControlFlow::Loop),
                ))) // 没有 Never Type，使用 Unit Type 代替
            }
        }
    }

    fn visit_continue_expr(&mut self, _: &'ast crate::ast::expr::ContinueExpr) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => {
                self.get_cycle_scope_mut()?;
                Ok(Some(Self::never_expr_result_int_specified(
                    InterruptControlFlow::Loop,
                )))
            }
        }
    }

    fn visit_ret_expr(&mut self, RetExpr(expr): &'ast RetExpr) -> Self::ExprRes {
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
                let target_id = scope.kind.as_fn().unwrap().0;

                let ret_ty = self.get_type_by_id(res.type_id);
                let target_ty = self.get_type_by_id(*target_id);

                if !ret_ty.can_trans_to_target_type(&target_ty) {
                    Err(SemanticError::TypeMismatch)
                } else {
                    Ok(Some(Self::never_expr_result_int_specified(
                        res.int_flow.concat(InterruptControlFlow::Return),
                    )))
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
        }: &'ast StructExpr,
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
                            .map(|(s, e)| e.map(|e| (s, e)))
                            .collect::<Option<Vec<_>>>()
                            .unwrap();

                        let category = Self::utilize_category(
                            exp_fields.iter().map(|x| x.1.category).collect(),
                        )?;

                        let int_flow = exp_fields.iter().fold(
                            InterruptControlFlow::Not,
                            |x,
                             (
                                _,
                                ExprResult {
                                    type_id: _,
                                    category: _,
                                    int_flow: y,
                                },
                            )| x.concat(*y),
                        );

                        let mut dic: HashMap<Symbol, ExprResult> = HashMap::new();

                        for x in exp_fields.into_iter() {
                            if dic.insert(x.0, x.1).is_some() {
                                return Err(SemanticError::MultiSpecifiedField);
                            }
                        }

                        for (field_ident, field_type_id) in fields {
                            let Some(res) = dic.get(field_ident) else {
                                return Err(SemanticError::MissingField);
                            };
                            let res_ty = self.get_type_by_id(res.type_id);
                            let field_ty = self.get_type_by_id(*field_type_id);
                            if !res_ty.can_trans_to_target_type(&field_ty) {
                                return Err(SemanticError::TypeMismatch);
                            }
                        }

                        Ok(Some(ExprResult {
                            type_id: self.intern_type(
                                self.resolve_ty_in_scope_by_symbol(&struct_info.name, id),
                            ),
                            category,
                            int_flow,
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

    fn visit_repeat_expr(
        &mut self,
        RepeatExpr(expr, const_expr): &'ast RepeatExpr,
    ) -> Self::ExprRes {
        let res = self.visit_expr(expr)?;

        match res {
            Some(res) => {
                let size = self.const_eval(ResolvedTy::usize(), &const_expr.value)?;
                no_assignee!(res.category);

                Ok(Some(ExprResult {
                    type_id: self.intern_type(ResolvedTy::Array(
                        Box::new(self.get_type_by_id(res.type_id)),
                        size.into_u_size().unwrap(),
                    )),
                    category: ExprCategory::Not,
                    int_flow: res.int_flow,
                }))
            }
            None => Ok(None),
        }
    }

    fn visit_wild_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::WildPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Ok(PatResult {
            bindings: Vec::new(),
        })
    }

    fn visit_ident_pat(
        &mut self,
        IdentPat(mode, ident, guarder): &'ast IdentPat,
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
        _pat: &'ast crate::ast::pat::StructPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_or_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::OrPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_path_pat(
        &mut self,
        PathPat(qself, path): &'ast PathPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        if let Ok(var) = self.search_value_by_path(qself, path)
            && let ValueContainer::Variable(var) = var
            && var.kind.is_constant()
        {
            return Err(SemanticError::Unimplemented);
        }

        let new_pat = IdentPat(
            BindingMode(crate::ast::ByRef::No, Mutability::Not),
            path.get_ident().clone(),
            None,
        );

        self.visit_ident_pat(&new_pat, expected_ty)
    }

    fn visit_tuple_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::TuplePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_ref_pat(
        &mut self,
        RefPat(pat, mutbl): &'ast RefPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
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
        _pat: &'ast crate::ast::pat::LitPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        todo!()
    }

    fn visit_range_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::RangePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_slice_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::SlicePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_rest_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::RestPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }
}
