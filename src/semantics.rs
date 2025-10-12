pub mod error;
pub mod forir;
pub mod primitives;
pub mod resolved_ty;
pub mod super_trait;
pub mod utils;
pub mod visitor;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter,
    rc::Rc,
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
    const_eval::{ConstEvalValue, ConstEvaler},
    irgen::IRGenHelper,
    lexer::TokenPosition,
    make_semantic_error,
    semantics::{
        error::SemanticError,
        resolved_ty::{PreludePool, ResolvedTy, ResolvedTypes, TypePtr},
        utils::*,
        visitor::Visitor,
    },
};

macro_rules! no_assignee {
    ($id:expr) => {
        if matches!($id, ExprCategory::Only) {
            return Err(make_semantic_error!(AssigneeOnlyExpr));
        }
    };
}

macro_rules! check_sized {
    ($analyzer:expr, $id:expr) => {
        if !$analyzer.has_sized_trait($id) {
            return Err(make_semantic_error!(NotSizedType));
        }
    };
}

macro_rules! get_mutbl {
    ($id:expr) => {
        match $id {
            ExprCategory::Only => return Err(make_semantic_error!(AssigneeOnlyExpr)),
            ExprCategory::Place(mutbl) => mutbl,
            ExprCategory::Not => Mutability::Mut,
        }
    };
}

macro_rules! pool {
    ($analyzer:expr, $name:ident) => {
        $analyzer.prelude_pool.$name.clone()
    };
}

macro_rules! pool_ref {
    ($analyzer:expr, $name:ident) => {
        &$analyzer.prelude_pool.$name
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
    impls: HashMap<TypePtr, Impls>, // (本征 impl, trait impl)
    pub(crate) scopes: HashMap<NodeId, Scope>,
    current_scope: NodeId,
    stage: AnalyzeStage,
    pub(crate) state: AnalyzerState,

    builtin_impls: BuiltInImpls,
    pub(crate) prelude_pool: PreludePool,

    ir_helper: RefCell<IRGenHelper>,
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
        let pool = PreludePool::default();

        let preludes = [
            (
                Symbol("print".to_string()),
                Variable {
                    ty: ResolvedTy::Fn(vec![pool.ref_str.clone()], pool.unit.clone()).into(),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("println".to_string()),
                Variable {
                    ty: ResolvedTy::Fn(vec![pool.ref_str.clone()], pool.unit.clone()).into(),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("printInt".to_string()),
                Variable {
                    ty: ResolvedTy::Fn(vec![pool.i32.clone()], pool.unit.clone()).into(),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("printlnInt".to_string()),
                Variable {
                    ty: ResolvedTy::Fn(vec![pool.i32.clone()], pool.unit.clone()).into(),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("getString".to_string()),
                Variable {
                    ty: ResolvedTy::Fn(vec![], pool.string.clone()).into(),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("getInt".to_string()),
                Variable {
                    ty: ResolvedTy::Fn(vec![], pool.i32.clone()).into(),
                    mutbl: Mutability::Not,
                    kind: VariableKind::Fn,
                },
            ),
            (
                Symbol("exit".to_string()),
                Variable {
                    ty: ResolvedTy::Fn(vec![pool.i32.clone()], pool.never.clone()).into(),
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

        Self {
            impls: HashMap::default(),
            scopes: HashMap::from([(0, root_scope)]),
            current_scope: 0,
            stage: AnalyzeStage::SymbolCollect,
            state: AnalyzerState::default(),
            builtin_impls: BuiltInImpls::new(&pool),
            prelude_pool: pool,

            ir_helper: IRGenHelper::new().into(),
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
                | ScopeKind::Fn { .. } => return Err(make_semantic_error!(NoLoopScope)),
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
                    return Err(make_semantic_error!(NoFnScope));
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
                    return Err(make_semantic_error!(NoFnScope));
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
            return Err(make_semantic_error!(InvalidScope));
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
            return Err(make_semantic_error!(UndefinedScope));
        }

        self.current_scope = id;

        Ok(())
    }

    fn exit_scope(&mut self) -> Result<(), SemanticError> {
        if matches!(self.get_scope().kind, ScopeKind::Root) {
            return Err(make_semantic_error!(InvalidScope));
        }

        self.current_scope = self.get_scope().father;

        Ok(())
    }

    fn add_type(&mut self, ident: Symbol, info: TypeInfo) -> Result<TypePtr, SemanticError> {
        let s = self.get_scope_mut();

        if s.types.contains_key(&ident) {
            return Err(make_semantic_error!(MultiDefined));
        }

        s.types.insert(ident.clone(), info);

        let resolved = ResolvedTy::Named(self.get_full_name(ident));

        Ok(Rc::new(resolved))
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
            return Err(make_semantic_error!(MultiDefined));
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
                return Err(make_semantic_error!(UndefinedScope));
            };

            if let Some(info) = scope.types.get(ident) {
                return Ok((id, info));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(make_semantic_error!(UnknownType));
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
                return Err(make_semantic_error!(UndefinedScope));
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
                return Err(make_semantic_error!(UnknownType));
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
                return Err(make_semantic_error!(UndefinedScope));
            };

            if let Some(var) = scope.values.get(ident) {
                if !include_local && matches!(var.kind, VariableKind::Decl | VariableKind::Inited) {
                    return Err(make_semantic_error!(LocalVarOutOfFn));
                }
                return Ok((id, var));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(make_semantic_error!(UnknownVariable));
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
                return Err(make_semantic_error!(UndefinedScope));
            };

            if let Some(var) = scope.values.get(ident) {
                if !include_local && matches!(var.kind, VariableKind::Decl | VariableKind::Inited) {
                    return Err(make_semantic_error!(LocalVarOutOfFn));
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
                return Err(make_semantic_error!(UnknownVariable));
            } else {
                id = scope.father;

                if matches!(scope.kind, ScopeKind::Fn { .. }) {
                    include_local &= false;
                }
            }
        }
    }

    fn get_types_fields(
        &self,
        tys: ResolvedTypes,
        symbol: &Symbol,
    ) -> Result<(Variable, DerefLevel), SemanticError> {
        match tys {
            ResolvedTypes::Types(hash_set) => {
                let a: Vec<(Variable, DerefLevel)> = hash_set
                    .into_iter()
                    .filter_map(|x| self.get_type_fields(x, symbol).ok())
                    .collect();

                if a.is_empty() {
                    return Err(make_semantic_error!(NonProvidedField));
                }

                debug_assert!(a.len() < 2);

                Ok(a.into_iter().next().unwrap())
            }
            ResolvedTypes::Ref(resolved_types, mutability) => {
                let (var, deref_level) = self.get_types_fields(*resolved_types, symbol)?;
                Ok((var, deref_level.merge(DerefLevel::Deref(mutability))))
            }
            _ => Err(make_semantic_error!(NonProvidedField)),
        }
    }

    // fields 不包括 method，field 会自动 deref
    fn get_type_fields(
        &self,
        ty: TypePtr,
        symbol: &Symbol,
    ) -> Result<(Variable, DerefLevel), SemanticError> {
        match &ty.as_ref() {
            ResolvedTy::Named(symbols) => {
                let info = self.get_type_info(symbols);
                match &info.kind {
                    TypeKind::Placeholder => panic!("Impossible!"),
                    TypeKind::Struct { fields } => {
                        for (ident, field_ty) in fields {
                            if ident == symbol {
                                return Ok((
                                    Variable {
                                        ty: field_ty.clone(),
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

                Err(make_semantic_error!(NonProvidedField))
            }
            ResolvedTy::Ref(de_refed_ty, mutability) => {
                let (var, deref_level) = self.get_type_fields(de_refed_ty.clone(), symbol)?;

                Ok((var, deref_level.merge(DerefLevel::Deref(*mutability))))
            }
            ResolvedTy::ImplicitSelf => self.get_type_fields(self.get_self_type()?, symbol),
            _ => Err(make_semantic_error!(NonProvidedField)),
        }
    }

    fn get_type_items_no_deref(
        &self,
        ty: &TypePtr,
        is_methods_call: bool,
        name: &Symbol,
    ) -> Result<Option<(TypePtr, ImplInfoItem<'_>)>, SemanticError> {
        fn get_impl_item_type(
            item: &ImplInfoItem,
            is_methods_call: bool,
            self_ty: &TypePtr,
        ) -> Option<TypePtr> {
            match &item {
                ImplInfoItem::Method(fn_sig) => {
                    let fn_ty = fn_sig.ty.clone();

                    match (is_methods_call, fn_ty.is_method()) {
                        (true, true) => Some(fn_ty),
                        (true, false) => None,
                        (false, true) => Some(fn_ty.method_to_func(self_ty).into()),
                        (false, false) => Some(fn_ty),
                    }
                }
                ImplInfoItem::Constant(constant) => {
                    if !is_methods_call {
                        Some(constant.ty.clone())
                    } else {
                        None
                    }
                }
            }
        }

        // builtin 在 get_impl 中返回，从而此处不需要特殊处理
        // visit_impl_item 时保证了 implInfo 中的 fn 只有第一位可能出现 Self 类型，其他位置的 Self 都被展开了
        if let Some((inherent_impl, trait_impls)) = self.get_impl(ty) {
            if let Some(item) = inherent_impl.get(name) {
                let replacer = get_impl_item_type(&item, is_methods_call, ty);
                if let Some(replacer) = replacer {
                    return Ok(Some((replacer, item)));
                }
            };

            let mut candidate = None;

            for trait_impl in trait_impls.values() {
                if let Some(item) = trait_impl.get(name) {
                    let replacer = get_impl_item_type(&item, is_methods_call, ty);
                    if let Some(replacer) = replacer
                        && candidate.replace((replacer, item)).is_some()
                    {
                        return Err(make_semantic_error!(MultipleApplicable));
                    }
                }
            }

            Ok(candidate)
        } else {
            Ok(None)
        }
    }

    fn get_types_items_no_deref(
        &self,
        tys: &ResolvedTypes,
        is_methods_call: bool,
        name: &Symbol,
    ) -> Result<Option<(TypePtr, ImplInfoItem<'_>)>, SemanticError> {
        let tys = tys.to_resolved_tys();
        let results = tys
            .into_iter()
            .map(|x| self.get_type_items_no_deref(&x, is_methods_call, name))
            .collect::<Result<Vec<Option<(TypePtr, ImplInfoItem<'_>)>>, SemanticError>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<(TypePtr, ImplInfoItem<'_>)>>();

        if results.is_empty() {
            Ok(None)
        } else {
            // 暂时先返回第一个，这里直接返回一个列表可能会更好
            Ok(Some(results.into_iter().next().unwrap()))
        }
    }

    // item 包含 method
    fn get_types_items(
        &self,
        mut tys: ResolvedTypes,
        is_methods_call: bool,
        name: &Symbol,
    ) -> Result<(TypePtr, DerefLevel), SemanticError> {
        let mut deref_level = DerefLevel::Not;

        loop {
            if let Some(ret) = self.get_types_items_no_deref(&tys, is_methods_call, name)? {
                return Ok((ret.0, deref_level));
            }
            let Some(de) = tys.deref_once() else {
                return Err(make_semantic_error!(NonMethodCall));
            };

            deref_level = deref_level.merge(DerefLevel::Deref(de.1));
            tys = de.0;
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
        if qself.is_some() {
            return Err(make_semantic_error!(Unimplemented));
        }

        if path.segments.len() > 1 {
            return Err(make_semantic_error!(Unimplemented));
        }

        let first = path.segments.first().unwrap();
        if first.args.is_some() {
            return Err(make_semantic_error!(Unimplemented));
        }

        self.search_type(&first.ident.symbol)
    }

    pub(crate) fn search_value_by_path(
        &mut self,
        qself: &Option<Box<QSelf>>,
        path: &Path,
    ) -> Result<ValueContainer<'_>, SemanticError> {
        if qself.is_some() {
            return Err(make_semantic_error!(Unimplemented));
        }

        // 在没有 module 时，path expr 应该只可能为 value 或 ty::value 格式
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

                // 枚举Type::Variant，这个不能简单的直接在 inherit impl 中定义几个常量
                if let ResolvedTy::Named(fullname) = ty.clone().as_ref() {
                    let info = self.get_type_info(fullname);
                    if let TypeKind::Enum { fields } = &info.kind
                        && fields.contains(&value_seg.ident.symbol)
                    {
                        return Ok(ValueContainer::Temp(Variable {
                            ty,
                            mutbl: Mutability::Not,
                            kind: VariableKind::Constant(ConstEvalValue::Enum(
                                fullname.clone(),
                                value_seg.ident.symbol.clone(),
                            )),
                        }));
                    }
                }

                self.get_type_items_no_deref(&ty, false, &value_seg.ident.symbol)?
                    .map(|(id, v)| ValueContainer::ImplInfoItem(id, v))
                    .ok_or(make_semantic_error!(UnknownVariable))
            }
            _ => Err(make_semantic_error!(InvalidPath)),
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

    pub fn get_type_info(&self, full_name: &FullName) -> &TypeInfo {
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

    pub(crate) fn get_self_type_from(&self, mut id: NodeId) -> Result<TypePtr, SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get(&id) {
                scope
            } else {
                return Err(make_semantic_error!(UndefinedScope));
            };

            match &scope.kind {
                ScopeKind::Crate => return Err(make_semantic_error!(UnknownType)),
                ScopeKind::Trait(ty) | ScopeKind::Impl(ty) => return Ok(ty.clone()),
                _ => id = scope.father,
            }
        }
    }

    fn get_self_type(&self) -> Result<TypePtr, SemanticError> {
        self.get_self_type_from(self.current_scope)
    }

    fn resolve_ty_self_kind_specified(
        &mut self,
        ty: &Ty,
        expand_self: bool,
    ) -> Result<TypePtr, SemanticError> {
        Ok(Rc::new(match &ty.kind {
            TyKind::Slice(slice_ty) => {
                ResolvedTy::Slice(self.resolve_ty_self_kind_specified(&slice_ty.0, expand_self)?)
            }
            TyKind::Array(array_ty) => ResolvedTy::Array(
                {
                    let inner = self.resolve_ty_self_kind_specified(&array_ty.0, expand_self)?;
                    check_sized!(self, &inner);
                    inner
                },
                self.const_eval(pool!(self, usize), &array_ty.1.value)?
                    .into_u_size()
                    .unwrap(),
            ),
            TyKind::Ref(RefTy(MutTy { ty: ty2, mutbl })) => ResolvedTy::Ref(
                self.resolve_ty_self_kind_specified(ty2, expand_self)?,
                *mutbl,
            ),
            TyKind::Tup(tup_ty) => ResolvedTy::Tup(
                tup_ty
                    .0
                    .iter()
                    .map(|x| self.resolve_ty_self_kind_specified(x, expand_self))
                    .collect::<Result<Vec<_>, SemanticError>>()?,
            ),
            TyKind::Path(path_ty) => {
                let PathTy(qself, path) = path_ty;
                if qself.is_some() {
                    return Err(make_semantic_error!(Unimplemented));
                }

                if path.segments.len() > 1 {
                    return Err(make_semantic_error!(InvalidPath));
                }

                let seg = path.segments.first().unwrap();
                let s = &seg.ident.symbol;
                if s.is_path_segment() {
                    if s.is_big_self() {
                        if seg.args.is_some() {
                            return Err(make_semantic_error!(InvalidPath));
                        }
                        return Ok(if expand_self {
                            self.get_self_type()?
                        } else {
                            pool!(self, big_self)
                        });
                    }
                    return Err(make_semantic_error!(InvalidPath));
                }
                match self.search_type(s) {
                    Ok((id, _)) => self.resolve_ty_in_scope_by_symbol(s, id),
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
                            ResolvedTy::BuiltIn(s.clone(), args_v)
                        } else {
                            return Err(err);
                        }
                    }
                }
            }
            TyKind::TraitObject(_) => return Err(make_semantic_error!(Unimplemented)),
            TyKind::ImplTrait(_) => return Err(make_semantic_error!(Unimplemented)),
            TyKind::Infer(_) => return Err(make_semantic_error!(Unimplemented)),
            TyKind::ImplicitSelf => return Ok(pool!(self, implicit_self)),
        }))
    }

    // 这个函数不会展开 self（为了保留函数参数中的 self），但是会展开 Self
    pub(crate) fn resolve_ty(&mut self, ty: &Ty) -> Result<TypePtr, SemanticError> {
        self.resolve_ty_self_kind_specified(ty, true)
    }

    fn resolve_ty_in_scope_by_symbol(&self, s: &Symbol, id: usize) -> ResolvedTy {
        ResolvedTy::Named(self.get_full_name_from(id, s.clone()))
    }

    fn unit_expr_result_int_specified(&self, int_flow: InterruptControlFlow) -> ExprResult {
        ExprResult {
            expr_tys: pool!(self, unit).into(),
            category: ExprCategory::Not,
            int_flow,
            value: None,
        }
    }

    fn unit_expr_result(&self) -> ExprResult {
        self.unit_expr_result_int_specified(InterruptControlFlow::Not)
    }

    fn never_expr_result_int_specified(&self, int_flow: InterruptControlFlow) -> ExprResult {
        ExprResult {
            expr_tys: ResolvedTypes::Never,
            category: ExprCategory::Not,
            int_flow,
            value: None,
        }
    }

    pub(crate) fn const_eval(
        &mut self,
        ty: TypePtr,
        expr: &Expr,
    ) -> Result<ConstEvalValue, SemanticError> {
        match ConstEvaler::eval(self, expr) {
            Ok(x) => x.cast(&self.prelude_pool, &ty.into(), false),
            Err(err) => Err(err),
        }
        .map_err(|x| x.into())
    }

    fn utilize_category(cats: Vec<ExprCategory>) -> Result<ExprCategory, SemanticError> {
        let has_only = cats.iter().any(|x| matches!(x, ExprCategory::Only));
        let has_value = cats.iter().any(|x| matches!(x, ExprCategory::Not));
        let has_const = cats
            .iter()
            .any(|x| matches!(x, ExprCategory::Place(Mutability::Not)));

        match (has_only, has_value, has_const) {
            (true, true, _) => Err(make_semantic_error!(ConflictAssignee)),
            (true, false, true) => Err(make_semantic_error!(ImmutableVar)),
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
            self.add_scope(kind)?;
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
                            return Err(make_semantic_error!(TypeMismatch));
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
                                return Err(make_semantic_error!(ConstantWithoutBody));
                            }
                        }
                    };

                    if methods.contains_key(&ident.symbol) || constants.contains_key(&ident.symbol)
                    {
                        return Err(make_semantic_error!(MultiDefined));
                    }

                    constants.insert(
                        ident.symbol.clone(),
                        Constant {
                            ty: resolved_ty,
                            value,
                        },
                    );
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
                        FnRetTy::Default => pool!(self, unit),
                        FnRetTy::Ty(ty) => self.resolve_ty_self_kind_specified(ty, expand_self)?,
                    };
                    let fn_type = ResolvedTy::Fn(param_tys, ret_ty);

                    if methods.contains_key(&ident.symbol) || constants.contains_key(&ident.symbol)
                    {
                        return Err(make_semantic_error!(MultiDefined));
                    }

                    if body.is_none() && !is_trait_item {
                        return Err(make_semantic_error!(FnWithoutBody));
                    }

                    methods.insert(
                        ident.symbol.clone(),
                        FnSig {
                            ty: fn_type.into(),
                            is_placeholder: body.is_none(),
                        },
                    );
                }
            }
            self.state = old_state;
        }
        Ok((methods, constants))
    }

    fn get_impl(&self, ty: &TypePtr) -> Option<&Impls> {
        if let Some(i) = self.impls.get(ty) {
            Some(i)
        } else if *ty == pool!(self, u32) || *ty == pool!(self, i32) {
            Some(&self.builtin_impls.u32_and_usize_and_integer)
        } else if *ty == pool!(self, string) {
            Some(&self.builtin_impls.string)
        } else if *ty == pool!(self, str) {
            Some(&self.builtin_impls.str)
        } else if ty.is_array() || ty.is_slice() {
            Some(&self.builtin_impls.array_and_slice)
        } else {
            None
        }
    }

    fn get_impl_mut(&mut self, ty: &TypePtr) -> &mut Impls {
        if !self.impls.contains_key(ty) {
            self.impls.insert(
                ty.clone(),
                self.get_impl(ty).cloned().unwrap_or((
                    ImplInfo {
                        methods: HashMap::new(),
                        constants: HashMap::new(),
                    },
                    HashMap::new(),
                )),
            );
        }

        self.impls.get_mut(ty).unwrap()
    }

    fn add_bindings(
        &mut self,
        pat_res_es: Vec<PatResult>,
        kind: VariableKind,
    ) -> Result<(), SemanticError> {
        let mut set: HashSet<Symbol> = HashSet::new();

        for pat_res in pat_res_es {
            for (symbol, ty, mutbl) in pat_res.bindings {
                if !set.insert(symbol.clone()) {
                    return Err(make_semantic_error!(MultiBinding));
                }

                // bindings 不能遮蔽 const, 即使不同作用域
                // binding 还应该检查没有重名
                if let Ok(var) = self.search_value(&symbol)
                    && var.kind.is_constant()
                {
                    return Err(make_semantic_error!(ShadowedConstantByBinding));
                }

                self.add_value(
                    symbol,
                    Variable {
                        ty,
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
            AnalyzeStage::Body => {
                // let mut helper = self.ir_helper.borrow_mut();
                // helper.add_struct_declares(&self, krate.id);
                // helper.complete_struct_declares(&self);
                // helper.add_globals(&self, krate.id);
            }
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
                        return Err(make_semantic_error!(ConstantWithoutBody));
                    }

                    self.add_value(
                        item.ident.symbol.clone(),
                        Variable {
                            ty: pool!(self, unit),
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
            AnalyzeStage::Impl => {
                if self.is_free_scope() {
                    let var = self.search_value_mut(&item.ident.symbol).unwrap();

                    if var.kind.as_constant().unwrap().is_un_evaled() {
                        let ty = self.resolve_ty(&item.ty)?;
                        check_sized!(self, &ty);
                        let value = self.const_eval(ty.clone(), item.expr.as_ref().unwrap())?;

                        let var = self.search_value_mut(&item.ident.symbol).unwrap();
                        *var = Variable {
                            ty,
                            mutbl: Mutability::Not,
                            kind: VariableKind::Constant(value),
                        }
                    }
                }
            }
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
        }: &'ast FnItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                let is_main_fn =
                    ident.symbol.0.as_str() == "main" && self.get_scope().kind.is_crate(); // 现在只允许存在一个 Crate，因此满足这个条件的就是 main fn
                self.add_scope(ScopeKind::Fn {
                    ret_ty: pool!(self, unit),
                    main_fn: if is_main_fn {
                        MainFunctionState::UnExited
                    } else {
                        MainFunctionState::Not
                    },
                })?;
            }
            AnalyzeStage::Definition => {
                let mut ret_ty = match &sig.decl.output {
                    FnRetTy::Default => pool!(self, unit),
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
                    &self.get_scope().kind
                {
                    ret_ty = ret_ty.expand_self(self_ty.clone());
                    param_tys = param_tys
                        .into_iter()
                        .map(|x| x.expand_self(self_ty.clone()))
                        .collect();
                    false
                } else {
                    if ret_ty.is_implicit_self_or_ref_implicit_self()
                        || param_tys
                            .iter()
                            .any(|x| x.is_implicit_self_or_ref_implicit_self())
                    {
                        return Err(make_semantic_error!(SelfInNoAssocFn));
                    }

                    true
                };

                let bindings = sig
                    .decl
                    .inputs
                    .iter()
                    .zip(param_tys.iter())
                    .map(|(param, id)| self.visit_pat(&param.pat, id.clone()))
                    .collect::<Result<Vec<_>, SemanticError>>()?;

                if is_free_scope {
                    let fn_ty = ResolvedTy::Fn(param_tys, ret_ty.clone());

                    self.add_value(
                        ident.symbol.clone(),
                        Variable {
                            ty: fn_ty.into(),
                            mutbl: Mutability::Not,
                            kind: VariableKind::Fn,
                        },
                        false,
                    )?;
                }

                self.enter_scope()?;

                // local 函数参数能被 item 遮蔽，从而应该是在此处添加到 scope 的
                self.add_bindings(bindings, VariableKind::Inited)?;
                let unit = pool!(self, unit);
                let kind = self.get_scope_mut().kind.as_fn_mut().unwrap();
                if kind.1.is_un_exited() && ret_ty != unit {
                    return Err(make_semantic_error!(MainFunctionWithNonUnit));
                }
                *kind.0 = ret_ty;

                self.exit_scope()?;
            }
            AnalyzeStage::Impl => {}
            AnalyzeStage::Body => {}
        }
        self.enter_scope()?;
        if let Some(b) = body {
            let res = self.visit_block_expr_with_kind(b, ScopeKind::Lambda)?;
            if let Some(res) = res {
                let (target_ty, main_fn_state) = self.get_scope().kind.as_fn().unwrap();

                if main_fn_state.is_un_exited() {
                    return Err(make_semantic_error!(MainFunctionNotExited));
                }

                match res {
                    StmtResult::Expr(expr_result) => {
                        let ret_ty = expr_result.expr_tys;
                        if !ret_ty.can_trans_to_target_type(target_ty) {
                            return Err(make_semantic_error!(TypeMismatch));
                        }
                    }
                    StmtResult::Else { int_flow } => match int_flow {
                        InterruptControlFlow::Not => {
                            if *target_ty != pool!(self, unit) {
                                return Err(make_semantic_error!(NoReturnFunction));
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
        Err(make_semantic_error!(Unimplemented))
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
                            Err(make_semantic_error!(Unimplemented))
                        } else {
                            Ok(x.ident.symbol.clone())
                        }
                    })
                    .collect::<Result<Vec<_>, SemanticError>>()?;

                let vec_len = fields.len();
                let fields: HashSet<Symbol> = HashSet::from_iter(fields);
                if vec_len != fields.len() {
                    return Err(make_semantic_error!(MultiDefined));
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
                                    .ok_or(make_semantic_error!(Unimplemented))?
                                    .symbol
                                    .clone(),
                                {
                                    let ty = self.resolve_ty(&x.ty)?;
                                    check_sized!(self, &ty); // 暂时让 struct 的成员均为 sized type
                                    ty
                                },
                            ))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?,
                    crate::ast::item::VariantData::Tuple(_) => {
                        return Err(make_semantic_error!(Unimplemented));
                    }
                    crate::ast::item::VariantData::Unit => Vec::new(),
                };

                let vec_len = fields.len();
                let fields: HashMap<Symbol, TypePtr> = HashMap::from_iter(fields);
                if vec_len != fields.len() {
                    return Err(make_semantic_error!(MultiDefined));
                }

                let (id, info) = self.search_type_mut(&ident.symbol)?;
                *info.kind.as_struct_mut().unwrap() = fields;

                // unit struct 隐式添加了一个 constant
                if variant_data.is_unit() {
                    let self_ty = self.resolve_ty_in_scope_by_symbol(&ident.symbol, id);
                    let fullname = self_ty.as_named().unwrap().clone();
                    self.add_value(
                        ident.symbol.clone(),
                        Variable {
                            ty: self_ty.into(),
                            mutbl: Mutability::Mut,
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
                self.add_scope(ScopeKind::Impl(pool!(self, unit)))?;
            }
            AnalyzeStage::Definition => {
                let self_ty = self.resolve_ty(self_ty)?;

                self.enter_scope()?;
                *self.get_scope_mut().kind.as_impl_mut().unwrap() = self_ty;
                self.exit_scope()?;
            }
            AnalyzeStage::Impl => {
                self.enter_scope()?;
                let self_ty = self.get_scope().kind.as_impl().unwrap().clone();

                // trait 中使用 Self 作为类型，送到 impl 时替换为具体的 Type
                let (mut methods, mut constants) = self.resolve_assoc_items(items, false)?;

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
                            return Err(make_semantic_error!(NotTrait));
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
                                ty: x.ty.expand_self(self_ty.clone()),
                                ..*x
                            }) else {
                                return Err(make_semantic_error!(NotTraitMember));
                            };

                            if sig.ty != trait_sig.ty {
                                return Err(make_semantic_error!(IncompatibleFn));
                            }

                            musts.remove(&symbol);
                        }

                        for (symbol, constant) in &constants {
                            let Some(trait_constant) =
                                trait_constants.get(symbol).map(|x| Constant {
                                    ty: x.ty.expand_self(self_ty.clone()),
                                    value: x.value.clone(),
                                })
                            else {
                                return Err(make_semantic_error!(NotTraitMember));
                            };

                            if constant.ty != trait_constant.ty {
                                return Err(make_semantic_error!(TypeMismatch));
                            }

                            musts.remove(&symbol);
                        }

                        if !musts.is_empty() {
                            return Err(make_semantic_error!(NotAllTraitItemsImplemented));
                        }

                        for (symbol, sig) in trait_methods {
                            if !sig.is_placeholder && !methods.contains_key(symbol) {
                                methods.insert(
                                    symbol.clone(),
                                    FnSig {
                                        ty: sig.ty.expand_self(self_ty.clone()),
                                        is_placeholder: false,
                                    },
                                );
                            }
                        }

                        for (symbol, constant) in trait_constants {
                            if !matches!(constant.value, ConstEvalValue::Placeholder)
                                && !constants.contains_key(symbol)
                            {
                                constants.insert(symbol.clone(), constant.clone()); // Trait 里的默认常量不可能是 Self Type
                            }
                        }

                        let (_, trait_impls) = self.get_impl_mut(&self_ty);

                        if trait_impls.contains_key(&full_name) {
                            return Err(make_semantic_error!(MultiImplemented));
                        }

                        trait_impls.insert(full_name, ImplInfo { methods, constants });
                    }
                    None => {
                        let (inherent_impl, _) = self.get_impl_mut(&self_ty);

                        for (symbol, sig) in methods {
                            if inherent_impl.contains_key(&symbol) {
                                return Err(make_semantic_error!(MultiDefined));
                            }
                            inherent_impl.methods.insert(symbol, sig);
                        }

                        for (symbol, constant) in constants {
                            if inherent_impl.contains_key(&symbol) {
                                return Err(make_semantic_error!(MultiDefined));
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
                            && res.expr_tys.can_trans_to_target_type(pool_ref!(self, unit))
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
            return Err(make_semantic_error!(Unimplemented));
        };

        let expr_res = if let LocalKind::Init(expr) = &stmt.kind {
            self.visit_expr(expr)?
        } else {
            None
        };

        if matches!(self.stage, AnalyzeStage::Body) {
            let expected_ty = self.resolve_ty(ty)?;
            check_sized!(self, &expected_ty);
            let pat_res = self.visit_pat(&stmt.pat, expected_ty.clone())?;

            let int_flow = match &stmt.kind {
                LocalKind::Decl => {
                    // TODO: 检查变量是否完成初始化，需要进行控制流分析，需要允许回退内层 scope 对外层 TypeTable 的修改
                    InterruptControlFlow::Not
                }
                LocalKind::Init(_) => {
                    let ExprResult {
                        expr_tys: expr_ty,
                        category,
                        int_flow,
                        ..
                    } = expr_res.unwrap();

                    no_assignee!(category);
                    if !expr_ty.can_trans_to_target_type(&expected_ty) {
                        return Err(make_semantic_error!(TypeMismatch));
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
                    expr_tys: ty,
                    category,
                    int_flow,
                    ..
                } = x.unwrap();
                (ty, (category, int_flow))
            })
            .unzip();

        let unified_type =
            ResolvedTypes::utilize(types).ok_or(make_semantic_error!(TypeMismatch))?;
        let cat = Self::utilize_category(cats)?;
        let int_flow = ints
            .iter()
            .fold(InterruptControlFlow::Not, |x, y| x.concat(*y));

        let ret_ty = ResolvedTypes::Array(unified_type.into(), expr.0.len() as u32);

        Ok(Some(ExprResult {
            expr_tys: ret_ty,
            category: cat,
            int_flow,
            value: None,
        }))
    }

    fn visit_const_block_expr(
        &mut self,
        _expr: &'ast crate::ast::expr::ConstBlockExpr,
    ) -> Self::ExprRes {
        Err(make_semantic_error!(Unimplemented))
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

                let ty = if let ResolvedTypes::Types(inners) = res.expr_tys
                    && inners.len() == 1
                {
                    inners.into_iter().next().unwrap()
                } else {
                    return Err(make_semantic_error!(TypeMismatch));
                };

                if let ResolvedTy::Fn(required, ret_ty) = ty.as_ref() {
                    if required.len() != params_res.len() {
                        return Err(make_semantic_error!(MismatchArgNum));
                    }

                    let mut int_flow = res.int_flow;
                    for (income, target) in params_res.into_iter().zip(required.iter()) {
                        debug_assert!(!target.is_implicit_self_or_ref_implicit_self());
                        let income_ty = income.expr_tys;
                        int_flow = int_flow.concat(income.int_flow);

                        if !income_ty.can_trans_to_target_type(target) {
                            return Err(make_semantic_error!(TypeMismatch));
                        }
                    }

                    if let Ok(fn_scope) = self.get_fn_scope_mut() {
                        let (_, main_fn_state) = fn_scope.kind.as_fn_mut().unwrap();
                        match (&main_fn_state, ret_ty.is_never()) {
                            // 检查 Exit 函数，只有 Exit 函数返回类型为 Never
                            (MainFunctionState::Not, true) => {
                                return Err(make_semantic_error!(NotMainFunction));
                            }
                            (MainFunctionState::UnExited, true) => {
                                *main_fn_state = MainFunctionState::Exited
                            }
                            (MainFunctionState::Exited, _) => {
                                return Err(make_semantic_error!(ExprAfterExit));
                            }
                            _ => {}
                        }
                    }

                    Ok(Some(ExprResult {
                        expr_tys: ret_ty.clone().into(),
                        category: ExprCategory::Not,
                        int_flow: int_flow.concat(if ret_ty.is_never() {
                            InterruptControlFlow::Return
                        } else {
                            InterruptControlFlow::Not
                        }),
                        value: None,
                    }))
                } else {
                    Err(make_semantic_error!(NonFunctionCall))
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
                expr_tys: ty,
                category,
                mut int_flow,
                ..
            }) => {
                no_assignee!(category);
                let params_res = params_res.into_iter().collect::<Option<Vec<_>>>().unwrap();
                for x in &params_res {
                    no_assignee!(x.category);
                }
                let ident = &seg.ident;
                if seg.args.is_some() {
                    return Err(make_semantic_error!(Unimplemented));
                }

                let (fn_ty, deref_level) = self.get_types_items(
                    ty,
                    true, /* 此处保证 items 中均为 Fn Variant 且首个参数为 self */
                    &ident.symbol,
                )?;

                let (required_tys, ret_ty) = fn_ty.as_fn().unwrap();

                if required_tys.len() != params_res.len() + 1 {
                    return Err(make_semantic_error!(MismatchArgNum));
                }

                match required_tys.first().unwrap().as_ref() {
                    ResolvedTy::Ref(_, target_mut) => {
                        let self_mut = get_mutbl!(category).merge_with_deref_level(deref_level);
                        if !self_mut.can_trans_to(target_mut) {
                            return Err(make_semantic_error!(ImmutableVar));
                        }
                    }
                    ResolvedTy::ImplicitSelf => {
                        if matches!(deref_level, DerefLevel::Deref(_)) {
                            return Err(make_semantic_error!(UnDereferenceable));
                        }
                    }
                    _ => panic!("Impossible"),
                }

                for (income, target) in params_res.into_iter().zip(required_tys[1..].iter()) {
                    debug_assert!(!target.is_implicit_self_or_ref_implicit_self());
                    let income_ty = income.expr_tys;
                    int_flow = int_flow.concat(income.int_flow);

                    if !income_ty.can_trans_to_target_type(target) {
                        return Err(make_semantic_error!(TypeMismatch));
                    }
                }

                Ok(Some(ExprResult {
                    expr_tys: ret_ty.clone().into(),
                    category: ExprCategory::Not,
                    int_flow,
                    value: None,
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
                    expr_tys: pool!(self, unit).into(),
                    category: ExprCategory::Place(Mutability::Mut),
                    int_flow: InterruptControlFlow::Not,
                    value: None,
                })
            } else {
                None
            }),
            [e] => self.visit_expr(e),
            _ => Err(make_semantic_error!(Unimplemented)),
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

                // TODO: &1 + &2 之类的
                let ty1 = res1.expr_tys;
                let ty2 = res2.expr_tys;
                let int_flow = res1.int_flow.concat(res2.int_flow);

                let bool_result = Ok(Some(ExprResult {
                    expr_tys: pool!(self, bool).into(),
                    category: ExprCategory::Not,
                    int_flow,
                    value: None,
                }));
                let result_f = |ty: ResolvedTypes| {
                    Ok(Some(ExprResult {
                        expr_tys: ty,
                        category: ExprCategory::Not,
                        int_flow,
                        value: None,
                    }))
                };

                if let Some(uted) = ResolvedTypes::utilize(vec![ty1.clone(), ty2.clone()]) {
                    // Enum
                    if let Some(types) = uted.as_types()
                        && types.len() == 1
                        && let Some(fullname) = types.iter().next().unwrap().as_named()
                        && self.get_type_info(fullname).kind.is_enum()
                    {
                        return Ok(Some(ExprResult {
                            expr_tys: pool!(self, bool).into(),
                            category: ExprCategory::Not,
                            int_flow,
                            value: None,
                        }));
                    };

                    // 比较
                    if matches!(
                        bin_op,
                        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Ge | BinOp::Gt
                    ) && let Some(_) = ResolvedTypes::utilize(vec![
                        uted.clone(),
                        ResolvedTypes::from([
                            pool!(self, i32),
                            pool!(self, u32),
                            pool!(self, isize),
                            pool!(self, usize),
                            pool!(self, bool),
                            pool!(self, char),
                        ]),
                    ]) {
                        return bool_result;
                    }

                    // +-*/%
                    if matches!(
                        bin_op,
                        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem
                    ) && let Some(ret_ty) = ResolvedTypes::utilize(vec![
                        uted.clone(),
                        ResolvedTypes::from([
                            pool!(self, i32),
                            pool!(self, u32),
                            pool!(self, isize),
                            pool!(self, usize),
                        ]),
                    ]) {
                        return result_f(ret_ty);
                    }

                    // && ||
                    if matches!(bin_op, BinOp::And | BinOp::Or)
                        && let Some(ret_ty) = ResolvedTypes::utilize(vec![
                            uted.clone(),
                            ResolvedTypes::from([pool!(self, bool)]),
                        ])
                    {
                        return result_f(ret_ty);
                    }

                    // & | ^
                    if matches!(bin_op, BinOp::BitXor | BinOp::BitAnd | BinOp::BitOr)
                        && let Some(ret_ty) = ResolvedTypes::utilize(vec![
                            uted.clone(),
                            ResolvedTypes::from([
                                pool!(self, i32),
                                pool!(self, u32),
                                pool!(self, isize),
                                pool!(self, usize),
                                pool!(self, bool),
                            ]),
                        ])
                    {
                        return result_f(ret_ty);
                    }
                };

                // String + &str
                if (matches!(bin_op, BinOp::Add))
                    && ty1.can_trans_to_target_type(pool_ref!(self, string))
                    && ty2.can_trans_to_target_type(pool_ref!(self, ref_str))
                {
                    return Ok(Some(ExprResult {
                        expr_tys: pool!(self, string).into(),
                        category: ExprCategory::Not,
                        int_flow,
                        value: None,
                    }));
                };

                // 字符串比较
                if matches!(bin_op, BinOp::Eq | BinOp::Ne)
                    && (ResolvedTypes::utilize(vec![
                        ty1.clone(),
                        ResolvedTypes::from([pool!(self, ref_str), pool!(self, string)]),
                    ])
                    .is_some())
                    && (ResolvedTypes::utilize(vec![
                        ty2.clone(),
                        ResolvedTypes::from([pool!(self, ref_str), pool!(self, string)]),
                    ])
                    .is_some())
                {
                    return bool_result;
                }

                // << >>
                if matches!(bin_op, BinOp::Shl | BinOp::Shr)
                    && let Some(ret_ty) = (ResolvedTypes::utilize(vec![
                        ty1.clone(),
                        ResolvedTypes::from([
                            pool!(self, i32),
                            pool!(self, u32),
                            pool!(self, isize),
                            pool!(self, usize),
                        ]),
                    ]))
                    && (ResolvedTypes::utilize(vec![
                        ty2.clone(),
                        ResolvedTypes::from([
                            pool!(self, i32),
                            pool!(self, u32),
                            pool!(self, isize),
                            pool!(self, usize),
                        ]),
                    ])
                    .is_some())
                {
                    return result_f(ret_ty);
                }

                Err(make_semantic_error!(NoBinaryOperation(
                    *bin_op,
                    ty1.into(),
                    ty2.into()
                )))
            }
            (None, Some(_)) | (Some(_), None) => panic!("Impossible!"),
            (None, None) => Ok(None),
        }
    }

    fn visit_unary_expr(&mut self, expr: &'ast crate::ast::expr::UnaryExpr) -> Self::ExprRes {
        let res = self.visit_expr(&expr.1)?;
        match res {
            Some(ExprResult {
                expr_tys: ty,
                category,
                int_flow,
                value,
            }) => {
                debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                no_assignee!(category);
                match expr.0 {
                    UnOp::Deref => {
                        // TODO: 检测是否实现 Copy Trait

                        // 在 rust 中，貌似任意 value expr 都可以取其 ref，并且再解引用后可以得到 place value
                        if let Some((de, mutbl)) = ty.deref_once() {
                            Ok(Some(ExprResult {
                                expr_tys: de,
                                category: ExprCategory::Place(mutbl),
                                int_flow,
                                value: None,
                            }))
                        } else {
                            Err(make_semantic_error!(UnDereferenceable))
                        }
                    }
                    UnOp::Not => {
                        if ty.is_number_type() || ty.can_trans_to_target_type(pool_ref!(self, bool))
                        {
                            Ok(Some(ExprResult {
                                expr_tys: ty.clone(),
                                category: ExprCategory::Not,
                                int_flow,
                                value: None,
                            }))
                        } else {
                            Err(make_semantic_error!(NoImplementation))
                        }
                    }
                    UnOp::Neg => {
                        Ok(Some(ExprResult {
                            expr_tys: {
                                // 为了正确处理 ±2147483648 的 work around
                                let signed = [pool!(self, i32), pool!(self, isize)].into();
                                let unsigned = [pool!(self, u32), pool!(self, usize)].into();
                                if value == Some(2147483648) && ty == unsigned {
                                    signed
                                } else if value == Some(2147483648) && ty == signed {
                                    unsigned
                                } else if let Some(uted) = ResolvedTypes::utilize(vec![
                                    ty,
                                    ResolvedTypes::Types(HashSet::from([
                                        pool!(self, i32),
                                        pool!(self, isize),
                                    ])),
                                ]) {
                                    uted
                                } else {
                                    return Err(make_semantic_error!(NoImplementation));
                                }
                            },
                            value: value.map(|x| !x + 1),
                            category: ExprCategory::Not,
                            int_flow,
                        }))
                    }
                }
            }
            None => Ok(None),
        }
    }

    fn visit_lit_expr(&mut self, expr: &'ast crate::ast::expr::LitExpr) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => {
                let (expr_tys, value) = match expr.kind {
                    LitKind::Bool => (pool!(self, bool).into(), None),
                    LitKind::Char => (pool!(self, char).into(), None),
                    LitKind::Integer => {
                        let value = expr.to_integer()?;
                        match value {
                            ConstEvalValue::U32(v) => (pool!(self, u32).into(), Some(v)),
                            ConstEvalValue::I32(v) => (pool!(self, i32).into(), Some(v as u32)),
                            ConstEvalValue::USize(v) => (pool!(self, usize).into(), Some(v)),
                            ConstEvalValue::ISize(v) => (pool!(self, usize).into(), Some(v as u32)),
                            ConstEvalValue::Integer(v) => (
                                if v < 2147483648 {
                                    [
                                        pool!(self, u32),
                                        pool!(self, i32),
                                        pool!(self, isize),
                                        pool!(self, usize),
                                    ]
                                    .into()
                                } else {
                                    [pool!(self, u32), pool!(self, usize)].into()
                                },
                                Some(v),
                            ),
                            _ => panic!("Impossible!"),
                        }
                    }
                    LitKind::Str | LitKind::StrRaw(_) => {
                        if expr.suffix.is_none() {
                            (pool!(self, ref_str).into(), None)
                        } else {
                            return Err(make_semantic_error!(UnknownSuffix));
                        }
                    }
                    _ => return Err(make_semantic_error!(Unimplemented)),
                };
                Ok(Some(ExprResult {
                    expr_tys,
                    category: ExprCategory::Not,
                    int_flow: InterruptControlFlow::Not,
                    value,
                }))
            }
        }
    }

    fn visit_cast_expr(&mut self, expr: &'ast crate::ast::expr::CastExpr) -> Self::ExprRes {
        let res = self.visit_expr(&expr.0)?;
        match res {
            Some(ExprResult {
                expr_tys: expr_ty,
                category,
                int_flow,
                ..
            }) => {
                debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                no_assignee!(category);

                let target_ty = self.resolve_ty(&expr.1)?;

                if (expr_ty.is_number_type()
                    || expr_ty.can_trans_to_target_type(pool_ref!(self, char))
                    || expr_ty.can_trans_to_target_type(pool_ref!(self, bool)))
                    && (target_ty == pool!(self, i32)
                        || target_ty == pool!(self, u32)
                        || target_ty == pool!(self, isize)
                        || target_ty == pool!(self, usize))
                {
                    Ok(Some(ExprResult {
                        expr_tys: target_ty.into(),
                        category: ExprCategory::Not,
                        int_flow,
                        value: None,
                    }))
                } else {
                    Err(make_semantic_error!(IncompatibleCast))
                }
            }
            None => Ok(None),
        }
    }

    fn visit_let_expr(&mut self, _: &'ast crate::ast::expr::LetExpr) -> Self::ExprRes {
        Err(make_semantic_error!(Unimplemented))
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

                let con_ty = con_res.expr_tys;
                if !con_ty.can_trans_to_target_type(pool_ref!(self, bool)) {
                    return Err(make_semantic_error!(TypeMismatch));
                }

                if let Some(els_res) = els_res {
                    no_assignee!(els_res.category);
                    Ok(Some(ExprResult {
                        expr_tys: ResolvedTypes::utilize(vec![take_res.expr_tys, els_res.expr_tys])
                            .ok_or(make_semantic_error!(TypeMismatch))?,
                        category: ExprCategory::Not,
                        int_flow: con_res
                            .int_flow
                            .concat(take_res.int_flow.shunt(els_res.int_flow)),
                        value: None,
                    }))
                } else if take_res
                    .expr_tys
                    .can_trans_to_target_type(pool_ref!(self, unit))
                {
                    Ok(Some(self.unit_expr_result_int_specified(con_res.int_flow)))
                } else {
                    Err(make_semantic_error!(TypeMismatch))
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

                let con_ty = con_res.expr_tys;
                if !con_ty.can_trans_to_target_type(pool_ref!(self, bool)) {
                    return Err(make_semantic_error!(TypeMismatch));
                }

                let body_int_flow = match body_res {
                    StmtResult::Expr(expr_result) => {
                        if !expr_result
                            .expr_tys
                            .can_trans_to_target_type(pool_ref!(self, unit))
                        {
                            return Err(make_semantic_error!(TypeMismatch));
                        }
                        expr_result.int_flow
                    }
                    StmtResult::Else { int_flow } => int_flow,
                };

                let out_of_cycle_int_flow = body_int_flow.out_of_cycle();

                Ok(Some(self.unit_expr_result_int_specified(
                    con_res.int_flow.concat(out_of_cycle_int_flow),
                )))
            }
            (None, None) => Ok(None),
            _ => panic!("Impossible"),
        }
    }

    fn visit_for_loop_expr(&mut self, _: &'ast crate::ast::expr::ForLoopExpr) -> Self::ExprRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_loop_expr(&mut self, LoopExpr(block): &'ast LoopExpr) -> Self::ExprRes {
        let res = self.visit_block_expr_with_kind(block, ScopeKind::Loop { ret_ty: None })?;

        match res {
            Some(res) => {
                let int_flow = match res {
                    StmtResult::Expr(expr_result) => {
                        if !expr_result
                            .expr_tys
                            .can_trans_to_target_type(pool_ref!(self, unit))
                        {
                            return Err(make_semantic_error!(TypeMismatch));
                        }
                        expr_result.int_flow
                    }
                    StmtResult::Else { int_flow } => int_flow,
                };

                if int_flow.is_not() {
                    Ok(Some(self.never_expr_result_int_specified(
                        InterruptControlFlow::Return,
                    )))
                } else {
                    let out_of_cycle_int_flow = int_flow.out_of_cycle();

                    Ok(Some(ExprResult {
                        expr_tys: self
                            .get_scope_by_id(block.id)
                            .kind
                            .as_loop()
                            .unwrap()
                            .clone()
                            .unwrap_or(pool!(self, never).into()),
                        category: ExprCategory::Not,
                        int_flow: out_of_cycle_int_flow,
                        value: None,
                    }))
                }
            }
            None => Ok(None),
        }
    }

    fn visit_match_expr(&mut self, _: &'ast crate::ast::expr::MatchExpr) -> Self::ExprRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_block_expr(&mut self, expr: &'ast crate::ast::expr::BlockExpr) -> Self::ExprRes {
        self.visit_block_expr_with_kind(expr, ScopeKind::Lambda)
            .map(|x| match x {
                Some(StmtResult::Expr(expr_result)) => Some(expr_result),
                Some(StmtResult::Else { int_flow }) => Some(if int_flow.is_not() {
                    self.unit_expr_result_int_specified(InterruptControlFlow::Not)
                } else {
                    self.never_expr_result_int_specified(int_flow)
                }),
                None => None,
            })
    }

    fn visit_assign_expr(&mut self, AssignExpr(left, right): &'ast AssignExpr) -> Self::ExprRes {
        let right_res = self.visit_expr(right)?;
        let left_res = self.visit_expr(left)?;

        match (left_res, right_res) {
            (None, None) => Ok(None),
            (None, Some(_)) | (Some(_), None) => panic!("Impossible"),
            (Some(left_res), Some(right_res)) => {
                match left_res.category {
                    ExprCategory::Place(Mutability::Not) => {
                        return Err(make_semantic_error!(ImmutableVar));
                    }
                    ExprCategory::Not => return Err(make_semantic_error!(NonAssigneeExpr)),
                    ExprCategory::Only | ExprCategory::Place(Mutability::Mut) => {}
                }
                no_assignee!(right_res.category);

                let left_ty = left_res.expr_tys;
                let right_ty = right_res.expr_tys;
                if ResolvedTypes::utilize(vec![left_ty, right_ty]).is_none() {
                    return Err(make_semantic_error!(TypeMismatch));
                }

                Ok(Some(self.unit_expr_result_int_specified(
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
                        return Err(make_semantic_error!(ImmutableVar));
                    }
                    ExprCategory::Only | ExprCategory::Not => {
                        return Err(make_semantic_error!(NonPlaceExpr));
                    }
                    ExprCategory::Place(Mutability::Mut) => {}
                }
                no_assignee!(right_res.category);

                let result = Ok(Some(self.unit_expr_result_int_specified(
                    right_res.int_flow.concat(left_res.int_flow),
                )));

                let left_ty = left_res.expr_tys;
                let right_ty = right_res.expr_tys;

                if let Some(uted) = ResolvedTypes::utilize(vec![left_ty.clone(), right_ty.clone()])
                {
                    // +-*/%
                    if matches!(
                        op,
                        AssignOp::AddAssign
                            | AssignOp::SubAssign
                            | AssignOp::MulAssign
                            | AssignOp::DivAssign
                            | AssignOp::RemAssign
                    ) && let Some(_) = ResolvedTypes::utilize(vec![
                        uted.clone(),
                        ResolvedTypes::from([
                            pool!(self, i32),
                            pool!(self, u32),
                            pool!(self, isize),
                            pool!(self, usize),
                        ]),
                    ]) {
                        return result;
                    }

                    // & | ^
                    if matches!(
                        op,
                        AssignOp::BitXorAssign | AssignOp::BitAndAssign | AssignOp::BitOrAssign
                    ) && let Some(_) = ResolvedTypes::utilize(vec![
                        uted.clone(),
                        ResolvedTypes::from([
                            pool!(self, i32),
                            pool!(self, u32),
                            pool!(self, isize),
                            pool!(self, usize),
                            pool!(self, bool),
                        ]),
                    ]) {
                        return result;
                    }
                }

                // String += &str
                if (matches!(op, AssignOp::AddAssign))
                    && left_ty.can_trans_to_target_type(pool_ref!(self, string))
                    && right_ty.can_trans_to_target_type(pool_ref!(self, ref_str))
                {
                    return result;
                };

                // << >>
                if matches!(op, AssignOp::ShlAssign | AssignOp::ShrAssign)
                    && let Some(_) = (ResolvedTypes::utilize(vec![
                        left_ty.clone(),
                        ResolvedTypes::from([
                            pool!(self, i32),
                            pool!(self, u32),
                            pool!(self, isize),
                            pool!(self, usize),
                        ]),
                    ]))
                    && (ResolvedTypes::utilize(vec![
                        right_ty.clone(),
                        ResolvedTypes::from([
                            pool!(self, i32),
                            pool!(self, u32),
                            pool!(self, isize),
                            pool!(self, usize),
                        ]),
                    ])
                    .is_some())
                {
                    return result;
                }

                Err(make_semantic_error!(Unimplemented))
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
                let (field, deref_level) = self.get_types_fields(res.expr_tys, &ident.symbol)?;

                Ok(Some(ExprResult {
                    expr_tys: field.ty.into(),
                    category: ExprCategory::Place(res_mut.merge_with_deref_level(deref_level)),
                    int_flow: res.int_flow,
                    value: None,
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
                    ExprCategory::Only => return Err(make_semantic_error!(AssigneeOnlyExpr)),
                };
                no_assignee!(res1.category);
                no_assignee!(res2.category);

                let ty1 = res1.expr_tys;
                let ty2 = res2.expr_tys;
                if !ty2.can_trans_to_target_type(&pool!(self, usize)) {
                    return Err(make_semantic_error!(TypeMismatch));
                }

                if let Some((ele_ty, level)) = ty1.out_of_array() {
                    Ok(Some(ExprResult {
                        expr_tys: ele_ty.clone(),
                        category: ExprCategory::Place(match level {
                            DerefLevel::Not => res1_mut,
                            DerefLevel::Deref(mutability) => mutability,
                        }),
                        int_flow: res1.int_flow.concat(res2.int_flow),
                        value: None,
                    }))
                } else {
                    Err(make_semantic_error!(TypeMismatch))
                }
            }
            (None, None) => Ok(None),
            (None, Some(_)) | (Some(_), None) => panic!("Impossible"),
        }
    }

    fn visit_range_expr(&mut self, _: &'ast crate::ast::expr::RangeExpr) -> Self::ExprRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_underscore_expr(
        &mut self,
        _: &'ast crate::ast::expr::UnderscoreExpr,
    ) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => Ok(Some(ExprResult {
                expr_tys: ResolvedTypes::Infer,
                category: ExprCategory::Only,
                int_flow: InterruptControlFlow::Not,
                value: None,
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
                            let evaled_value =
                                self.const_eval(ty.clone(), item.expr.as_ref().unwrap())?;

                            let var = self.search_value_mut(&item.ident.symbol)?;

                            assert!(std::ptr::eq(var_ptr, &raw const *var));

                            *var = Variable {
                                ty,
                                mutbl: crate::ast::Mutability::Not,
                                kind: VariableKind::Constant(evaled_value.clone()),
                            };

                            self.state = old_state;

                            unsafe { &*var_ptr }
                        }
                        _ => variable,
                    };

                    Ok(Some(ExprResult {
                        expr_tys: variable.ty.clone().into(),
                        category: ExprCategory::Place(variable.mutbl),
                        int_flow: InterruptControlFlow::Not,
                        value: None,
                    }))
                }
                ValueContainer::ImplInfoItem(ty, _) => Ok(Some(ExprResult {
                    expr_tys: ty.into(),
                    category: ExprCategory::Place(Mutability::Not),
                    int_flow: InterruptControlFlow::Not,
                    value: None,
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
                        expr_tys: variable.ty.into(),
                        category: ExprCategory::Place(variable.mutbl),
                        int_flow: InterruptControlFlow::Not,
                        value: None,
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
                expr_tys: ty,
                category,
                int_flow,
                ..
            }) => {
                no_assignee!(category);
                let ret_ty = ResolvedTypes::Ref(ty.into(), expr.0);
                Ok(Some(ExprResult {
                    expr_tys: ret_ty,
                    category: ExprCategory::Not,
                    int_flow,
                    value: None,
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
                let unit = pool!(self, unit);

                let target_scope = self.get_cycle_scope_mut()?;

                let mut int_flow = InterruptControlFlow::Not;

                match &mut target_scope.kind {
                    ScopeKind::Loop { ret_ty } => match (res, &ret_ty) {
                        (None, None) => {}
                        (None, Some(exist)) => {
                            if !exist.can_trans_to_target_type(&unit) {
                                return Err(make_semantic_error!(TypeMismatch));
                            }
                        }
                        (Some(new), None) => {
                            no_assignee!(new.category);
                            int_flow = new.int_flow;
                            *ret_ty = Some(new.expr_tys)
                        }
                        (Some(new), Some(exist)) => {
                            no_assignee!(new.category);
                            int_flow = new.int_flow;

                            let ty = ResolvedTypes::utilize(vec![exist.clone(), new.expr_tys])
                                .ok_or(make_semantic_error!(TypeMismatch))?;
                            *ret_ty = Some(ty)
                        }
                    },
                    ScopeKind::CycleExceptLoop => {
                        if expr.0.is_some() {
                            return Err(make_semantic_error!(BreakWithValue));
                        }
                    }
                    _ => panic!("Impossible"),
                }

                Ok(Some(self.never_expr_result_int_specified(
                    int_flow.concat(InterruptControlFlow::Loop),
                )))
            }
        }
    }

    fn visit_continue_expr(&mut self, _: &'ast crate::ast::expr::ContinueExpr) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition | AnalyzeStage::Impl => Ok(None),
            AnalyzeStage::Body => {
                self.get_cycle_scope_mut()?;
                Ok(Some(self.never_expr_result_int_specified(
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
                AnalyzeStage::Body => Some(self.unit_expr_result()),
                _ => None,
            }
        };

        match res {
            Some(res) => {
                no_assignee!(res.category);
                let scope = self.get_fn_scope()?;
                let target_ty = scope.kind.as_fn().unwrap().0;

                let ret_ty = res.expr_tys;

                if !ret_ty.can_trans_to_target_type(target_ty) {
                    Err(make_semantic_error!(TypeMismatch))
                } else {
                    Ok(Some(self.never_expr_result_int_specified(
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
            return Err(make_semantic_error!(Unimplemented));
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
                            |x, (_, ExprResult { int_flow: y, .. })| x.concat(*y),
                        );

                        let mut dic: HashMap<Symbol, ExprResult> = HashMap::new();

                        for x in exp_fields.into_iter() {
                            if dic.insert(x.0, x.1).is_some() {
                                return Err(make_semantic_error!(MultiSpecifiedField));
                            }
                        }

                        if dic.len() > fields.len() {
                            return Err(make_semantic_error!(UnknownField));
                        }

                        for (field_ident, field_type) in fields {
                            let Some(res) = dic.get(field_ident) else {
                                return Err(make_semantic_error!(MissingField));
                            };
                            let res_ty = &res.expr_tys;
                            let field_ty = field_type;
                            if !res_ty.can_trans_to_target_type(field_ty) {
                                return Err(make_semantic_error!(TypeMismatch));
                            }
                        }

                        Ok(Some(ExprResult {
                            expr_tys: TypePtr::from(
                                self.resolve_ty_in_scope_by_symbol(&struct_info.name, id),
                            )
                            .into(),
                            category,
                            int_flow,
                            value: None,
                        }))
                    }

                    TypeKind::Enum { fields: _ }
                    | TypeKind::Trait {
                        methods: _,
                        constants: _,
                    } => Err(make_semantic_error!(NotStructType)),
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
                let size = self.const_eval(pool!(self, usize), &const_expr.value)?;
                no_assignee!(res.category);

                Ok(Some(ExprResult {
                    expr_tys: ResolvedTypes::Array(
                        res.expr_tys.into(),
                        size.into_u_size().unwrap(),
                    ),
                    category: ExprCategory::Not,
                    int_flow: res.int_flow,
                    value: None,
                }))
            }
            None => Ok(None),
        }
    }

    fn visit_wild_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::WildPat,
        _expected_ty: TypePtr,
    ) -> Self::PatRes {
        Ok(PatResult {
            bindings: Vec::new(),
        })
    }

    fn visit_ident_pat(
        &mut self,
        IdentPat(mode, ident, guarder): &'ast IdentPat,
        expected_ty: TypePtr,
    ) -> Self::PatRes {
        if guarder.is_some() {
            return Err(make_semantic_error!(Unimplemented));
        }

        let (target_ty, mutbl) = match mode.0 {
            crate::ast::ByRef::Yes(mutability) => {
                let t = ResolvedTy::Ref(expected_ty.clone(), mutability);
                (t.into(), Mutability::Not)
            }
            crate::ast::ByRef::No => (expected_ty, mode.1),
        };

        let bindings = vec![(ident.symbol.clone(), target_ty, mutbl)];

        Ok(PatResult { bindings })
    }

    fn visit_struct_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::StructPat,
        _expected_ty: TypePtr,
    ) -> Self::PatRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_or_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::OrPat,
        _expected_ty: TypePtr,
    ) -> Self::PatRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_path_pat(
        &mut self,
        PathPat(qself, path): &'ast PathPat,
        expected_ty: TypePtr,
    ) -> Self::PatRes {
        if let Ok(var) = self.search_value_by_path(qself, path)
            && let ValueContainer::Variable(var) = var
            && var.kind.is_constant()
        {
            return Err(make_semantic_error!(Unimplemented));
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
        _expected_ty: TypePtr,
    ) -> Self::PatRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_ref_pat(
        &mut self,
        RefPat(pat, mutbl): &'ast RefPat,
        expected_ty: TypePtr,
    ) -> Self::PatRes {
        match expected_ty.as_ref() {
            ResolvedTy::Ref(resolved_ty, mutability) => {
                if mutbl == mutability {
                    self.visit_pat(pat, resolved_ty.clone())
                } else {
                    Err(make_semantic_error!(PatMismatch))
                }
            }
            _ => Err(make_semantic_error!(PatMismatch)),
        }
    }

    fn visit_lit_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::LitPat,
        _expected_ty: TypePtr,
    ) -> Self::PatRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_range_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::RangePat,
        _expected_ty: TypePtr,
    ) -> Self::PatRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_slice_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::SlicePat,
        _expected_ty: TypePtr,
    ) -> Self::PatRes {
        Err(make_semantic_error!(Unimplemented))
    }

    fn visit_rest_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::RestPat,
        _expected_ty: TypePtr,
    ) -> Self::PatRes {
        Err(make_semantic_error!(Unimplemented))
    }
}
