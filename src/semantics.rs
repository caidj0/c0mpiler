pub mod const_eval;
pub mod primitives;
pub mod visitor;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    vec,
};

use crate::{
    ast::{
        Crate, Ident, Mutability, NodeId, Symbol,
        expr::{
            BinOp, BinaryExpr, BlockExpr, CallExpr, Expr, FieldExpr, IfExpr, LitKind, LoopExpr,
            MethodCallExpr, PathExpr, RepeatExpr, RetExpr, StructExpr, StructRest, UnOp, WhileExpr,
        },
        item::{
            AssocItemKind, ConstItem, EnumItem, FnItem, FnRetTy, ImplItem, Item, ItemKind,
            StructItem, TraitItem,
        },
        pat::IdentPat,
        path::{Path, PathSegment},
        stmt::{LocalKind, StmtKind},
        ty::{MutTy, PathTy, RefTy, Ty, TyKind},
    },
    semantics::{const_eval::ConstEvalError, visitor::Visitor},
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

#[derive(Debug)]
pub enum SemanticError {
    Unimplemented,
    UndefinedScope,
    MultiDefined,
    ConstEvalError(ConstEvalError),
    InvaildPath,
    UnknownType,
    UnknownVariable,
    UnknownField,
    InvaildScope,
    FnWithoutBody,
    UnknownSuffix,
    NoImplementation,
    UnDereferenceable,
    IncompatibleCast,
    AssigneeOnlyExpr,
    TypeMismatch,
    ConflictAssignee,
    NonFunctionCall,
    NonMethodCall,
    MismatchArgNum,
    ImmutableVar,
    NoLoopScope,
    BreakWithValue,
    NoFnScope,
    NotStructType,
    MultiSpecifiedField,
    NonProvidedField,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedTy {
    BulitIn(Symbol, Vec<ResolvedTy>),
    Named(Vec<Symbol>),
    Ref(Box<ResolvedTy>, Mutability),
    Array(Box<ResolvedTy>, u32),
    Slice(Box<ResolvedTy>),
    Tup(Vec<ResolvedTy>),
    Fn(Vec<ResolvedTy>, Box<ResolvedTy>),
    Infer,
    ImplicitSelf,
    Any, // for underscore
}

impl ResolvedTy {
    pub fn unit() -> Self {
        Self::Tup(Vec::new())
    }

    pub fn bool() -> Self {
        Self::BulitIn(Symbol("bool".to_string()), Vec::new())
    }

    pub fn char() -> Self {
        Self::BulitIn(Symbol("char".to_string()), Vec::new())
    }

    pub fn integer() -> Self {
        Self::BulitIn(Symbol("integer".to_string()), Vec::new())
    }

    pub fn i32() -> Self {
        Self::BulitIn(Symbol("i32".to_string()), Vec::new())
    }

    pub fn u32() -> Self {
        Self::BulitIn(Symbol("u32".to_string()), Vec::new())
    }

    pub fn isize() -> Self {
        Self::BulitIn(Symbol("isize".to_string()), Vec::new())
    }

    pub fn usize() -> Self {
        Self::BulitIn(Symbol("usize".to_string()), Vec::new())
    }

    pub fn str() -> Self {
        Self::BulitIn(Symbol("str".to_string()), Vec::new())
    }

    pub fn ref_str() -> Self {
        Self::Ref(Box::new(Self::str()), Mutability::Not)
    }

    pub fn string() -> Self {
        Self::BulitIn(Symbol("String".to_string()), Vec::new())
    }

    pub fn implicit_self() -> Self {
        Self::ImplicitSelf
    }

    pub fn ref_implicit_self() -> Self {
        Self::Ref(Box::new(Self::implicit_self()), Mutability::Not)
    }

    pub fn ref_mut_implicit_self() -> Self {
        Self::Ref(Box::new(Self::implicit_self()), Mutability::Mut)
    }

    pub fn try_deref(self) -> (Self, Mutability) {
        if let ResolvedTy::Ref(resolved, mutbl) = self {
            (*resolved, mutbl)
        } else {
            (self, Mutability::Mut)
        }
    }

    pub fn deref_all(self) -> (Self, Mutability) {
        let mut ret_ty = self;
        let mut ret_mut = Mutability::Mut;

        while let ResolvedTy::Ref(resolved, mutbl) = ret_ty {
            ret_ty = *resolved;
            ret_mut = ret_mut.merge(mutbl)
        }

        (ret_ty, ret_mut)
    }

    pub fn is_number_type(&self) -> bool {
        *self == Self::integer()
            || *self == Self::i32()
            || *self == Self::u32()
            || *self == Self::usize()
            || *self == Self::isize()
    }

    pub fn is_implicit_self(&self) -> bool {
        match self {
            ResolvedTy::Ref(resolved_ty, _) => {
                matches!(resolved_ty.as_ref(), ResolvedTy::ImplicitSelf)
            }
            ResolvedTy::ImplicitSelf => true,
            _ => false,
        }
    }

    pub fn is_method(&self) -> bool {
        match self {
            ResolvedTy::Fn(tys, _) => match tys.first() {
                Some(ResolvedTy::ImplicitSelf) => true,
                Some(ResolvedTy::Ref(inref, _)) => {
                    matches!(inref.as_ref(), ResolvedTy::ImplicitSelf)
                }
                _ => false,
            },
            _ => false,
        }
    }

    pub fn method_to_func(&self, self_ty: &Self) -> Self {
        let mut ret = self.clone();
        let ResolvedTy::Fn(tys, _) = &mut ret else {
            panic!("Impossible!");
        };

        let first = tys.first_mut().unwrap();

        match first {
            ResolvedTy::Ref(resolved_ty, _) => {
                debug_assert!(matches!(resolved_ty.as_ref(), ResolvedTy::ImplicitSelf));
                *resolved_ty = Box::new(self_ty.clone())
            }
            ResolvedTy::ImplicitSelf => *first = self_ty.clone(),
            _ => panic!("Impossible!"),
        }

        ret
    }

    pub fn into_ref(self) -> Self {
        Self::Ref(Box::new(self), Mutability::Not)
    }

    // 包括相等类型 + 其他情况
    pub fn can_trans_to_target_type(&self, target: &Self) -> bool {
        if self == target {
            return true;
        }

        match (self, target) {
            (ResolvedTy::BulitIn(symbol1, args1), ResolvedTy::BulitIn(symbol2, args2)) => {
                let arg_same = args1.iter().zip(args2.iter()).all(|(x, y)| x == y);
                if arg_same && symbol1 == symbol2 {
                    true
                } else {
                    if *self == ResolvedTy::integer() && target.is_number_type() {
                        true
                    } else {
                        false
                    }
                }
            }
            (ResolvedTy::Named(_), ResolvedTy::Named(_)) => false,
            (
                ResolvedTy::Ref(resolved_ty1, mutability1),
                ResolvedTy::Ref(resolved_ty2, mutability2),
            ) => match (mutability1, mutability2) {
                (Mutability::Not, Mutability::Mut) => false,
                _ => resolved_ty1.can_trans_to_target_type(&resolved_ty2),
            },
            (ResolvedTy::Array(resolved_ty1, len1), ResolvedTy::Array(resolved_ty2, len2)) => {
                if len1 == len2 {
                    resolved_ty1.can_trans_to_target_type(&resolved_ty2)
                } else {
                    false
                }
            }
            (ResolvedTy::Slice(resolved_ty1), ResolvedTy::Slice(resolved_ty2)) => {
                resolved_ty1.can_trans_to_target_type(&resolved_ty2)
            }
            (ResolvedTy::Tup(items1), ResolvedTy::Tup(items2)) => {
                if items1.len() != items2.len() {
                    return false;
                }
                for (x, y) in items1.iter().zip(items2.iter()) {
                    if !x.can_trans_to_target_type(y) {
                        return false;
                    }
                }
                true
            }
            (ResolvedTy::Fn(_, _), ResolvedTy::Fn(_, _)) => {
                unimplemented!()
            }
            (ResolvedTy::Infer, ResolvedTy::Infer) => {
                unimplemented!()
            }
            (ResolvedTy::ImplicitSelf, ResolvedTy::ImplicitSelf) => {
                panic!("Impossible")
            }
            (ResolvedTy::Any, _) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Default)]
pub struct TypeTable {
    entries: Vec<Box<ResolvedTy>>,
    map: HashMap<ResolvedTy, TypeId>,
}

impl TypeTable {
    pub fn intern(&mut self, ty: ResolvedTy) -> TypeId {
        if let Some(&id) = self.map.get(&ty) {
            return id;
        }
        let id = TypeId(self.entries.len());
        self.entries.push(Box::new(ty.clone()));
        self.map.insert(ty, id);
        id
    }

    pub fn get(&self, id: TypeId) -> &ResolvedTy {
        &self.entries[id.0]
    }
}

#[derive(Debug)]
pub struct TypeInfo {
    pub name: Symbol,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub struct ImplInfo {
    pub self_ty: TypeId,
    pub for_trait: Option<Vec<Symbol>>,
    pub methods: Vec<FnSig>,
    pub constants: Vec<Constant>,
}

#[derive(Debug)]
pub struct FnSig {
    pub name: Symbol,
    pub type_id: TypeId,
    pub is_placeholder: bool,
}

#[derive(Debug)]
pub struct Constant {
    pub name: Symbol,
    pub ty: TypeId,
    pub is_placeholder: bool,
}

#[derive(Debug)]
pub enum TypeKind {
    Placeholder,
    Struct {
        fields: Vec<(Ident, TypeId)>,
    },
    Enum {
        fields: Vec<Ident>,
    },
    Trait {
        methods: Vec<FnSig>,
        constants: Vec<Constant>,
    },
}

#[derive(Debug)]
pub enum VariableKind {
    Decl,
    Inited,
    Const,
}

#[derive(Debug)]
pub struct Variable {
    pub ty: TypeId,
    pub mutbl: Mutability,
    pub kind: VariableKind,
}

#[derive(Debug)]
pub enum ScopeKind {
    Lambda,
    Root,
    Trait(TypeId),
    Impl(TypeId),
    Fn { ret_ty: TypeId },
    Loop { ret_ty: Option<TypeId> },
    CycleExceptLoop,
}

#[derive(Debug)]
pub struct Scope {
    pub id: NodeId,
    pub kind: ScopeKind,
    pub types: HashMap<Symbol, TypeInfo>,
    pub values: HashMap<Symbol, Variable>,
    pub children: HashSet<NodeId>,
    pub father: NodeId,
}

#[derive(Debug)]
pub enum ExprCategory {
    Place(Mutability),
    Not, // 也就是 Value Expr
    Only,
}

#[derive(Debug)]
pub struct ExprResult {
    pub type_id: TypeId,
    pub category: ExprCategory,
}

#[derive(Debug)]
pub struct PatResult {
    pub bindings: Vec<(Symbol, TypeId, Mutability)>,
}

#[derive(Debug)]
pub enum AnalyzeStage {
    SymbolCollect,
    Definition,
    Body,
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    type_table: RefCell<TypeTable>,
    impls: HashMap<TypeId, Vec<ImplInfo>>,
    scopes: HashMap<NodeId, Scope>,
    current_scope: NodeId,
    stage: AnalyzeStage,
    current_ast_id: NodeId,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut type_table: TypeTable = TypeTable::default();
        type_table.intern(ResolvedTy::unit()); // Unit -> 0
        type_table.intern(ResolvedTy::Any); // Any -> 1

        Self {
            type_table: RefCell::new(type_table),
            impls: HashMap::default(),
            scopes: HashMap::default(),
            current_scope: 0,
            stage: AnalyzeStage::SymbolCollect,
            current_ast_id: 0,
        }
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
                ScopeKind::Root | ScopeKind::Trait(_) | ScopeKind::Impl(_) => {
                    return Err(SemanticError::NoFnScope);
                }
                ScopeKind::Fn { ret_ty: _ } => return Ok(scope),
            }
            id = scope.father;
        }
    }

    fn add_scope(&mut self, id: NodeId, kind: ScopeKind) -> Result<(), SemanticError> {
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

    fn enter_scope(&mut self, id: NodeId) -> Result<(), SemanticError> {
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

        self.current_scope = self.get_scope().id;

        Ok(())
    }

    fn add_type(&mut self, ident: Symbol, info: TypeInfo) -> Result<TypeId, SemanticError> {
        let s = self.get_scope_mut();

        if s.types.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.types.insert(ident.clone(), info);

        let mut full_name = self.get_prefix_name();
        full_name.push(ident);
        let resolved = ResolvedTy::Named(full_name);

        Ok(self.intern_type(resolved))
    }

    fn add_value(&mut self, ident: Symbol, var: Variable) -> Result<(), SemanticError> {
        let s = self.get_scope_mut();

        s.values.insert(ident, var); // 允许变量遮蔽

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

            if scope.types.contains_key(ident) {
                return Ok((id, scope.types.get(ident).unwrap()));
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
    ) -> Result<(NodeId, &Variable), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if scope.types.contains_key(ident) {
                return Ok((id, scope.values.get(ident).unwrap()));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownVariable);
            } else {
                id = scope.father;
            }
        }
    }

    fn search_value_from_mut(
        &mut self,
        ident: &Symbol,
        mut id: NodeId,
    ) -> Result<(NodeId, &mut Variable), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get_mut(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if scope.types.contains_key(ident) {
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
            }
        }
    }

    fn get_builtin_methods(&self, ty: &ResolvedTy, is_methods_call: bool) -> Vec<(Symbol, TypeId)> {
        let mut ret = Vec::new();

        let ref_implicit_self = if is_methods_call {
            ResolvedTy::ref_implicit_self()
        } else {
            ty.clone().into_ref()
        };

        if *ty == ResolvedTy::u32() || *ty == ResolvedTy::usize() {
            ret.push((
                Symbol("to_string".to_string()),
                self.intern_type(ResolvedTy::Fn(
                    vec![ref_implicit_self.clone()],
                    Box::new(ResolvedTy::string()),
                )),
            ));
        }

        if *ty == ResolvedTy::string() {
            ret.push((
                Symbol("as_str".to_string()),
                self.intern_type(ResolvedTy::Fn(
                    vec![ref_implicit_self.clone()],
                    Box::new(ResolvedTy::ref_str()),
                )),
            ))
        }

        if *ty == ResolvedTy::string() || *ty == ResolvedTy::str() {
            ret.push((
                Symbol("len".to_string()),
                self.intern_type(ResolvedTy::Fn(
                    vec![ref_implicit_self.clone()],
                    Box::new(ResolvedTy::u32()),
                )),
            ))
        }

        ret
    }

    // fields 不包括 method
    fn get_type_fields(&self, id: TypeId) -> Result<Vec<(Symbol, Variable)>, SemanticError> {
        let ty = self.get_type_by_id(id);

        let mut ret = Vec::new();

        match &ty {
            ResolvedTy::BulitIn(_, _) => {}
            ResolvedTy::Named(symbols) => {
                let info = self.get_type_info(symbols);
                match &info.kind {
                    TypeKind::Placeholder => panic!("Impossible!"),
                    TypeKind::Struct { fields } => {
                        for (ident, field_id) in fields {
                            ret.push((
                                ident.symbol.clone(),
                                Variable {
                                    ty: *field_id,
                                    mutbl: Mutability::Mut,
                                    kind: VariableKind::Inited,
                                },
                            ));
                        }
                    }
                    TypeKind::Enum { fields: _ } => {}
                    TypeKind::Trait {
                        methods: _,
                        constants: _,
                    } => {}
                }
            }
            ResolvedTy::Ref(resolved_ty, _) => {
                let derefed_id = self.intern_type(resolved_ty.as_ref().clone());
                ret.append(&mut self.get_type_fields(derefed_id)?);
                // 会不断尝试 deref
            }
            ResolvedTy::Array(_, _) | ResolvedTy::Slice(_) => {
                ret.push((
                    Symbol("len".to_string()),
                    Variable {
                        ty: self.intern_type(ResolvedTy::Fn(
                            vec![ResolvedTy::ref_implicit_self()],
                            Box::new(ResolvedTy::u32()),
                        )),
                        mutbl: Mutability::Not,
                        kind: VariableKind::Const,
                    },
                ));
            }
            ResolvedTy::ImplicitSelf => {
                ret.append(&mut self.get_type_fields(self.get_self_type()?)?);
            }
            ResolvedTy::Tup(_) | ResolvedTy::Fn(_, _) | ResolvedTy::Infer | ResolvedTy::Any => {}
        }

        Ok(ret)
    }

    // item 包含 method，
    // TODO：处理不同 Trait 重名的情况
    fn get_type_items(
        &self,
        id: TypeId,
        is_methods_call: bool,
    ) -> Result<Vec<(Symbol, TypeId)>, SemanticError> {
        let ty = self.get_type_by_id(id).clone();

        let mut ret = Vec::new();

        match &ty {
            ResolvedTy::BulitIn(_, _) => {
                let ty = ty.clone();
                ret.append(&mut self.get_builtin_methods(&ty, is_methods_call));
            }
            ResolvedTy::Named(symbols) => {
                let info = self.get_type_info(symbols);
                match &info.kind {
                    TypeKind::Placeholder => panic!("Impossible!"),
                    TypeKind::Struct { fields: _ } => {}
                    TypeKind::Enum { fields } => {
                        if !is_methods_call {
                            for ident in fields {
                                ret.push((ident.symbol.clone(), id));
                            }
                        }
                    }
                    TypeKind::Trait { methods, constants } => {
                        self.add_fn_item_to_vec(&mut ret, methods, &ty, is_methods_call);
                        if !is_methods_call {
                            self.add_const_item_to_vec(&mut ret, constants);
                        }
                    }
                }
            }
            ResolvedTy::Array(_, _) | ResolvedTy::Slice(_) => {
                ret.push((
                    Symbol("len".to_string()),
                    self.intern_type(ResolvedTy::Fn(
                        vec![if is_methods_call {
                            ResolvedTy::ref_implicit_self()
                        } else {
                            ty.clone().into_ref()
                        }],
                        Box::new(ResolvedTy::u32()),
                    )),
                ));
            }
            ResolvedTy::ImplicitSelf => {
                ret.append(&mut self.get_type_items(self.get_self_type()?, is_methods_call)?);
            }
            ResolvedTy::Ref(_, _)
            | ResolvedTy::Tup(_)
            | ResolvedTy::Fn(_, _)
            | ResolvedTy::Infer
            | ResolvedTy::Any => {}
        }

        if let Some(impls) = self.impls.get(&id) {
            for x in impls {
                debug_assert_eq!(x.self_ty, id);
                self.add_fn_item_to_vec(&mut ret, &x.methods, &ty, is_methods_call);
                if !is_methods_call {
                    self.add_const_item_to_vec(&mut ret, &x.constants);
                }
            }
        }

        Ok(ret)
    }

    fn add_fn_item_to_vec(
        &self,
        ret: &mut Vec<(Symbol, TypeId)>,
        methods: &[FnSig],
        self_ty: &ResolvedTy,
        only_methods: bool,
    ) {
        for method in methods {
            let ty = self.get_type_by_id(method.type_id);
            match (only_methods, ty.is_method()) {
                (true, true) | (false, false) => ret.push((method.name.clone(), method.type_id)),
                (true, false) => {}
                (false, true) => ret.push((
                    method.name.clone(),
                    self.intern_type(ty.method_to_func(self_ty)),
                )),
            }
        }
    }

    fn add_const_item_to_vec(&self, ret: &mut Vec<(Symbol, TypeId)>, constants: &[Constant]) {
        for constant in constants {
            ret.push((constant.name.clone(), constant.ty));
        }
    }

    fn search_type_mut(
        &mut self,
        ident: &Symbol,
    ) -> Result<(NodeId, &mut TypeInfo), SemanticError> {
        self.search_type_from_mut(ident, self.current_scope)
    }

    fn search_type(&self, ident: &Symbol) -> Result<(NodeId, &TypeInfo), SemanticError> {
        self.search_type_from(ident, self.current_scope)
    }

    fn search_value_mut(
        &mut self,
        ident: &Symbol,
    ) -> Result<(NodeId, &mut Variable), SemanticError> {
        self.search_value_from_mut(ident, self.current_scope)
    }

    // 返回值为 (Scope Id, 变量类型)
    fn search_value(&self, ident: &Symbol) -> Result<(NodeId, &Variable), SemanticError> {
        self.search_value_from(ident, self.current_scope)
    }

    fn get_type_info(&self, symbols: &[Symbol]) -> &TypeInfo {
        debug_assert!(symbols.len() >= 2);
        let scope_symbol = &symbols[symbols.len() - 2];
        let scope_id: NodeId = scope_symbol.0.strip_prefix("$").unwrap().parse().unwrap();
        let scope = self.scopes.get(&scope_id).unwrap();
        scope.types.get(symbols.last().unwrap()).unwrap()
    }

    pub fn visit(&mut self, krate: &crate::ast::Crate) -> Result<(), SemanticError> {
        self.stage = AnalyzeStage::SymbolCollect;
        self.visit_crate(krate)?;
        self.stage = AnalyzeStage::Definition;
        self.visit_crate(krate)?;
        Ok(())
    }

    fn get_prefix_name_from(&self, mut id: NodeId) -> Vec<Symbol> {
        let mut ret = Vec::new();
        loop {
            let scope = self.scopes.get(&id).unwrap();
            ret.push(Symbol(format!("${}", scope.id)));
            if matches!(scope.kind, ScopeKind::Root) {
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
                ScopeKind::Root => return Err(SemanticError::UnknownType),
                ScopeKind::Trait(type_id) | ScopeKind::Impl(type_id) => return Ok(type_id),
                _ => id = scope.father,
            }
        }
    }

    fn get_self_type(&self) -> Result<TypeId, SemanticError> {
        self.get_self_type_from(self.current_scope)
    }

    fn resolve_implicit_self<'a>(&self, ty: &ResolvedTy) -> Result<ResolvedTy, SemanticError> {
        match ty {
            ResolvedTy::Ref(resolved_ty, mutability) => {
                if matches!(resolved_ty.as_ref(), ResolvedTy::ImplicitSelf) {
                    Ok(ResolvedTy::Ref(
                        Box::new(self.get_type_by_id(self.get_self_type()?).clone()),
                        *mutability,
                    ))
                } else {
                    Err(SemanticError::TypeMismatch)
                }
            }
            ResolvedTy::ImplicitSelf => Ok(self.get_type_by_id(self.get_self_type()?).clone()),
            _ => Err(SemanticError::TypeMismatch),
        }
    }

    // 这个函数不会展开隐式 self（为了保留函数参数中的 self）
    fn resolve_ty(&self, ty: &Ty) -> Result<ResolvedTy, SemanticError> {
        match &ty.kind {
            TyKind::Slice(slice_ty) => {
                Ok(ResolvedTy::Slice(Box::new(self.resolve_ty(&slice_ty.0)?)))
            }
            TyKind::Array(array_ty) => Ok(ResolvedTy::Array(
                Box::new(self.resolve_ty(&array_ty.0)?),
                array_ty.1.value.as_ref().try_into()?,
            )),
            TyKind::Ref(RefTy(MutTy { ty: ty2, mutbl })) => {
                Ok(ResolvedTy::Ref(Box::new(self.resolve_ty(ty2)?), *mutbl))
            }
            TyKind::Tup(tup_ty) => Ok(ResolvedTy::Tup(
                tup_ty
                    .0
                    .iter()
                    .map(|x| self.resolve_ty(x))
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
                        return Ok(self.get_type_by_id(self.get_self_type()?).clone());
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
                                                        args_v.push(self.resolve_ty(ty)?)
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
            TyKind::ImplicitSelf => Ok(ResolvedTy::ImplicitSelf),
        }
    }

    fn resolve_ty_in_scope_by_symbol(&self, s: &Symbol, id: usize) -> ResolvedTy {
        let mut full_name = self.get_prefix_name_from(id);
        full_name.push(s.clone());

        ResolvedTy::Named(full_name)
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

    fn any_type() -> TypeId {
        TypeId(1)
    }

    fn unit_expr_result() -> ExprResult {
        ExprResult {
            type_id: Self::unit_type(),
            category: ExprCategory::Not,
        }
    }

    fn check_const(&self, ty: &ResolvedTy, expr: &Expr) -> Result<(), SemanticError> {
        if let ResolvedTy::BulitIn(ident, _) = ty {
            match ident.0.as_str() {
                "u32" => {
                    let _: u32 = expr.try_into()?;
                    return Ok(());
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
            .filter(|x| *x != Self::any_type())
            .collect();

        if types.is_empty() {
            return Ok(Self::any_type());
        }

        let mut ret_ty = self.get_type_by_id(*types.first().unwrap());
        for x in types {
            let t = self.get_type_by_id(x);
            if ret_ty != t {
                if ret_ty == ResolvedTy::integer() && t.is_number_type() {
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
        let old_ast_id = self.current_ast_id;
        self.current_ast_id = expr.id;
        let mut ret;
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(self.current_ast_id, kind)?;
                ret = None
            }
            AnalyzeStage::Definition => {
                ret = None;
            }
            AnalyzeStage::Body => ret = Some(Self::unit_expr_result()),
        }

        self.enter_scope(self.current_ast_id)?;
        for stmt in &expr.stmts {
            // parser 阶段就保证只有最后一条 stmt 有可能不是 unit type
            ret = self.visit_stmt(stmt)?;
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

        self.current_ast_id = old_ast_id;
        Ok(ret.map(|x| ExprResult {
            type_id: x.type_id,
            category: ExprCategory::Not,
        }))
    }
}

impl Visitor for SemanticAnalyzer {
    type DefaultRes = Result<(), SemanticError>;
    type ExprRes = Result<Option<ExprResult>, SemanticError>;
    type PatRes = Result<PatResult, SemanticError>;

    fn visit_crate(&mut self, krate: &Crate) -> Result<(), SemanticError> {
        if matches!(self.stage, AnalyzeStage::SymbolCollect) {
            self.scopes.insert(
                krate.id,
                Scope {
                    id: krate.id,
                    kind: ScopeKind::Root,
                    types: HashMap::default(),
                    values: HashMap::default(),
                    children: HashSet::default(),
                    father: krate.id,
                },
            );
        }
        self.current_scope = krate.id;
        self.current_ast_id = krate.id;

        for item in &krate.items {
            self.visit_item(item)?
        }
        Ok(())
    }

    fn visit_item(&mut self, item: &Item) -> Result<(), SemanticError> {
        let old_id = self.current_ast_id;
        self.current_ast_id = item.id;
        match &item.kind {
            ItemKind::Const(const_item) => self.visit_const_item(const_item)?,
            ItemKind::Fn(fn_item) => self.visit_fn_item(fn_item)?,
            ItemKind::Mod(mod_item) => self.visit_mod_item(mod_item)?,
            ItemKind::Enum(enum_item) => self.visit_enum_item(enum_item)?,
            ItemKind::Struct(struct_item) => self.visit_struct_item(struct_item)?,
            ItemKind::Trait(trait_item) => self.visit_trait_item(trait_item)?,
            ItemKind::Impl(impl_item) => self.visit_impl_item(impl_item)?,
        }
        self.current_ast_id = old_id;
        Ok(())
    }

    fn visit_associate_item(&mut self, item: &Item<AssocItemKind>) -> Result<(), SemanticError> {
        let old_id = self.current_ast_id;
        self.current_ast_id = item.id;
        match &item.kind {
            AssocItemKind::Const(const_item) => self.visit_const_item(const_item)?,
            AssocItemKind::Fn(fn_item) => self.visit_fn_item(fn_item)?,
        }
        self.current_ast_id = old_id;
        Ok(())
    }

    fn visit_const_item(
        &mut self,
        ConstItem { ident, ty, expr }: &ConstItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            // 目前还是认为 const expr 中只能有简单表达式
            AnalyzeStage::SymbolCollect => {}
            AnalyzeStage::Definition => {
                let ty = self.resolve_ty(&ty)?;
                if let Some(e) = expr {
                    self.check_const(&ty, e)?;
                }
                let tyid = self.intern_type(ty);
                self.add_value(
                    ident.symbol.clone(),
                    Variable {
                        ty: tyid,
                        mutbl: Mutability::Not,
                        kind: VariableKind::Const,
                    },
                )?;
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
        }: &FnItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(self.current_ast_id, ScopeKind::Fn { ret_ty: TypeId(0) })?;
            }
            AnalyzeStage::Definition => {}
            AnalyzeStage::Body => {}
        }
        self.enter_scope(self.current_ast_id)?;
        match self.stage {
            AnalyzeStage::SymbolCollect => {}
            AnalyzeStage::Definition => {
                let param_tys = sig
                    .decl
                    .inputs
                    .iter()
                    .map(|x| self.resolve_ty(&x.ty))
                    .collect::<Result<Vec<_>, SemanticError>>()?;
                let ret_ty = match &sig.decl.output {
                    FnRetTy::Default => ResolvedTy::Tup(Vec::new()),
                    FnRetTy::Ty(ty) => self.resolve_ty(&ty)?,
                };
                let tyid = self.intern_type(ResolvedTy::Fn(param_tys, Box::new(ret_ty.clone())));
                self.add_value(
                    ident.symbol.clone(),
                    Variable {
                        ty: tyid,
                        mutbl: Mutability::Not,
                        kind: VariableKind::Const,
                    },
                )?;

                let ret_ty_id = self.intern_type(ret_ty);
                match &mut self.get_scope_mut().kind {
                    ScopeKind::Fn { ret_ty } => *ret_ty = ret_ty_id,
                    _ => panic!("Impossible!"),
                }

                todo!() // TODO impl 和 trait 的 fn 无法直接找到自己，也就是说它们的 scope 没有它们自己，这应该怎么解决
            }
            AnalyzeStage::Body => {}
        }
        if let Some(b) = body {
            self.visit_block_expr(b)?;
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
                        kind: TypeKind::Placeholder,
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
                            Ok(x.ident.clone())
                        }
                    })
                    .collect::<Result<Vec<_>, SemanticError>>()?;
                self.search_type_mut(&ident.symbol)?.1.kind = TypeKind::Enum { fields };
            }
            AnalyzeStage::Body => todo!(),
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
                        kind: TypeKind::Struct { fields: Vec::new() },
                    },
                )?;
            }
            AnalyzeStage::Definition => {
                let fields = match variant_data {
                    crate::ast::item::VariantData::Struct { fields } => fields
                        .iter()
                        .map(|x| {
                            Ok((
                                x.ident.clone().ok_or(SemanticError::Unimplemented)?,
                                self.intern_type(self.resolve_ty(&x.ty)?),
                            ))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?,
                    crate::ast::item::VariantData::Tuple(_) => {
                        return Err(SemanticError::Unimplemented);
                    }
                    crate::ast::item::VariantData::Unit => Vec::new(),
                };
                self.search_type_mut(&ident.symbol)?.1.kind = TypeKind::Struct { fields }
            }
            AnalyzeStage::Body => todo!(),
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
                        kind: TypeKind::Placeholder,
                    },
                )?;
                self.add_scope(self.current_ast_id, ScopeKind::Trait(id))?;
            }
            AnalyzeStage::Definition => {
                let mut methods = Vec::new();
                let mut constants = Vec::new();
                for item in items {
                    match &item.kind {
                        AssocItemKind::Const(ConstItem { ident, ty, expr }) => {
                            let resloved_ty = self.resolve_ty(&ty)?;
                            if let Some(e) = expr {
                                self.check_const(&resloved_ty, e)?;
                            }
                            let tyid = self.intern_type(resloved_ty);
                            constants.push(Constant {
                                name: ident.symbol.clone(),
                                ty: tyid,
                                is_placeholder: expr.is_none(),
                            })
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
                                .map(|x| self.resolve_ty(&x.ty))
                                .collect::<Result<Vec<_>, SemanticError>>()?;
                            let ret_ty = match &sig.decl.output {
                                FnRetTy::Default => ResolvedTy::Tup(Vec::new()),
                                FnRetTy::Ty(ty) => self.resolve_ty(&ty)?,
                            };
                            let fn_type = ResolvedTy::Fn(param_tys, Box::new(ret_ty));
                            let fn_type_id = self.intern_type(fn_type);
                            methods.push(FnSig {
                                name: ident.symbol.clone(),
                                type_id: fn_type_id,
                                is_placeholder: body.is_none(),
                            });
                        }
                    }
                }
                self.search_type_mut(&ident.symbol)?.1.kind =
                    TypeKind::Trait { methods, constants };
            }
            AnalyzeStage::Body => todo!(),
        }
        self.enter_scope(self.current_ast_id)?;
        for item in items {
            self.visit_associate_item(item)?;
        }
        self.exit_scope()?;
        Ok(())
    }

    fn visit_impl_item(
        &mut self,
        ImplItem {
            generics,
            of_trait,
            self_ty,
            items,
        }: &ImplItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                // 0 type 作为 self ty 的占位符
                self.add_scope(self.current_ast_id, ScopeKind::Impl(TypeId(0)))?;
            }
            AnalyzeStage::Definition => {
                todo!()
                // TODO 在这里检查一下实现和 Trait 是否能对应上，同时把 self type 设置好
            }
            AnalyzeStage::Body => todo!(),
        }
        self.enter_scope(self.current_ast_id)?;
        for item in items {
            self.visit_associate_item(item)?;
        }
        self.exit_scope()?;
        Err(SemanticError::Unimplemented)
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::stmt::Stmt) -> Self::ExprRes {
        match &stmt.kind {
            StmtKind::Let(local_stmt) => {
                self.visit_let_stmt(local_stmt)?;
            }
            StmtKind::Item(item) => {
                self.visit_item(item)?;
            }
            StmtKind::Expr(expr) => return self.visit_expr(expr),
            StmtKind::Semi(expr) => {
                return self
                    .visit_expr(expr)
                    .map(|x| x.map(|_| Self::unit_expr_result()));
            }
            StmtKind::Empty(_) => {}
        }

        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => Ok(None),
            AnalyzeStage::Body => Ok(Some(Self::unit_expr_result())),
        }
    }

    fn visit_let_stmt(&mut self, stmt: &crate::ast::stmt::LocalStmt) -> Result<(), SemanticError> {
        let expected_ty = match &stmt.ty {
            Some(ty) => self.resolve_ty(&ty)?,
            None => return Err(SemanticError::Unimplemented),
        };

        let expected_ty_id = self.intern_type(expected_ty.clone());

        match &stmt.kind {
            LocalKind::Decl => {
                if matches!(self.stage, AnalyzeStage::Body) {
                    // 从下面复制上来的，因为不用检查变量是否初始化
                    let pat_res = self.visit_pat(&stmt.pat, expected_ty_id)?;
                    for (symbol, type_id, mutbl) in pat_res.bindings {
                        self.add_value(
                            symbol,
                            Variable {
                                ty: type_id,
                                mutbl: mutbl,
                                kind: VariableKind::Inited,
                            },
                        )?;
                    }
                }
            }
            LocalKind::Init(expr) => {
                let res = self.visit_expr(&expr)?;
                match res {
                    Some(ExprResult { type_id, category }) => {
                        debug_assert!(matches!(self.stage, AnalyzeStage::Body));
                        no_assignee!(category);
                        let flag = if expected_ty_id == type_id {
                            true
                        } else {
                            let expr_ty = self.get_type_by_id(type_id);
                            expected_ty.is_number_type() && (expr_ty == ResolvedTy::integer())
                        };
                        if flag {
                            let pat_res = self.visit_pat(&stmt.pat, expected_ty_id)?;
                            for (symbol, type_id, mutbl) in pat_res.bindings {
                                self.add_value(
                                    symbol,
                                    Variable {
                                        ty: type_id,
                                        mutbl: mutbl,
                                        kind: VariableKind::Inited,
                                    },
                                )?;
                            }
                        }
                    }
                    None => {}
                }
            }
        }
        Ok(())
    }

    fn visit_array_expr(&mut self, expr: &crate::ast::expr::ArrayExpr) -> Self::ExprRes {
        let expr_res = expr
            .0
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<Option<ExprResult>>, SemanticError>>()?;

        if matches!(
            self.stage,
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition
        ) {
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

    fn visit_const_block_expr(&mut self, expr: &crate::ast::expr::ConstBlockExpr) -> Self::ExprRes {
        todo!()
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
                        debug_assert!(!target.is_implicit_self());
                        let income_ty = self.get_type_by_id(income.type_id);

                        // 有没有 auto deref 的情况？
                        if income_ty != *target {
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
            span,
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

                let mut opt_ty = Some(self.get_type_by_id(type_id).clone());
                let mut ref_flag = false;
                let mut mutbl = get_mutbl!(category);

                while let Some(ty) = &opt_ty {
                    let ty_id = self.intern_type(ty.clone());
                    let items = self.get_type_items(
                        ty_id,
                        true, /* 此处保证 items 中均为 Fn Variant 且首个参数为 self */
                    )?;
                    if let Some((_, fn_id)) =
                        items.iter().find(|(symbol, _)| *symbol == ident.symbol)
                    {
                        let fn_ty = self.get_type_by_id(*fn_id);

                        let ResolvedTy::Fn(required, ret) = fn_ty else {
                            panic!("Impossible!")
                        };

                        if required.len() != params_res.len() + 1 {
                            return Err(SemanticError::MismatchArgNum);
                        }

                        // 检查 self
                        match required.first().unwrap() {
                            ResolvedTy::Ref(_, target_mut) => match (target_mut, mutbl) {
                                (Mutability::Mut, Mutability::Not) => {
                                    return Err(SemanticError::ImmutableVar);
                                }
                                _ => {}
                            },
                            ResolvedTy::ImplicitSelf => {
                                if ref_flag {
                                    return Err(SemanticError::TypeMismatch);
                                }
                            }
                            _ => panic!("Impossible"),
                        }

                        // 检查其他参数
                        for (income, target) in params_res.iter().zip(required[1..].iter()) {
                            debug_assert!(!target.is_implicit_self());
                            let income_ty = self.get_type_by_id(income.type_id);

                            if income_ty != *target {
                                return Err(SemanticError::TypeMismatch);
                            }
                        }

                        // 返回
                        return Ok(Some(ExprResult {
                            type_id: self.intern_type(ret.as_ref().clone()),
                            category: ExprCategory::Not,
                        }));
                    }

                    opt_ty = match ty {
                        ResolvedTy::Ref(resolved_ty, mutability) => {
                            ref_flag = true;
                            mutbl = mutbl.merge(*mutability);
                            Some(resolved_ty.as_ref().clone())
                        }
                        _ => None,
                    }
                }

                Err(SemanticError::NonMethodCall)
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
                Some(Self::unit_expr_result())
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

                let ty1 = self.get_type_by_id(res1.type_id).try_deref().0;
                let ty2 = self.get_type_by_id(res2.type_id).try_deref().0;

                match bin_op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                        if !ty1.is_number_type() || !ty2.is_number_type() {
                            return Err(SemanticError::NoImplementation);
                        }
                        let ret_ty = if ty1 == ResolvedTy::integer() {
                            ty2
                        } else if ty2 == ResolvedTy::integer() || ty1 == ty2 {
                            ty1
                        } else {
                            return Err(SemanticError::NoImplementation);
                        };

                        Ok(Some(ExprResult {
                            type_id: self.intern_type(ret_ty.clone()),
                            category: ExprCategory::Not,
                        }))
                    }
                    BinOp::And | BinOp::Or => {
                        if ty1 == ResolvedTy::bool() && ty2 == ResolvedTy::bool() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::bool()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                    BinOp::BitXor | BinOp::BitAnd | BinOp::BitOr => {
                        if ty1 == ResolvedTy::bool() && ty2 == ResolvedTy::bool() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::bool()),
                                category: ExprCategory::Not,
                            }))
                        } else if ty1.is_number_type() && ty2.is_number_type() {
                            let ret_ty = if ty1 == ResolvedTy::integer() {
                                ty2
                            } else if ty2 == ResolvedTy::integer() || ty1 == ty2 {
                                ty1
                            } else {
                                return Err(SemanticError::NoImplementation);
                            };

                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ret_ty.clone()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                    BinOp::Shl | BinOp::Shr => {
                        if ty1.is_number_type() && ty2.is_number_type() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ty1.clone()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Ge | BinOp::Gt => {
                        if (matches!(ty1, ResolvedTy::BulitIn(_, _))
                            && matches!(ty2, ResolvedTy::BulitIn(_, _))
                            && ty1 == ty2)
                            || ((ty1 == ResolvedTy::str() || ty1 == ResolvedTy::string())
                                && (ty2 == ResolvedTy::str() || ty2 == ResolvedTy::string()))
                            || (ty1 == ResolvedTy::integer() && ty2.is_number_type())
                            || (ty2 == ResolvedTy::integer() && ty1.is_number_type())
                        {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::bool()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
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
                        if ty == ResolvedTy::bool()
                            || ty == ResolvedTy::integer()
                            || ty == ResolvedTy::i32()
                            || ty == ResolvedTy::u32()
                        {
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
                        if ty == ResolvedTy::i32() || ty == ResolvedTy::integer() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::i32()),
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
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => Ok(None),
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

    fn visit_assign_expr(&mut self, expr: &crate::ast::expr::AssignExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_assign_op_expr(&mut self, expr: &crate::ast::expr::AssignOpExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_field_expr(
        &mut self,
        FieldExpr(expr, ident): &crate::ast::expr::FieldExpr,
    ) -> Self::ExprRes {
        let res = self.visit_expr(expr)?;

        match res {
            Some(res) => {
                let res_mut = get_mutbl!(res.category);
                let fields = self.get_type_fields(res.type_id)?;
                match fields.iter().find(|(symbol, _)| *symbol == ident.symbol) {
                    Some((_, var)) => Ok(Some(ExprResult {
                        type_id: var.ty,
                        category: ExprCategory::Place(var.mutbl.merge(res_mut)),
                    })),
                    None => Err(SemanticError::UnknownVariable),
                }
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
                let (ty1, mut1) = self.get_type_by_id(res1.type_id).deref_all();
                let ty2 = self.get_type_by_id(res2.type_id);

                if ty2 != ResolvedTy::usize() {
                    return Err(SemanticError::TypeMismatch);
                }

                if let ResolvedTy::Array(ele_ty, _) | ResolvedTy::Slice(ele_ty) = ty1 {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(ele_ty.as_ref().clone()),
                        category: ExprCategory::Place(res1_mut.merge(mut1)),
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
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => Ok(None),
            AnalyzeStage::Body => Ok(Some(ExprResult {
                type_id: self.intern_type(ResolvedTy::Any),
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
                    let values = self.get_type_items(type_id, false)?;
                    for (symbol, id) in &values {
                        if *symbol == value_seg.ident.symbol {
                            return Ok(Some(ExprResult {
                                type_id: *id,
                                category: ExprCategory::Not,
                            }));
                        }
                    }

                    Err(SemanticError::UnknownVariable)
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
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => {
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
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => Ok(None),
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
                let ScopeKind::Fn { ret_ty: target_id } = scope.kind else {
                    panic!("Impossible")
                };

                let ret_ty = self.get_type_by_id(res.type_id);
                let target_ty = self.get_type_by_id(target_id);

                if !ret_ty.can_trans_to_target_type(&target_ty) {
                    Err(SemanticError::TypeMismatch)
                } else {
                    Ok(Some(Self::unit_expr_result()))
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
        if qself.is_some() || !matches!(rest, StructRest::None) {
            return Err(SemanticError::Unimplemented);
        };

        let exp_fields = fields
            .iter()
            .map(|x| -> Result<(Symbol, Option<ExprResult>), SemanticError> {
                Ok((x.ident.symbol.clone(), self.visit_expr(&x.expr)?))
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;

        if path.segments.len() > 1 {
            return Err(SemanticError::Unimplemented);
        }
        let first = path.segments.first().unwrap();
        if first.args.is_some() {
            return Err(SemanticError::Unimplemented);
        }
        let (id, struct_info) = self.search_type(&first.ident.symbol)?;
        match &struct_info.kind {
            TypeKind::Placeholder => panic!("Impossible"),
            TypeKind::Struct { fields } => match self.stage {
                AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => Ok(None),
                AnalyzeStage::Body => {
                    let exp_fields = exp_fields
                        .into_iter()
                        .map(|(s, e)| match e {
                            Some(e) => Some((s, e)),
                            None => None,
                        })
                        .collect::<Option<Vec<_>>>()
                        .unwrap();

                    for (_, res) in &exp_fields {
                        no_assignee!(res.category);
                    }

                    let mut dic: HashMap<Symbol, ExprResult> = HashMap::new();

                    for x in exp_fields.into_iter() {
                        if let Some(_) = dic.insert(x.0, x.1) {
                            return Err(SemanticError::MultiSpecifiedField);
                        }
                    }

                    for (field_ident, field_type_id) in fields {
                        if let Some(res) = dic.get(&field_ident.symbol) {
                            let res_ty = self.get_type_by_id(res.type_id);
                            let field_ty = self.get_type_by_id(*field_type_id);
                            if !res_ty.can_trans_to_target_type(&field_ty) {
                                return Err(SemanticError::TypeMismatch);
                            }
                        }
                    }

                    Ok(Some(ExprResult {
                        type_id: self.intern_type(
                            self.resolve_ty_in_scope_by_symbol(&first.ident.symbol, id),
                        ),
                        category: ExprCategory::Not,
                    }))
                }
            },
            TypeKind::Enum { fields: _ }
            | TypeKind::Trait {
                methods: _,
                constants: _,
            } => Err(SemanticError::NotStructType),
        }
    }

    fn visit_repeat_expr(&mut self, RepeatExpr(expr, const_expr): &RepeatExpr) -> Self::ExprRes {
        todo!()
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
        pat: &crate::ast::pat::PathPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        todo!()
    }

    fn visit_tuple_pat(
        &mut self,
        _pat: &crate::ast::pat::TuplePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_ref_pat(
        &mut self,
        pat: &crate::ast::pat::RefPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        todo!()
    }

    fn visit_lit_pat(
        &mut self,
        pat: &crate::ast::pat::LitPat,
        expected_ty: TypeId,
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
