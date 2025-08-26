use std::collections::{HashMap, HashSet};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, NodeId, Symbol},
    semantics::{const_eval::ConstEvalValue, resolved_ty::ResolvedTy},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FullName(pub Vec<Symbol>);

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
    pub methods: HashMap<Symbol, FnSig>,
    pub constants: HashMap<Symbol, Constant>,
}

pub enum ImplInfoItem<'a> {
    Method(&'a FnSig),
    Constant(&'a Constant),
}

impl ImplInfo {
    pub fn contains_key(&self, key: &Symbol) -> bool {
        self.methods.contains_key(key) || self.constants.contains_key(key)
    }

    pub fn get(&self, key: &Symbol) -> Option<ImplInfoItem> {
        self.methods
            .get(key)
            .map(ImplInfoItem::Method)
            .or(self.constants.get(key).map(ImplInfoItem::Constant))
    }
}

#[derive(Debug)]
pub struct FnSig {
    pub type_id: TypeId,
    pub is_placeholder: bool,
}

#[derive(Debug)]
pub struct Constant {
    pub ty: TypeId,
    pub value: ConstEvalValue,
}

#[derive(Debug, EnumAsInner)]
pub enum TypeKind {
    Placeholder,
    Struct {
        fields: HashMap<Symbol, TypeId>,
    },
    Enum {
        fields: HashSet<Symbol>,
    },
    Trait {
        methods: HashMap<Symbol, FnSig>,
        constants: HashMap<Symbol, Constant>,
    },
}

#[derive(Debug)]
pub enum VariableKind {
    Decl,
    Inited,
    Fn,
    Constant(ConstEvalValue),
}

#[derive(Debug)]
pub struct Variable {
    pub ty: TypeId,
    pub mutbl: Mutability,
    pub kind: VariableKind,
}

#[derive(Debug)]
pub enum DerefLevel {
    Not,
    Deref(Mutability),
}

impl DerefLevel {
    pub fn merge(self, other: Self) -> Self {
        match (self, other) {
            (DerefLevel::Not, DerefLevel::Not) => DerefLevel::Not,
            (DerefLevel::Not, DerefLevel::Deref(mutability)) => DerefLevel::Deref(mutability),
            (DerefLevel::Deref(mutability), DerefLevel::Not) => DerefLevel::Deref(mutability),
            (DerefLevel::Deref(mutability1), DerefLevel::Deref(mutability2)) => {
                DerefLevel::Deref(Mutability::merge(mutability1, mutability2))
            }
        }
    }

    pub fn get_mutbl(&self) -> Mutability {
        match self {
            DerefLevel::Not => Mutability::Mut,
            DerefLevel::Deref(mutability) => *mutability,
        }
    }
}

#[derive(Debug, EnumAsInner)]
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
    Impl,
    Body,
}

pub type Impls = (ImplInfo, HashMap<FullName, ImplInfo>);
