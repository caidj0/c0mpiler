use std::collections::{HashMap, HashSet};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, NodeId, Symbol},
    const_eval::ConstEvalValue,
    semantics::resolved_ty::ResolvedTy,
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

#[derive(Debug, Clone)]
pub struct ImplInfo {
    pub methods: HashMap<Symbol, FnSig>,
    pub constants: HashMap<Symbol, Constant>,
}

#[derive(Debug)]
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

#[derive(Debug, Clone, Copy)]
pub struct FnSig {
    pub type_id: TypeId,
    pub is_placeholder: bool,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, EnumAsInner)]
pub enum VariableKind {
    Decl,
    Inited,
    Fn,
    Constant(ConstEvalValue),
}

impl VariableKind {
    pub fn can_shadow_unconditionally(&self, shadowed: &Self) -> bool {
        matches!(self, Self::Constant(_) | Self::Fn)
            && matches!(shadowed, Self::Decl | Self::Inited)
    }
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

#[derive(Debug, EnumAsInner, Clone)]
pub enum ScopeKind {
    Lambda,
    Root,
    Crate,
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

#[derive(Debug, Clone, Copy)]
pub enum ExprCategory {
    Place(Mutability),
    Not, // 也就是 Value Expr
    Only,
}

#[derive(Debug, Clone, Copy)]
pub enum InterruptControlFlow {
    Not,
    Loop,
    Return,
}

impl InterruptControlFlow {
    pub fn concat(self, other: Self) -> Self {
        match self {
            InterruptControlFlow::Not => other,
            _ => self,
        }
    }

    pub fn shunt(self, other: Self) -> Self {
        match (self, other) {
            (_, InterruptControlFlow::Not) | (InterruptControlFlow::Not, _) => {
                InterruptControlFlow::Not
            }
            (_, InterruptControlFlow::Loop) | (InterruptControlFlow::Loop, _) => {
                InterruptControlFlow::Loop
            }
            (InterruptControlFlow::Return, InterruptControlFlow::Return) => {
                InterruptControlFlow::Return
            }
        }
    }

    pub fn out_of_cycle(self) -> Self {
        match self {
            InterruptControlFlow::Not | InterruptControlFlow::Loop => InterruptControlFlow::Not,
            InterruptControlFlow::Return => InterruptControlFlow::Return,
        }
    }
}

#[derive(Debug)]
pub struct ExprResult {
    pub type_id: TypeId,
    pub category: ExprCategory,
    pub int_flow: InterruptControlFlow,
}

impl ExprResult {
    pub fn replace_by(&mut self, other: Self) {
        *self = Self {
            int_flow: self.int_flow.concat(other.int_flow),
            ..other
        }
    }
}

#[derive(Debug, EnumAsInner)]
pub enum StmtResult {
    Expr(ExprResult),
    Else { int_flow: InterruptControlFlow },
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

#[derive(Debug)]
pub struct BulitInImpls {
    pub u32_and_usize: Impls,
    pub string: Impls,
    pub str: Impls,
    pub array_and_slice: Impls,
}

impl BulitInImpls {
    pub fn new(type_table: &mut TypeTable) -> Self {
        let len_method: ResolvedTy = ResolvedTy::Fn(
            vec![ResolvedTy::ref_implicit_self()],
            Box::new(ResolvedTy::usize()),
        );
        let len_method_id = type_table.intern(len_method);
        let len = (
            Symbol("len".to_string()),
            FnSig {
                type_id: len_method_id,
                is_placeholder: false,
            },
        );
        let to_string_method = ResolvedTy::Fn(
            vec![ResolvedTy::ref_implicit_self()],
            Box::new(ResolvedTy::string()),
        );
        let to_string_method_id = type_table.intern(to_string_method);
        let to_string = (
            Symbol("to_string".to_string()),
            FnSig {
                type_id: to_string_method_id,
                is_placeholder: false,
            },
        );
        let as_str_method = ResolvedTy::Fn(
            vec![ResolvedTy::ref_implicit_self()],
            Box::new(ResolvedTy::ref_str()),
        );
        let as_str_method_id = type_table.intern(as_str_method);
        let as_str = (
            Symbol("as_str".to_string()),
            FnSig {
                type_id: as_str_method_id,
                is_placeholder: false,
            },
        );

        Self {
            u32_and_usize: (
                ImplInfo {
                    methods: HashMap::from([to_string]),
                    constants: HashMap::new(),
                },
                HashMap::new(),
            ),
            string: (
                ImplInfo {
                    methods: HashMap::from([as_str, len.clone()]),
                    constants: HashMap::new(),
                },
                HashMap::new(),
            ),
            str: (
                ImplInfo {
                    methods: HashMap::from([len.clone()]),
                    constants: HashMap::new(),
                },
                HashMap::new(),
            ),
            array_and_slice: (
                ImplInfo {
                    methods: HashMap::from([len.clone()]),
                    constants: HashMap::new(),
                },
                HashMap::new(),
            ),
        }
    }
}

#[derive(Debug, EnumAsInner)]
pub enum ValueContainer<'a> {
    Variable(&'a Variable),
    ImplInfoItem(TypeId, ImplInfoItem<'a>),
}
