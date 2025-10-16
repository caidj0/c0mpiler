use std::collections::{HashMap, HashSet};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{NodeId, Symbol},
    semantics::{resolved_ty::TypePtr, value::Value},
};

// Enum 可以认为是一个 Scope，Trait 也认为是一个 Scope
#[derive(Debug)]
pub struct Scope {
    pub id: NodeId,
    pub kind: ScopeKind,
    pub types: HashMap<Symbol, TypePtr>,
    pub values: HashMap<Symbol, Value>,
    pub children: HashSet<NodeId>,
    pub father: NodeId,
}

#[derive(Debug, EnumAsInner)]
pub enum ScopeKind {
    Lambda,
    Root,
    Crate,
    Trait(TypePtr),
    Impl(TypePtr),
    Enum,
    Fn {
        ret_ty: TypePtr,
        main_fn: MainFunctionState,
    },
    Loop {
        ret_ty: TypePtr,
    },
    CycleExceptLoop,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum MainFunctionState {
    Not,
    UnExited,
    Exited,
}
