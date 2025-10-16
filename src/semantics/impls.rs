use std::collections::HashMap;

use crate::{
    ast::{NodeId, Symbol}, semantics::{resolved_ty::TypePtr, value::Value},
};

#[derive(Debug)]
pub struct Impls {
    pub(crate) inherent: ImplInfo,
    pub(crate) traits: HashMap<TypePtr, ImplInfo>,
}

// Constant 和 Function 共享一个命名空间
#[derive(Debug)]
pub struct ImplInfo {
    pub(crate) values: HashMap<Symbol, Value>,
}
