use std::collections::HashMap;

use crate::{
    ast::{NodeId, Symbol}, semantics::value::Value,
};

#[derive(Debug)]
pub struct Impls {
    inherent: ImplInfo,
    traits: HashMap<NodeId, ImplInfo>,
}

#[derive(Debug)]
pub struct ImplInfo {
    pub methods: HashMap<Symbol, Value>,
    pub constants: HashMap<Symbol, Value>,
}
