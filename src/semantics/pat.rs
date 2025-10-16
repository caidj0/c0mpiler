use crate::{ast::Symbol, semantics::value::ValueIndex};

#[derive(Debug)]
pub struct PatResult {
    pub bindings: Vec<(Symbol, ValueIndex)>,
}
