use enum_as_inner::EnumAsInner;

use crate::{ast::Mutability, semantics::value::ValueIndex};

#[derive(Debug)]
pub struct ExprExtra {}

#[derive(Debug)]
pub struct ExprResult {
    pub value_index: ValueIndex,
    pub assignee: AssigneeKind,
    pub interrupt: ControlFlowInterruptKind,
}

#[derive(Debug)]
pub enum AssigneeKind {
    Place(Mutability),
    Not,
    Only,
}

#[derive(Debug, Clone, Copy, EnumAsInner)]
pub enum ControlFlowInterruptKind {
    Not,
    Loop,
    Return,
}
