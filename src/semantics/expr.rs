use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, NodeId, Span},
    semantics::{resolved_ty::TypePtr, value::ValueIndex},
};

#[derive(Debug)]
pub struct ExprExtra<'tmp> {
    pub(crate) target_ty: Option<&'tmp mut TypePtr>,
    pub(crate) scope_id: NodeId,
    pub(crate) self_id: NodeId,

    pub(crate) span: Span,
}

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
