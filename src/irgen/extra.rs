use crate::{ast::NodeId, irgen::value::ValuePtrContainer};

#[derive(Debug, Clone, Copy)]
pub(crate) struct ExprExtra {
    pub(crate) scope_id: NodeId,
    pub(crate) self_id: NodeId,
}

pub(crate) struct PatExtra {
    pub(crate) ptr: ValuePtrContainer,
    pub(crate) self_id: NodeId,
}
