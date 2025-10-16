use crate::ast::{NodeId, Span};

#[derive(Debug)]
pub struct ItemExtra {
    pub(crate) father: NodeId,
    pub(crate) self_id: NodeId,
    pub(crate) span: Span,
}
