use crate::{
    ast::NodeId,
    ir::ir_value::{BasicBlockPtr, ValuePtr},
    irgen::value::ValuePtrContainer,
    semantics::item::AssociatedInfo,
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct CycleInfo<'tmp> {
    pub(crate) continue_bb: &'tmp BasicBlockPtr,
    pub(crate) next_bb: &'tmp BasicBlockPtr,
    pub(crate) value: Option<&'tmp ValuePtr>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ExprExtra<'tmp> {
    pub(crate) scope_id: NodeId,
    pub(crate) self_id: NodeId,

    pub(crate) cycle_info: Option<CycleInfo<'tmp>>,
}

#[derive(Debug)]
pub(crate) struct ItemExtra {
    pub(crate) scope_id: NodeId,
    pub(crate) self_id: NodeId,

    pub(crate) associated_info: Option<AssociatedInfo>,
}

pub(crate) struct PatExtra {
    pub(crate) value: ValuePtrContainer,
    pub(crate) self_id: NodeId,
}
