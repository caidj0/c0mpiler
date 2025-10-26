use crate::{
    ast::NodeId,
    ir::ir_value::{BasicBlockPtr, ValuePtr},
    irgen::value::ValuePtrContainer,
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

pub(crate) struct PatExtra {
    pub(crate) value: ValuePtrContainer,
    pub(crate) self_id: NodeId,
}
