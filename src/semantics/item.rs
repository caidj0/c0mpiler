use crate::{
    ast::{NodeId, Span},
    semantics::resolved_ty::TypeKey,
};

#[derive(Debug)]
pub struct ItemExtra {
    pub(crate) father: NodeId,
    pub(crate) self_id: NodeId,
    pub(crate) span: Span,

    pub(crate) associated_info: Option<AssociatedInfo>,
}

#[derive(Debug)]
pub struct AssociatedInfo {
    pub(crate) is_trait: bool,

    pub(crate) ty: TypeKey,
    pub(crate) for_trait: Option<TypeKey>,
}
