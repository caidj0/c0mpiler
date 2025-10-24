use crate::{
    impossible,
    ir::{ir_type::TypePtr, ir_value::ValuePtr},
    irgen::IRGenerator,
    semantics::value::ValueIndex,
};

pub(crate) struct ValuePtrContainer {
    pub(crate) ptr: ValuePtr,
    pub(crate) kind: ContainerKind,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ContainerKind {
    Raw,
    Ptr,
    WrapedPtr,
}

impl<'analyzer> IRGenerator<'analyzer> {
    pub(crate) fn raw_value_to_ptr(&self, raw: ValuePtrContainer) -> ValuePtrContainer {
        debug_assert_eq!(raw.kind, ContainerKind::Raw);
        let allocaed = self.builder.build_alloca(raw.ptr.get_type().clone(), None);
        self.builder.build_store(raw.ptr, allocaed.clone().into());
        ValuePtrContainer {
            ptr: allocaed.into(),
            kind: ContainerKind::Ptr,
        }
    }

    pub(crate) fn get_value_ptr(&self, value: ValuePtrContainer) -> ValuePtrContainer {
        match value.kind {
            ContainerKind::Raw => self.raw_value_to_ptr(value),
            ContainerKind::Ptr | ContainerKind::WrapedPtr => value,
        }
    }

    pub(crate) fn get_raw_value(&self, value: ValuePtrContainer, ty: &TypePtr) -> ValuePtr {
        match value.kind {
            ContainerKind::Raw => {
                debug_assert_eq!(value.ptr.get_type(), ty);
                value.ptr
            }
            ContainerKind::Ptr => self.builder.build_load(ty.clone(), value.ptr, None).into(),
            ContainerKind::WrapedPtr => impossible!(),
        }
    }

    pub(crate) fn add_value_index(&mut self, index: ValueIndex, value: ValuePtrContainer) {
        let replacer = self.value_indexes.insert(index, value);
        debug_assert!(replacer.is_none());
    }
}
