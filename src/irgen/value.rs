use crate::{
    impossible,
    ir::{ir_type::TypePtr, ir_value::ValuePtr},
    irgen::IRGenerator,
    semantics::{resolved_ty::TypeIntern, value::ValueIndex},
};

pub(crate) struct ValuePtrContainer {
    pub(crate) value_ptr: ValuePtr,
    pub(crate) kind: ContainerKind,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ContainerKind {
    Raw,
    Ptr(TypePtr),
    ToUnsizedPtr,
}

impl<'analyzer> IRGenerator<'analyzer> {
    pub(crate) fn get_value_presentation(&self, value: ValuePtrContainer) -> ValuePtr {
        match &value.kind {
            ContainerKind::Raw => {
                if value.value_ptr.get_type().is_aggregate_type() {
                    self.get_value_ptr(value).value_ptr
                } else {
                    value.value_ptr
                }
            }
            ContainerKind::Ptr(ty) => {
                if ty.is_aggregate_type() {
                    value.value_ptr
                } else {
                    self.get_raw_value(value)
                }
            }
            ContainerKind::ToUnsizedPtr => todo!(),
        }
    }

    pub(crate) fn raw_value_to_ptr(&self, raw: ValuePtrContainer) -> ValuePtrContainer {
        let allocaed = self.builder.build_alloca(raw.value_ptr.get_type().clone(), None);
        let inner_type = raw.value_ptr.get_type().clone();
        self.builder.build_store(raw.value_ptr, allocaed.clone().into());
        ValuePtrContainer {
            value_ptr: allocaed.into(),
            kind: ContainerKind::Ptr(inner_type),
        }
    }

    pub(crate) fn get_value_ptr(&self, value: ValuePtrContainer) -> ValuePtrContainer {
        match value.kind {
            ContainerKind::Raw => self.raw_value_to_ptr(value),
            ContainerKind::Ptr(..) | ContainerKind::ToUnsizedPtr => value,
        }
    }

    pub(crate) fn get_raw_value(&self, value: ValuePtrContainer) -> ValuePtr {
        match value.kind {
            ContainerKind::Raw => value.value_ptr,
            ContainerKind::Ptr(ty) => self.builder.build_load(ty, value.value_ptr, None).into(),
            ContainerKind::ToUnsizedPtr => impossible!(),
        }
    }

    pub(crate) fn add_value_index(&mut self, index: ValueIndex, value: ValuePtrContainer) {
        let replacer = self.value_indexes.insert(index, value);
        debug_assert!(replacer.is_none());
    }

    pub(crate) fn store_to_ptr(&mut self, dest: ValuePtr, src: ValuePtrContainer) {
        match src.kind {
            ContainerKind::Raw => self.builder.build_store(src.value_ptr, dest),
            ContainerKind::Ptr(ty) => {
                self.builder
                    .build_memcpy(&mut self.module, dest, src.value_ptr, ty)
            }
            ContainerKind::ToUnsizedPtr => impossible!(),
        };
    }
}
