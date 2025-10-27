use enum_as_inner::EnumAsInner;

use crate::{
    impossible,
    ir::{ir_type::TypePtr, ir_value::ValuePtr},
    irgen::IRGenerator,
    semantics::{impls::DerefLevel, resolved_ty::TypeIntern, value::ValueIndex},
};

#[derive(Debug, Clone)]
pub(crate) struct ValuePtrContainer {
    pub(crate) value_ptr: ValuePtr,
    pub(crate) kind: ContainerKind,
}

#[derive(Debug, PartialEq, Eq, EnumAsInner, Clone)]
pub(crate) enum ContainerKind {
    Raw,
    Ptr(TypePtr),
    ToUnsizedPtr,
}

impl<'analyzer> IRGenerator<'analyzer> {
    pub(crate) fn get_value_presentation(&self, value: ValuePtrContainer) -> ValuePtrContainer {
        match &value.kind {
            ContainerKind::Raw => {
                if value.value_ptr.get_type().is_aggregate_type() {
                    self.get_value_ptr(value)
                } else {
                    value
                }
            }
            ContainerKind::Ptr(ty) => {
                if ty.is_aggregate_type() {
                    value
                } else {
                    ValuePtrContainer {
                        value_ptr: self.get_raw_value(value),
                        kind: ContainerKind::Raw,
                    }
                }
            }
            ContainerKind::ToUnsizedPtr => todo!(),
        }
    }

    pub(crate) fn raw_value_to_ptr(&self, raw: ValuePtrContainer) -> ValuePtrContainer {
        let allocaed = self
            .builder
            .build_alloca(raw.value_ptr.get_type().clone(), None);
        let inner_type = raw.value_ptr.get_type().clone();
        self.builder
            .build_store(raw.value_ptr, allocaed.clone().into());
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

    pub(crate) fn get_value_index(&mut self, index: &ValueIndex) -> Option<&ValuePtrContainer> {
        self.value_indexes.get(index)
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

    // let a: Struct;  PtrType, Ptr(Struct)
    // a.i32;          PtrType, Ptr(i32)
    // &a;             PtrType, Raw
    // (&a).i32;       PtrType, Ptr(i32)
    // let b = &a;     PtrType, Ptr(Ptr)
    // b.i32;
    pub(crate) fn deref(
        &mut self,
        value: ValuePtrContainer,
        level: &DerefLevel,
        ty: &TypePtr,
    ) -> ValuePtrContainer {
        match level {
            DerefLevel::Not => value,
            DerefLevel::Deref(deref_level, ..) => {
                debug_assert!(value.kind.as_ptr().map_or(false, |x| x.is_ptr()));
                let value = self.get_raw_value(value);

                if deref_level.is_not() {
                    ValuePtrContainer {
                        value_ptr: value,
                        kind: ContainerKind::Ptr(ty.clone()),
                    }
                } else {
                    let new_value =
                        self.builder
                            .build_load(self.context.ptr_type().into(), value, None);
                    self.deref(
                        ValuePtrContainer {
                            value_ptr: new_value.into(),
                            kind: ContainerKind::Ptr(self.context.ptr_type().into()),
                        },
                        &deref_level,
                        ty,
                    )
                }
            }
        }
    }
}
