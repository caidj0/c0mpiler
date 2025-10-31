use enum_as_inner::EnumAsInner;

use crate::{
    ir::{ir_type::TypePtr, ir_value::ValuePtr},
    irgen::IRGenerator,
    semantics::{impls::DerefLevel, value::ValueIndex},
};

#[derive(Debug, Clone)]
pub(crate) struct ValuePtrContainer {
    pub(crate) value_ptr: ValuePtr,
    pub(crate) kind: ContainerKind,
}

impl ValuePtrContainer {
    pub(crate) fn flatten(self) -> Vec<ValuePtr> {
        match self.kind {
            ContainerKind::Raw { fat: Some(fat) } => vec![self.value_ptr, fat],
            _ => vec![self.value_ptr],
        }
    }
}

#[derive(Debug, EnumAsInner, Clone)]
pub(crate) enum ContainerKind {
    Raw { fat: Option<ValuePtr> },
    Ptr(TypePtr),
}

impl<'analyzer> IRGenerator<'analyzer> {
    pub(crate) fn get_value_type(&self, value: &ValuePtrContainer) -> TypePtr {
        match &value.kind {
            ContainerKind::Raw { fat: Some(..) } => self.fat_ptr_type().into(),
            ContainerKind::Raw { fat: None } => value.value_ptr.get_type().clone(),
            ContainerKind::Ptr(ty) => ty.clone(),
        }
    }

    pub(crate) fn get_value_presentation(&self, value: ValuePtrContainer) -> ValuePtrContainer {
        match &value.kind {
            ContainerKind::Raw { .. } => {
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
                        kind: ContainerKind::Raw { fat: None },
                    }
                }
            }
        }
    }

    pub(crate) fn raw_value_to_ptr(&self, raw: ValuePtrContainer) -> ValuePtrContainer {
        let raw_type = raw.value_ptr.get_type();
        if let Some(fat) = raw.kind.as_raw().unwrap() {
            debug_assert!(
                raw_type.is_ptr() && fat.is_int_type(),
                "raw: {:?}\nfat: {:?}",
                raw_type,
                fat
            );

            let fat_ptr_type = self.fat_ptr_type();
            let allocated = self.builder.build_alloca(fat_ptr_type.clone().into(), None);
            self.builder
                .build_store(raw.value_ptr, allocated.clone().into());
            let second = self.builder.build_getelementptr(
                fat_ptr_type.clone().into(),
                allocated.clone().into(),
                vec![
                    self.context.get_i32(0).into(),
                    self.context.get_i32(1).into(),
                ],
                None,
            );
            self.builder.build_store(fat.clone(), second.into());

            ValuePtrContainer {
                value_ptr: allocated.into(),
                kind: ContainerKind::Ptr(fat_ptr_type.into()),
            }
        } else {
            let allocated = self.builder.build_alloca(raw_type.clone(), None);
            let inner_type = raw_type.clone();
            self.builder
                .build_store(raw.value_ptr, allocated.clone().into());
            ValuePtrContainer {
                value_ptr: allocated.into(),
                kind: ContainerKind::Ptr(inner_type),
            }
        }
    }

    pub(crate) fn get_value_ptr(&self, value: ValuePtrContainer) -> ValuePtrContainer {
        match value.kind {
            ContainerKind::Raw { .. } => self.raw_value_to_ptr(value),
            ContainerKind::Ptr(..) => value,
        }
    }

    pub(crate) fn get_raw_value(&self, value: ValuePtrContainer) -> ValuePtr {
        match value.kind {
            ContainerKind::Raw { .. } => value.value_ptr,
            ContainerKind::Ptr(ty) => self.builder.build_load(ty, value.value_ptr, None).into(),
        }
    }

    pub(crate) fn add_value_index(&mut self, index: ValueIndex, value: ValuePtrContainer) {
        let replacer = self.value_indexes.insert(index, value);
        debug_assert!(replacer.is_none());
    }

    pub(crate) fn get_value_by_index(&mut self, index: &ValueIndex) -> Option<&ValuePtrContainer> {
        self.value_indexes.get(index)
    }

    pub(crate) fn store_to_ptr(&mut self, dest: ValuePtr, src: ValuePtrContainer) {
        match src.kind {
            ContainerKind::Raw { fat } => {
                self.builder.build_store(src.value_ptr, dest.clone());
                if let Some(fat) = fat {
                    let second = self.builder.build_getelementptr(
                        self.context.i8_type().into(),
                        dest,
                        vec![self.context.get_i32(4).into()],
                        None,
                    );
                    self.builder.build_store(fat, second.into());
                }
            }
            ContainerKind::Ptr(ty) => {
                self.builder
                    .build_memcpy(&mut self.module, dest, src.value_ptr, ty);
            }
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
                        deref_level,
                        ty,
                    )
                }
            }
        }
    }
}
