use crate::{
    ast::{BindingMode, ByRef},
    irgen::{IRGenerator, extra::PatExtra, value::ValuePtrContainer},
    semantics::value::{PlaceValueIndex, ValueIndex, ValueIndexKind},
};

impl<'ast, 'analyzer> IRGenerator<'ast, 'analyzer> {
    pub(crate) fn visit_ident_pat_impl(
        &mut self,
        BindingMode(by_ref, _): &BindingMode,
        ident: &crate::ast::Ident,
        PatExtra {
            value: right_ptr,
            self_id,
        }: PatExtra,
    ) {
        let index = ValueIndex::Place(PlaceValueIndex {
            name: ident.symbol.clone(),
            kind: ValueIndexKind::Bindings {
                binding_id: self_id,
            },
        });

        let ty = self.get_value_type(&right_ptr);
        let value = if matches!(by_ref, ByRef::Yes(_)) {
            let ptr = self.build_alloca(self.context.ptr_type().into(), Some(&ident.symbol.0));
            self.builder
                .build_store(self.get_value_ptr(right_ptr).value_ptr, ptr.clone().into());
            ValuePtrContainer {
                value_ptr: ptr.into(),
                kind: crate::irgen::value::ContainerKind::Ptr(self.context.ptr_type().into()),
            }
        } else {
            let ptr = self.build_alloca(ty.clone(), Some(&ident.symbol.0));
            self.store_to_ptr(ptr.clone().into(), right_ptr);

            ValuePtrContainer {
                value_ptr: ptr.into(),
                kind: crate::irgen::value::ContainerKind::Ptr(ty.clone()),
            }
        };
        self.add_value_index(index, value);
    }
}
