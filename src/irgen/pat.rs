use crate::{
    ast::{BindingMode, ByRef},
    irgen::{IRGenerator, extra::PatExtra},
    semantics::value::{PlaceValueIndex, ValueIndex, ValueIndexKind},
};

impl<'analyzer> IRGenerator<'analyzer> {
    pub(crate) fn visit_ident_pat_impl(
        &mut self,
        BindingMode(by_ref, _): &BindingMode,
        ident: &crate::ast::Ident,
        PatExtra { ptr, self_id }: PatExtra,
    ) {
        let index = ValueIndex::Place(PlaceValueIndex {
            name: ident.symbol.clone(),
            kind: ValueIndexKind::Bindings {
                binding_id: self_id,
            },
        });
        let value = if matches!(by_ref, ByRef::Yes(_)) {
            self.get_value_ptr(ptr)
        } else {
            ptr
        };
        self.add_value_index(index, value);
    }
}
