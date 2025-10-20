use crate::{
    ast::{BindingMode, Mutability, NodeId, Symbol},
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        resolved_ty::{ResolvedTy, TypeIntern},
        value::{Value, ValueKind},
    },
};

#[derive(Debug)]
pub struct PatResult {
    pub bindings: Vec<Binding>,
}

#[derive(Debug)]
pub struct Binding(
    pub(crate) Symbol,
    pub(crate) Value,
    pub(crate) Mutability,
    pub(crate) NodeId,
);

#[derive(Debug)]
pub struct PatExtra {
    pub(crate) id: NodeId,
    pub(crate) ty: TypeIntern,
}

impl SemanticAnalyzer {
    pub(crate) fn visit_ident_pat_impl(
        &mut self,
        BindingMode(by_ref, mutbl): &crate::ast::BindingMode,
        ident: &crate::ast::Ident,
        extra: PatExtra,
    ) -> Result<PatResult, SemanticError> {
        let ty = match *by_ref {
            crate::ast::ByRef::Yes(mutability) => {
                self.intern_type(ResolvedTy::ref_type(extra.ty, mutability.into()))
            }
            crate::ast::ByRef::No => extra.ty.to_key(),
        };

        let value = Value {
            ty: ty.into(),
            kind: ValueKind::Binding(*by_ref),
        };

        let bindings = vec![Binding(ident.symbol.clone(), value, *mutbl, extra.id)];

        Ok(PatResult { bindings })
    }
}
