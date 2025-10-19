use crate::{
    ast::{BindingMode, Mutability, NodeId, Symbol},
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        resolved_ty::TypeKey,
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
pub struct PatExtra<'tmp> {
    pub(crate) id: NodeId,
    pub(crate) ty: &'tmp mut TypeKey,
}

impl SemanticAnalyzer {
    pub(crate) fn visit_ident_pat_impl(
        BindingMode(by_ref, mutbl): &crate::ast::BindingMode,
        ident: &crate::ast::Ident,
        extra: PatExtra<'_>,
    ) -> Result<PatResult, SemanticError> {
        let ty = match *by_ref {
            crate::ast::ByRef::Yes(mutability) => {
                Self::ref_type(extra.ty.clone(), mutability.into())
            }
            crate::ast::ByRef::No => extra.ty.clone(),
        };

        let value = Value {
            ty,
            kind: ValueKind::Binding(*by_ref),
        };

        let bindings = vec![Binding(ident.symbol.clone(), value, *mutbl, extra.id)];

        Ok(PatResult { bindings })
    }
}
