use crate::{
    ast::{NodeId, Symbol},
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        resolved_ty::TypePtr,
        value::{Value, ValueKind},
    },
};

#[derive(Debug)]
pub struct PatResult {
    pub bindings: Vec<Binding>,
}

#[derive(Debug)]
pub struct Binding(pub(crate) Symbol, pub(crate) Value, pub(crate) NodeId);

#[derive(Debug)]
pub struct PatExtra<'tmp> {
    pub(crate) id: NodeId,
    pub(crate) ty: &'tmp mut TypePtr,
}

impl SemanticAnalyzer {
    pub(crate) fn visit_ident_pat_impl(
        mode: &crate::ast::BindingMode,
        ident: &crate::ast::Ident,
        extra: PatExtra<'_>,
    ) -> Result<PatResult, SemanticError> {
        let ty = match mode.0 {
            crate::ast::ByRef::Yes(mutability) => {
                Self::ref_type(extra.ty.clone(), mutability.into())
            }
            crate::ast::ByRef::No => extra.ty.clone(),
        };

        let value = Value {
            ty,
            mutbl: mode.1,
            kind: ValueKind::Binding(mode.0),
        };

        let bindings = vec![Binding(ident.symbol.clone(), value, extra.id)];

        Ok(PatResult { bindings })
    }
}
