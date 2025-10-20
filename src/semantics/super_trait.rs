use crate::{
    make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        resolved_ty::{AnyTyKind, BuiltInTyKind, TypeIntern},
    },
};

impl SemanticAnalyzer {
    pub(crate) fn check_sized(&self, ty: TypeIntern) -> Result<(), SemanticError> {
        let Some(probe) = self.probe_type(ty) else {
            return Err(make_semantic_error!(NotSizedType));
        };

        use super::resolved_ty::ResolvedTyKind::*;
        match probe.kind {
            BuiltIn(BuiltInTyKind::Str) | Trait | Fn(_, _) | ImplicitSelf(_) => {
                Err(make_semantic_error!(NotSizedType))
            }
            Any(AnyTyKind::AnyInt | AnyTyKind::AnySignedInt) => Ok(()),
            Any(_) => todo!(),
            _ => Ok(()),
        }
    }
}
