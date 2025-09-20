use crate::semantics::{SemanticAnalyzer, resolved_ty::ResolvedTy, utils::TypeKind};

impl SemanticAnalyzer {
    pub(crate) fn has_sized_trait(&self, ty: &ResolvedTy) -> bool {
        if *ty == ResolvedTy::str() {
            return false;
        }
        match ty {
            // 暂时认为 Struct 中只能以 sized type 为成员
            ResolvedTy::Slice(_) => false,
            _ => true,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn has_copy_trait(&self, ty: &ResolvedTy) -> bool {
        if ty.is_number_type() || *ty == ResolvedTy::char() || *ty == ResolvedTy::bool() {
            return true;
        }

        match ty {
            ResolvedTy::Named(full_name) => {
                let info = self.get_type_info(full_name);

                match &info.kind {
                    TypeKind::Placeholder | TypeKind::Trait { .. } | TypeKind::Struct { .. } => {
                        false
                    }
                    TypeKind::Enum { .. } => true,
                }
            }
            ResolvedTy::Ref(_, mutability) => mutability.is_not(),
            ResolvedTy::Never => true,
            _ => false,
        }
    }
}
