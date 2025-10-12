use crate::{
    ast::NodeId,
    semantics::{SemanticAnalyzer, resolved_ty::ResolvedTy, utils::TypeKind},
};

impl TypeKind {
    pub fn is_empty_length(&self, analyzer: &SemanticAnalyzer, from: Option<NodeId>) -> bool {
        match self {
            TypeKind::Placeholder => panic!("Impossible"),
            TypeKind::Struct { fields } => {
                fields.values().all(|x| x.is_empty_length(analyzer, from))
            }
            TypeKind::Enum { fields } => fields.len() == 0,
            TypeKind::Trait { .. } => panic!("Impossible"),
        }
    }
}

impl ResolvedTy {
    pub fn is_empty_length(&self, analyzer: &SemanticAnalyzer, from: Option<NodeId>) -> bool {
        match self {
            ResolvedTy::BuiltIn(..) => false,
            ResolvedTy::Named(full_name) => analyzer
                .get_type_info(full_name)
                .kind
                .is_empty_length(analyzer, from),
            ResolvedTy::Ref(_, _) => false,
            ResolvedTy::Array(resolved_ty, len) => {
                *len == 0 || resolved_ty.is_empty_length(analyzer, from)
            }
            ResolvedTy::Slice(resolved_ty) => resolved_ty.is_empty_length(analyzer, from),
            ResolvedTy::Tup(items) => items.iter().all(|x| x.is_empty_length(analyzer, from)),
            ResolvedTy::Fn(_, _) => false,
            ResolvedTy::ImplicitSelf => analyzer
                .get_self_type_from(from.unwrap())
                .unwrap()
                .is_empty_length(analyzer, from),
            ResolvedTy::Never => true,
        }
    }
}
