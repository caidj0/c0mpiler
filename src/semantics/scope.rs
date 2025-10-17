use std::collections::{HashMap, HashSet};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{NodeId, Symbol},
    semantics::{analyzer::SemanticAnalyzer, resolved_ty::TypePtr, value::Value},
};

// TODO: Associated Item 如何处理？Impl 和 Trait 有 scope，但是它们的 item 不保存 scope 里
#[derive(Debug)]
pub struct Scope {
    pub id: NodeId,
    pub kind: ScopeKind,
    pub types: HashMap<Symbol, TypePtr>,
    pub values: HashMap<Symbol, Value>,
    pub bindings: HashMap<Symbol, NodeId>,
    pub children: HashSet<NodeId>,
    pub father: NodeId,
}

#[derive(Debug, EnumAsInner)]
pub enum ScopeKind {
    Lambda,
    Root,
    Crate,
    Trait(TypePtr),
    Impl {
        ty: TypePtr,
        for_trait: Option<TypePtr>,
    },
    Struct(TypePtr, Vec<Symbol>),
    Enum(TypePtr, Vec<Symbol>),
    Fn {
        ret_ty: TypePtr,
        main_fn: MainFunctionState,
    },
    Loop {
        ret_ty: TypePtr,
    },
    CycleExceptLoop,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum MainFunctionState {
    Not,
    UnExited,
    Exited,
}

pub struct ScopeSearchResult {
    pub(crate) father: NodeId,
    pub(crate) kind: ScopeSearchResultKind,
}

pub enum ScopeSearchResultKind {
    Type(TypePtr),
}

impl SemanticAnalyzer {
    pub fn search_scope_or_type(
        &self,
        symbol: &Symbol,
        start_scope: NodeId,
    ) -> Option<ScopeSearchResult> {
        let mut scope_id = Some(start_scope);

        while let Some(id) = scope_id {
            if let Some(ty) = self.get_type(id, symbol) {
                return Some(ScopeSearchResult {
                    father: id,
                    kind: ScopeSearchResultKind::Type(ty),
                });
            }
            scope_id = self.get_parent_scope(id);
        }
        None
    }
}
