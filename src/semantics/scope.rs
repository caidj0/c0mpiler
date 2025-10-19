use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{NodeId, Symbol},
    make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        resolved_ty::TypeKey,
        value::{PlaceValue, Value},
    },
};

// TODO: Associated Item 如何处理？Impl 和 Trait 有 scope，但是它们的 item 不保存 scope 里
#[derive(Debug)]
pub struct Scope {
    pub id: NodeId,
    pub kind: ScopeKind,
    pub types: HashMap<Symbol, TypeKey>,
    pub values: HashMap<Symbol, PlaceValue>,
    pub bindings: HashMap<Symbol, NodeId>,
    pub children: HashSet<NodeId>,
    pub father: NodeId,
}

#[derive(Debug, EnumAsInner)]
pub enum ScopeKind {
    Lambda,
    Root,
    Crate,
    Trait(TypeKey),
    Impl {
        ty: TypeKey,
        for_trait: Option<TypeKey>,
    },
    Struct(TypeKey),
    Enum(TypeKey),
    Fn {
        ret_ty: TypeKey,
        main_fn: MainFunctionState,
    },
    Loop {
        ret_ty: TypeKey,
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
    Type(TypeKey),
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

    pub fn search_cycle_scope(&self, mut current_scope: NodeId) -> Result<NodeId, SemanticError> {
        loop {
            let scope = self.get_scope(current_scope);
            match &scope.kind {
                ScopeKind::Lambda => {}
                ScopeKind::Root
                | ScopeKind::Trait(_)
                | ScopeKind::Crate
                | ScopeKind::Impl { .. }
                | ScopeKind::Struct(_)
                | ScopeKind::Enum(_)
                | ScopeKind::Fn { .. } => return Err(make_semantic_error!(NotInCycleScope)),
                ScopeKind::Loop { .. } | ScopeKind::CycleExceptLoop => return Ok(current_scope),
            }
            current_scope = self.get_parent_scope(current_scope).unwrap();
        }
    }

    pub fn search_fn_scope(&self, mut current_scope: NodeId) -> Result<NodeId, SemanticError> {
        loop {
            let scope = self.get_scope(current_scope);
            match &scope.kind {
                ScopeKind::Lambda | ScopeKind::Loop { .. } | ScopeKind::CycleExceptLoop => {}
                ScopeKind::Root
                | ScopeKind::Crate
                | ScopeKind::Trait(..)
                | ScopeKind::Impl { .. }
                | ScopeKind::Struct(..)
                | ScopeKind::Enum(..) => return Err(make_semantic_error!(NotInFnScope)),
                ScopeKind::Fn { .. } => return Ok(current_scope),
            }
            current_scope = self.get_parent_scope(current_scope).unwrap();
        }
    }
}
