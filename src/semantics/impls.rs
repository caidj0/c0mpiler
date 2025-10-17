use std::collections::HashMap;

use crate::{
    ast::{NodeId, Symbol},
    make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer, error::SemanticError, item::AssociatedInfo,
        resolved_ty::TypePtr, value::Value,
    },
};

#[derive(Debug)]
pub struct Impls {
    pub(crate) inherent: ImplInfo,
    pub(crate) traits: HashMap<TypePtr, ImplInfo>,
}

// Constant 和 Function 共享一个命名空间
#[derive(Debug)]
pub struct ImplInfo {
    pub(crate) values: HashMap<Symbol, Value>,
}

impl SemanticAnalyzer {
    pub fn add_impl_value(
        &mut self,
        AssociatedInfo { ty, for_trait, .. }: &AssociatedInfo,
        name: &Symbol,
        value: Value,
    ) -> Result<&mut Value, SemanticError> {
        let impls = self.impls.get_mut(ty).unwrap();
        let info = if let Some(t) = for_trait {
            impls.traits.get_mut(t).unwrap()
        } else {
            &mut impls.inherent
        };
        let replace = info.values.insert(name.clone(), value);
        if replace.is_some() {
            Err(make_semantic_error!(ValueDefineConflict))
        } else {
            Ok(info.values.get_mut(name).unwrap())
        }
    }

    pub fn get_impl_value_mut(
        &mut self,
        AssociatedInfo { ty, for_trait, .. }: &AssociatedInfo,
        name: &Symbol,
    ) -> Option<&mut Value> {
        let impls = self.impls.get_mut(ty).unwrap();
        let info = if let Some(t) = for_trait {
            impls.traits.get_mut(t).unwrap()
        } else {
            &mut impls.inherent
        };
        info.values.get_mut(name)
    }
}
