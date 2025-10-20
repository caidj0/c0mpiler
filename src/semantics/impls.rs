use std::collections::HashMap;

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, Symbol},
    make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        item::AssociatedInfo,
        resolved_ty::{RefMutability, ResolvedTy, TypeKey},
        value::{PlaceValue, Value, ValueKind},
    },
};

#[derive(Debug, Default)]
pub struct Impls {
    pub(crate) inherent: ImplInfo,
    pub(crate) traits: HashMap<TypeKey, ImplInfo>, // Trait 的 key 应是唯一的，从而只需以 TypeKey 作为键
}

// Constant 和 Function 共享一个命名空间
#[derive(Debug, Default)]
pub struct ImplInfo {
    pub(crate) values: HashMap<Symbol, PlaceValue>,
}

impl SemanticAnalyzer {
    pub fn get_impls(&self, ty: &TypeKey) -> &Impls {
        let instance = self.probe_type_instance((*ty).into()).unwrap();
        self.impls.get(&instance).unwrap()
    }

    pub fn get_impls_mut(&mut self, ty: &TypeKey) -> &mut Impls {
        let instance = self.probe_type_instance((*ty).into()).unwrap();
        if !self.impls.contains_key(&instance) {
            let mut inherent = ImplInfo::default();
            if instance.kind.is_array() {
                let ref_ty =
                    self.intern_type(ResolvedTy::ref_type((*ty).into(), RefMutability::Not));
                let len_ty =
                    self.intern_type(ResolvedTy::fn_type(self.u32_type(), vec![ref_ty.into()]));
                inherent.values.insert(
                    Symbol::from("len"),
                    PlaceValue {
                        value: Value {
                            ty: len_ty.into(),
                            kind: ValueKind::Fn {
                                is_method: true,
                                is_placeholder: false,
                            },
                        },
                        mutbl: Mutability::Not,
                    },
                );
            }

            self.impls.insert(
                instance.clone(),
                Impls {
                    inherent,
                    ..Default::default()
                },
            );
        }

        self.impls.get_mut(&instance).unwrap()
    }

    pub fn add_impl_value(
        &mut self,
        AssociatedInfo { ty, for_trait, .. }: &AssociatedInfo,
        name: &Symbol,
        value: PlaceValue,
    ) -> Result<&mut PlaceValue, SemanticError> {
        let impls = self.get_impls_mut(ty);
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
    ) -> Option<&mut PlaceValue> {
        let impls = self.get_impls_mut(ty);
        let info = if let Some(t) = for_trait {
            impls.traits.get_mut(t).unwrap()
        } else {
            &mut impls.inherent
        };
        info.values.get_mut(name)
    }
}

#[derive(Debug, Default, Clone, EnumAsInner)]
pub enum DerefLevel {
    #[default]
    Not,
    Deref(Box<DerefLevel>, Mutability),
}

// 解引用时变量的 AssigneeKind：
impl DerefLevel {
    pub fn wrap(&mut self, mutbl: Mutability) {
        *self = DerefLevel::Deref(Box::new(std::mem::take(self)), mutbl)
    }

    pub fn chain_mutbl(&self, raw_kind: Mutability) -> Mutability {
        match self {
            DerefLevel::Not => raw_kind,
            DerefLevel::Deref(deref_level, mutability) => {
                let mut mutbl = *mutability;
                let mut inner = Some(deref_level);
                while let Some(i) = inner {
                    match i.as_ref() {
                        DerefLevel::Not => {
                            inner = None;
                        }
                        DerefLevel::Deref(deref_level, mutability) => {
                            mutbl = mutbl & *mutability;
                            inner = Some(deref_level);
                        }
                    }
                }
                mutbl
            }
        }
    }
}
