use std::{collections::HashMap, rc::Rc};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, Symbol},
    make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        item::AssociatedInfo,
        resolved_ty::{RefMutability, ResolvedTy, ResolvedTyInstance, TypeKey},
        value::{MethodKind, PlaceValue, Value, ValueKind},
    },
};

#[derive(Debug, Default)]
pub struct Impls<'ast> {
    pub(crate) inherent: ImplInfo<'ast>,
    pub(crate) traits: HashMap<Rc<ResolvedTyInstance>, ImplInfo<'ast>>,
}

// Constant 和 Function 共享一个命名空间
#[derive(Debug, Default)]
pub struct ImplInfo<'ast> {
    pub(crate) values: HashMap<Symbol, PlaceValue<'ast>>,
}

impl<'ast> SemanticAnalyzer<'ast> {
    pub fn get_impls(&self, ty: &TypeKey) -> Option<&Impls<'ast>> {
        let instance = self.probe_type_instance((*ty).into()).unwrap();
        self.get_impls_by_instance(&instance)
    }

    pub fn get_impls_by_instance(&self, instance: &ResolvedTyInstance) -> Option<&Impls<'ast>> {
        self.impls.get(instance)
    }

    pub fn get_impls_mut(&mut self, ty: &TypeKey) -> &mut Impls<'ast> {
        let instance = self.probe_type_instance((*ty).into()).unwrap();
        self.get_impls_by_instance_mut(&instance)
    }

    pub fn get_impls_by_instance_mut(&mut self, instance: &ResolvedTyInstance) -> &mut Impls<'ast> {
        if !self.impls.contains_key(instance) {
            let mut inherent = ImplInfo::default();
            if instance.kind.is_array() {
                let inner_ty = self.new_any_type();
                let ref_ty = self.intern_type(ResolvedTy::ref_type(inner_ty, RefMutability::Not));
                let len_ty =
                    self.intern_type(ResolvedTy::fn_type(self.usize_type(), vec![ref_ty.into()]));
                inherent.values.insert(
                    Symbol::from("len"),
                    PlaceValue {
                        value: Value {
                            ty: len_ty.into(),
                            kind: ValueKind::Fn {
                                method_kind: MethodKind::ByRef,
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

        self.impls.get_mut(instance).unwrap()
    }

    pub fn get_impl_for_trait(&self, ty: &TypeKey, trait_ty: &TypeKey) -> Option<&ImplInfo<'ast>> {
        let impls = self.get_impls(ty)?;
        let instance = self.probe_type_instance((*trait_ty).into()).unwrap();
        impls.traits.get(&instance)
    }

    pub fn get_impl_for_trait_mut(&mut self, ty: &TypeKey, trait_ty: &TypeKey) -> &mut ImplInfo<'ast> {
        let instance = self.probe_type_instance((*trait_ty).into()).unwrap();
        let impls = self.get_impls_mut(ty);
        if !impls.traits.contains_key(&instance) {
            impls
                .traits
                .insert(instance.clone().into(), ImplInfo::default());
        }
        impls.traits.get_mut(&instance).unwrap()
    }

    pub fn add_impl_value(
        &mut self,
        AssociatedInfo { ty, for_trait, .. }: &AssociatedInfo,
        name: &Symbol,
        value: PlaceValue<'ast>,
    ) -> Result<&mut PlaceValue<'ast>, SemanticError> {
        let info = if let Some(t) = for_trait {
            self.get_impl_for_trait_mut(ty, t)
        } else {
            &mut self.get_impls_mut(ty).inherent
        };
        let replace = info.values.insert(name.clone(), value);
        if replace.is_some() {
            Err(make_semantic_error!(ValueDefineConflict))
        } else {
            Ok(info.values.get_mut(name).unwrap())
        }
    }

    pub fn get_impl_value(
        &self,
        AssociatedInfo { ty, for_trait, .. }: &AssociatedInfo,
        name: &Symbol,
    ) -> Option<&PlaceValue<'ast>> {
        let info = if let Some(t) = for_trait {
            self.get_impl_for_trait(ty, t)?
        } else {
            &self.get_impls(ty)?.inherent
        };
        info.values.get(name)
    }

    pub fn get_impl_value_mut(
        &mut self,
        AssociatedInfo { ty, for_trait, .. }: &AssociatedInfo,
        name: &Symbol,
    ) -> Option<&mut PlaceValue<'ast>> {
        let info = if let Some(t) = for_trait {
            self.get_impl_for_trait_mut(ty, t)
        } else {
            &mut self.get_impls_mut(ty).inherent
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
