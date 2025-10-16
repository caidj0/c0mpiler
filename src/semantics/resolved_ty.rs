use std::{cell::RefCell, hash::Hash, rc::Rc};

use enum_as_inner::EnumAsInner;

use crate::semantics::utils::FullName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypePtr(pub(crate) Rc<RefCell<ResolvedTy>>);

impl TypePtr {
    pub fn deep_clone(&self) -> Self {
        Self(RefCell::new(self.0.borrow().deep_clone()).into())
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub fn borrow_mut(&self) -> std::cell::RefMut<'_, ResolvedTy> {
        self.0.borrow_mut()
    }

    pub fn borrow(&self) -> std::cell::Ref<'_, ResolvedTy> {
        self.0.borrow()
    }
}

impl Hash for TypePtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedTy {
    pub name: Option<FullName>,
    pub kind: ResolvedTyKind,
}

impl ResolvedTy {
    pub fn deep_clone(&self) -> Self {
        let name = self.name.clone();
        let kind = match &self.kind {
            ResolvedTyKind::BuiltIn(built_in_ty_kind) => {
                ResolvedTyKind::BuiltIn(built_in_ty_kind.clone())
            }
            ResolvedTyKind::Ref(type_ptr, ref_mutability) => {
                ResolvedTyKind::Ref(type_ptr.deep_clone(), ref_mutability.clone())
            }
            ResolvedTyKind::Struct(type_ptrs) => {
                ResolvedTyKind::Struct(type_ptrs.iter().map(|x| x.deep_clone()).collect())
            }
            ResolvedTyKind::Enum => ResolvedTyKind::Enum,
            ResolvedTyKind::Array(type_ptr, len) => {
                ResolvedTyKind::Array(type_ptr.deep_clone(), len.clone())
            }
            ResolvedTyKind::Fn(type_ptr, type_ptrs) => ResolvedTyKind::Fn(
                type_ptr.deep_clone(),
                type_ptrs.iter().map(|x| x.deep_clone()).collect(),
            ),
            ResolvedTyKind::ImplicitSelf => ResolvedTyKind::ImplicitSelf,
            ResolvedTyKind::Never => ResolvedTyKind::Never,
            ResolvedTyKind::Trait => ResolvedTyKind::Trait,
            ResolvedTyKind::Any(any_ty_kind) => ResolvedTyKind::Any(any_ty_kind.clone()),
        };

        Self { name, kind }
    }
}

#[derive(Debug, Clone, EnumAsInner, PartialEq, Eq, Hash)]
pub enum ResolvedTyKind {
    BuiltIn(BuiltInTyKind),
    Ref(TypePtr, RefMutability),
    Struct(Vec<TypePtr>),
    Enum,
    Trait,
    Array(TypePtr, u32),
    Fn(TypePtr, Vec<TypePtr>),
    ImplicitSelf,
    Never,
    Any(AnyTyKind),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltInTyKind {
    Bool,
    Char,
    I32,
    ISize,
    U32,
    USize,
    Str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RefMutability {
    Not,
    Mut,
    WeakMut,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyTyKind {
    Any,
    AnyInt,
    AnySignedInt,
}

impl AnyTyKind {
    pub fn can_cast_to(&self, target: &ResolvedTyKind) -> bool {
        use ResolvedTyKind::*;

        match (self, target) {
            (AnyTyKind::Any, _) => true,
            (
                AnyTyKind::AnyInt | AnyTyKind::AnySignedInt,
                BuiltIn(BuiltInTyKind::I32 | BuiltInTyKind::ISize) | Any(AnyTyKind::AnySignedInt),
            ) => true,
            (
                AnyTyKind::AnyInt,
                BuiltIn(BuiltInTyKind::U32 | BuiltInTyKind::USize) | Any(AnyTyKind::AnyInt),
            ) => true,
            _ => false,
        }
    }
}
