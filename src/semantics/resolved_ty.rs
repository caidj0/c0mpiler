use std::{cell::RefCell, rc::Rc};

use enum_as_inner::EnumAsInner;

use crate::semantics::utils::FullName;

pub type TypePtr = Rc<RefCell<ResolvedTy>>;

#[derive(Debug, Clone)]
pub struct ResolvedTy {
    pub name: Option<FullName>,
    pub kind: ResolvedTyKind,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ResolvedTyKind {
    BuiltIn(BuiltInTyKind),
    Ref(TypePtr, RefMutability),
    Struct(Vec<TypePtr>),
    Enum,
    Array(TypePtr, u32),
    Fn(TypePtr, Vec<TypePtr>),
    ImplicitSelf,
    Never,
    Any(AnyTyKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltInTyKind {
    Bool,
    Char,
    I32,
    ISize,
    U32,
    USize,
    Str,
}

#[derive(Debug, Clone, Copy)]
pub enum RefMutability {
    Not,
    Mut,
    WeakMut,
}

#[derive(Debug, Clone)]
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
