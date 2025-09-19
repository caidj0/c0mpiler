use std::vec;

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, Symbol},
    semantics::utils::{DerefLevel, FullName},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum ResolvedTy {
    BuiltIn(Symbol, Vec<ResolvedTy>),
    Named(FullName),
    Ref(Box<ResolvedTy>, Mutability),
    Array(Box<ResolvedTy>, u32),
    Slice(Box<ResolvedTy>),
    Tup(Vec<ResolvedTy>),
    Fn(Vec<ResolvedTy>, Box<ResolvedTy>),
    ImplicitSelf,
    Infer, // for underscore
    Never,
}

impl ResolvedTy {
    pub fn unit() -> Self {
        Self::Tup(Vec::new())
    }

    pub fn bool() -> Self {
        Self::BuiltIn(Symbol("bool".to_string()), Vec::new())
    }

    pub fn char() -> Self {
        Self::BuiltIn(Symbol("char".to_string()), Vec::new())
    }

    pub fn integer() -> Self {
        Self::BuiltIn(Symbol("integer".to_string()), Vec::new())
    }

    pub fn signed_integer() -> Self {
        Self::BuiltIn(Symbol("integer".to_string()), Vec::new())
    }

    pub fn i32() -> Self {
        Self::BuiltIn(Symbol("i32".to_string()), Vec::new())
    }

    pub fn u32() -> Self {
        Self::BuiltIn(Symbol("u32".to_string()), Vec::new())
    }

    pub fn isize() -> Self {
        Self::BuiltIn(Symbol("isize".to_string()), Vec::new())
    }

    pub fn usize() -> Self {
        Self::BuiltIn(Symbol("usize".to_string()), Vec::new())
    }

    pub fn str() -> Self {
        Self::BuiltIn(Symbol("str".to_string()), Vec::new())
    }

    pub fn ref_str() -> Self {
        Self::Ref(Box::new(Self::str()), Mutability::Not)
    }

    pub fn string() -> Self {
        Self::BuiltIn(Symbol("String".to_string()), Vec::new())
    }

    pub fn big_self() -> Self {
        Self::Named(FullName(vec![Symbol("Self".to_string())]))
    }

    pub fn implicit_self() -> Self {
        Self::ImplicitSelf
    }

    pub fn ref_implicit_self() -> Self {
        Self::Ref(Box::new(Self::implicit_self()), Mutability::Not)
    }

    pub fn ref_mut_implicit_self() -> Self {
        Self::Ref(Box::new(Self::implicit_self()), Mutability::Mut)
    }

    pub fn try_deref(self) -> (Self, DerefLevel) {
        if let ResolvedTy::Ref(resolved, mutbl) = self {
            (*resolved, DerefLevel::Deref(mutbl))
        } else {
            (self, DerefLevel::Not)
        }
    }

    pub fn deref_all(self) -> (Self, DerefLevel) {
        let mut ret_ty = self;
        let mut level = DerefLevel::Not;

        while let ResolvedTy::Ref(resolved, mutbl) = ret_ty {
            ret_ty = *resolved;
            level = level.merge(DerefLevel::Deref(mutbl))
        }

        (ret_ty, level)
    }

    pub fn is_number_type(&self) -> bool {
        *self == Self::integer()
            || *self == Self::i32()
            || *self == Self::u32()
            || *self == Self::usize()
            || *self == Self::isize()
            || *self == Self::signed_integer()
    }

    pub fn is_signed_number_type(&self) -> bool {
        *self == Self::i32() || *self == Self::isize() || *self == Self::signed_integer()
    }

    pub fn is_implicit_self_or_ref_implicit_self(&self) -> bool {
        match self {
            ResolvedTy::Ref(resolved_ty, _) => {
                matches!(resolved_ty.as_ref(), ResolvedTy::ImplicitSelf)
            }
            ResolvedTy::ImplicitSelf => true,
            _ => false,
        }
    }

    pub fn is_method(&self) -> bool {
        match self {
            ResolvedTy::Fn(tys, _) => tys
                .first()
                .map(|x| x.is_implicit_self_or_ref_implicit_self())
                .unwrap_or(false),
            _ => false,
        }
    }

    pub fn expand_self(&self, self_ty: &Self) -> Self {
        if *self == Self::big_self() {
            self_ty.clone()
        } else {
            match self {
                ResolvedTy::Ref(resolved_ty, mutability) => {
                    ResolvedTy::Ref(Box::new(resolved_ty.expand_self(self_ty)), *mutability)
                }
                ResolvedTy::Array(resolved_ty, len) => {
                    ResolvedTy::Array(Box::new(resolved_ty.expand_self(self_ty)), *len)
                }
                ResolvedTy::Slice(resolved_ty) => {
                    ResolvedTy::Slice(Box::new(resolved_ty.expand_self(self_ty)))
                }
                ResolvedTy::Tup(items) => {
                    ResolvedTy::Tup(items.iter().map(|x| x.expand_self(self_ty)).collect())
                }
                ResolvedTy::Fn(items, resolved_ty) => ResolvedTy::Fn(
                    items.iter().map(|x| x.expand_self(self_ty)).collect(),
                    Box::new(resolved_ty.expand_self(self_ty)),
                ),
                ResolvedTy::ImplicitSelf => self_ty.clone(),
                _ => self.clone(),
            }
        }
    }

    pub fn method_to_func(&self, self_ty: &Self) -> Self {
        let mut ret = self.clone();
        let ResolvedTy::Fn(tys, _) = &mut ret else {
            panic!("Impossible!");
        };

        let first = tys.first_mut().unwrap();

        match first {
            ResolvedTy::Ref(resolved_ty, _) => {
                debug_assert!(matches!(resolved_ty.as_ref(), ResolvedTy::ImplicitSelf));
                *resolved_ty = Box::new(self_ty.clone())
            }
            ResolvedTy::ImplicitSelf => *first = self_ty.clone(),
            _ => panic!("Impossible!"),
        }

        ret
    }

    pub fn r#ref(self) -> Self {
        Self::Ref(Box::new(self), Mutability::Not)
    }

    // 包括相等类型 + 其他情况
    pub fn can_trans_to_target_type(&self, target: &Self) -> bool {
        if self == target {
            return true;
        }

        match (self, target) {
            (ResolvedTy::BuiltIn(_, _), ResolvedTy::BuiltIn(_, _)) => {
                (*self == ResolvedTy::integer() && target.is_number_type())
                    || (*self == ResolvedTy::signed_integer() && target.is_signed_number_type())
            }
            (ResolvedTy::Named(_), ResolvedTy::Named(_)) => false,
            (
                ResolvedTy::Ref(resolved_ty1, mutability1),
                ResolvedTy::Ref(resolved_ty2, mutability2),
            ) => match (mutability1, mutability2) {
                (Mutability::Not, Mutability::Mut) => false,
                _ => resolved_ty1.can_trans_to_target_type(resolved_ty2),
            },
            (ResolvedTy::Array(resolved_ty1, len1), ResolvedTy::Array(resolved_ty2, len2)) => {
                if len1 == len2 {
                    resolved_ty1.can_trans_to_target_type(resolved_ty2)
                } else {
                    false
                }
            }
            (ResolvedTy::Slice(resolved_ty1), ResolvedTy::Slice(resolved_ty2)) => {
                resolved_ty1.can_trans_to_target_type(resolved_ty2)
            }
            (ResolvedTy::Tup(items1), ResolvedTy::Tup(items2)) => {
                if items1.len() != items2.len() {
                    return false;
                }
                for (x, y) in items1.iter().zip(items2.iter()) {
                    if !x.can_trans_to_target_type(y) {
                        return false;
                    }
                }
                true
            }
            (ResolvedTy::Fn(_, _), ResolvedTy::Fn(_, _)) => {
                unimplemented!()
            }
            (ResolvedTy::ImplicitSelf, ResolvedTy::ImplicitSelf) => {
                panic!("Impossible")
            }
            (_, ResolvedTy::Infer) => true,
            (ResolvedTy::Infer, _) => true, // TODO: Infer 类型到底如何变化？真的可以随意推导吗？
            (ResolvedTy::Never, _) => true,
            _ => false,
        }
    }

    pub fn utilize(types: Vec<ResolvedTy>) -> Option<ResolvedTy> {
        if types.is_empty() {
            return Some(ResolvedTy::Infer);
        }

        let mut iter = types.into_iter();
        let mut ret_ty = iter.next().unwrap();
        for x in iter {
            if !x.can_trans_to_target_type(&ret_ty) {
                if ret_ty.can_trans_to_target_type(&x) {
                    ret_ty = x;
                } else {
                    return None;
                }
            }
        }

        Some(ret_ty)
    }
}
