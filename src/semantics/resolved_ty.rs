use std::{collections::HashSet, hash::Hash, rc::Rc, vec};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, Symbol},
    semantics::utils::{DerefLevel, FullName},
};

pub type TypePtr = Rc<ResolvedTy>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum ResolvedTy {
    BuiltIn(Symbol, Vec<TypePtr>),
    Named(FullName),
    Ref(TypePtr, Mutability),
    Array(TypePtr, u32),
    Slice(TypePtr),
    Tup(Vec<TypePtr>),
    Fn(Vec<TypePtr>, TypePtr),
    ImplicitSelf,
    Never,
}

#[allow(dead_code)]
impl ResolvedTy {
    fn unit() -> Self {
        Self::Tup(Vec::new())
    }

    fn bool() -> Self {
        Self::BuiltIn(Symbol("bool".to_string()), Vec::new())
    }

    fn char() -> Self {
        Self::BuiltIn(Symbol("char".to_string()), Vec::new())
    }

    fn i32() -> Self {
        Self::BuiltIn(Symbol("i32".to_string()), Vec::new())
    }

    fn u32() -> Self {
        Self::BuiltIn(Symbol("u32".to_string()), Vec::new())
    }

    fn isize() -> Self {
        Self::BuiltIn(Symbol("isize".to_string()), Vec::new())
    }

    fn usize() -> Self {
        Self::BuiltIn(Symbol("usize".to_string()), Vec::new())
    }

    fn str() -> Self {
        Self::BuiltIn(Symbol("str".to_string()), Vec::new())
    }

    fn ref_str() -> Self {
        Self::Ref(Rc::new(Self::str()), Mutability::Not)
    }

    fn string() -> Self {
        Self::BuiltIn(Symbol("String".to_string()), Vec::new())
    }

    fn big_self() -> Self {
        Self::Named(FullName(vec![Symbol("Self".to_string())]))
    }

    fn implicit_self() -> Self {
        Self::ImplicitSelf
    }

    fn ref_implicit_self() -> Self {
        Self::Ref(Rc::new(Self::implicit_self()), Mutability::Not)
    }

    fn ref_mut_implicit_self() -> Self {
        Self::Ref(Rc::new(Self::implicit_self()), Mutability::Mut)
    }

    pub fn deref_once(&self) -> Option<(Rc<Self>, Mutability)> {
        if let ResolvedTy::Ref(resolved, mutbl) = self {
            Some((resolved.clone(), *mutbl))
        } else {
            None
        }
    }

    pub fn is_number_type(&self) -> bool {
        *self == Self::i32()
            || *self == Self::u32()
            || *self == Self::usize()
            || *self == Self::isize()
    }

    pub fn is_signed_number_type(&self) -> bool {
        *self == Self::i32() || *self == Self::isize()
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

    pub fn expand_self(self: &Rc<Self>, self_ty: Rc<Self>) -> Rc<Self> {
        if *self.as_ref() == Self::big_self() {
            self_ty
        } else {
            match self.as_ref() {
                ResolvedTy::Ref(resolved_ty, mutability) => Rc::new(ResolvedTy::Ref(
                    resolved_ty.expand_self(self_ty),
                    *mutability,
                )),
                ResolvedTy::Array(resolved_ty, len) => {
                    Rc::new(ResolvedTy::Array(resolved_ty.expand_self(self_ty), *len))
                }
                ResolvedTy::Slice(resolved_ty) => {
                    Rc::new(ResolvedTy::Slice(resolved_ty.expand_self(self_ty)))
                }
                ResolvedTy::Tup(items) => Rc::new(ResolvedTy::Tup(
                    items
                        .iter()
                        .map(|x| x.expand_self(self_ty.clone()))
                        .collect(),
                )),
                ResolvedTy::Fn(items, resolved_ty) => {
                    // fn 的首个参数若为 self，则不展开首个参数
                    let mut iter = items.iter();
                    let first = iter.next().map(|x| {
                        if x.is_implicit_self_or_ref_implicit_self() {
                            x.clone()
                        } else {
                            x.expand_self(self_ty.clone())
                        }
                    });
                    let params = first
                        .into_iter()
                        .chain(iter.map(|x| x.expand_self(self_ty.clone())))
                        .collect();
                    Rc::new(ResolvedTy::Fn(params, resolved_ty.expand_self(self_ty)))
                }
                ResolvedTy::ImplicitSelf => self_ty.clone(),
                _ => self.clone(),
            }
        }
    }

    pub fn method_to_func(&self, self_ty: &Rc<Self>) -> Self {
        let mut ret = self.clone();
        let ResolvedTy::Fn(tys, _) = &mut ret else {
            panic!("Impossible!");
        };

        let first = tys.first_mut().unwrap();

        match first.as_ref() {
            ResolvedTy::Ref(resolved_ty, mutbl) => {
                debug_assert!(matches!(resolved_ty.as_ref(), ResolvedTy::ImplicitSelf));
                *first = Rc::new(ResolvedTy::Ref(self_ty.clone(), *mutbl))
            }
            ResolvedTy::ImplicitSelf => *first = self_ty.clone(),
            _ => panic!("Impossible!"),
        }

        ret
    }

    // 包括相等类型 + 其他情况
    pub fn can_trans_to_target_type(&self, target: &Self, top_level: bool) -> bool {
        if self == target {
            return true;
        }

        match (self, target) {
            (
                ResolvedTy::Ref(resolved_ty1, mutability1),
                ResolvedTy::Ref(resolved_ty2, mutability2),
            ) => {
                if mutability1 == mutability2
                    || (top_level && mutability1.can_trans_to(mutability2))
                {
                    resolved_ty1.can_trans_to_target_type(resolved_ty2, false)
                } else {
                    false
                }
            }
            (ResolvedTy::Array(resolved_ty1, len1), ResolvedTy::Array(resolved_ty2, len2)) => {
                if len1 == len2 {
                    resolved_ty1.can_trans_to_target_type(resolved_ty2, false)
                } else {
                    false
                }
            }
            (ResolvedTy::Slice(resolved_ty1), ResolvedTy::Slice(resolved_ty2)) => {
                resolved_ty1.can_trans_to_target_type(resolved_ty2, false)
            }
            (ResolvedTy::Tup(items1), ResolvedTy::Tup(items2)) => {
                if items1.len() != items2.len() {
                    return false;
                }

                items1
                    .iter()
                    .zip(items2.iter())
                    .all(|(x, y)| x.can_trans_to_target_type(y, false))
            }
            (ResolvedTy::Fn(_, _), ResolvedTy::Fn(_, _)) => {
                unimplemented!()
            }
            (ResolvedTy::ImplicitSelf, ResolvedTy::ImplicitSelf) => {
                panic!("Impossible")
            }
            (ResolvedTy::Never, _) => true,
            _ => false,
        }
    }

    pub fn out_of_array(&self) -> Option<(TypePtr, DerefLevel)> {
        match self {
            ResolvedTy::Ref(resolved_ty, mutability) => resolved_ty
                .out_of_array()
                .map(|(a, b)| (a, b.merge(DerefLevel::Deref(*mutability)))),
            ResolvedTy::Array(resolved_ty, _) | ResolvedTy::Slice(resolved_ty) => {
                Some((resolved_ty.clone(), DerefLevel::Not))
            }
            _ => None,
        }
    }
}

macro_rules! prelude_pool {
    ($(($name:ident, $b:expr)),*) => {
        #[derive(Debug)]
        pub struct PreludePool {
            $(
                pub $name: TypePtr
            ),*
        }

        impl Default for PreludePool {
            fn default() -> Self {

                $(
                    let $name: TypePtr = $b.into();
                )*

                Self {
                    $(
                        $name
                    ),*
                }
            }
        }
    };
}

prelude_pool! {
    (unit, ResolvedTy::Tup(Vec::new())),
    (never, ResolvedTy::Never),
    (bool, ResolvedTy::BuiltIn(Symbol("bool".to_string()), Vec::new())),
    (char, ResolvedTy::BuiltIn(Symbol("char".to_string()), Vec::new())),
    (i32, ResolvedTy::BuiltIn(Symbol("i32".to_string()), Vec::new())),
    (u32, ResolvedTy::BuiltIn(Symbol("u32".to_string()), Vec::new())),
    (isize, ResolvedTy::BuiltIn(Symbol("isize".to_string()), Vec::new())),
    (usize, ResolvedTy::BuiltIn(Symbol("usize".to_string()), Vec::new())),
    (str, ResolvedTy::BuiltIn(Symbol("str".to_string()), Vec::new())),
    (string, ResolvedTy::BuiltIn(Symbol("String".to_string()), Vec::new())),
    (big_self, ResolvedTy::Named(FullName(vec![Symbol("Self".to_string())]))),
    (implicit_self, ResolvedTy::ImplicitSelf),

    (ref_str, ResolvedTy::Ref(str.clone(), Mutability::Not)),
    (ref_implicit_self, ResolvedTy::Ref(implicit_self.clone(), Mutability::Not)),
    (ref_mut_implicit_self, ResolvedTy::Ref(implicit_self.clone(), Mutability::Mut))
}

#[derive(Debug, EnumAsInner, Clone, PartialEq, Eq)]
pub enum ResolvedTypes {
    Types(HashSet<TypePtr>),
    Ref(Box<Self>, Mutability),
    Array(Box<Self>, u32),
    Infer,
    Never,
}

impl From<TypePtr> for ResolvedTypes {
    fn from(value: TypePtr) -> Self {
        Self::Types(HashSet::from([value]))
    }
}

impl From<HashSet<TypePtr>> for ResolvedTypes {
    fn from(value: HashSet<TypePtr>) -> Self {
        Self::Types(value)
    }
}

impl<const N: usize> From<[TypePtr; N]> for ResolvedTypes {
    fn from(value: [TypePtr; N]) -> Self {
        Self::Types(HashSet::from(value))
    }
}

impl FromIterator<TypePtr> for ResolvedTypes {
    fn from_iter<T: IntoIterator<Item = TypePtr>>(iter: T) -> Self {
        Self::Types(HashSet::from_iter(iter))
    }
}

impl ResolvedTypes {
    fn intersections(sets: Vec<HashSet<TypePtr>>) -> Option<HashSet<TypePtr>> {
        let mut iter = sets.into_iter();

        let mut intersection = iter.next()?;

        for other in iter {
            intersection.retain(|e| other.contains(e));
        }

        if intersection.is_empty() {
            None
        } else {
            Some(intersection)
        }
    }

    pub fn utilize(sets: Vec<Self>) -> Option<Self> {
        let has_never = sets.iter().any(|x| x.is_never());
        let has_infer = sets.iter().any(|x| x.is_infer());

        let sets: Vec<Self> = sets
            .into_iter()
            .filter(|x| !x.is_never() && !x.is_infer())
            .collect();

        if sets.is_empty() {
            if has_never {
                return Some(Self::Never);
            } else if has_infer {
                return Some(Self::Infer);
            } else {
                return None;
            }
        }

        let special = (|| {
            if sets.iter().all(|x| x.is_ref()) {
                let (inners, mutbls) = sets
                    .iter()
                    .map(|x| x.clone().into_ref().unwrap())
                    .collect::<(Vec<_>, Vec<_>)>();

                let mutbl = if mutbls.iter().all(|x| x.is_mut()) {
                    Mutability::Mut
                } else {
                    Mutability::Not
                };

                let inner = Self::utilize(inners.into_iter().map(|x| *x).collect())?;

                Some(Self::Ref(inner.into(), mutbl))
            } else if sets.iter().all(|x| x.is_array()) {
                let (inners, lens) = sets
                    .iter()
                    .map(|x| x.clone().into_array().unwrap())
                    .collect::<(Vec<_>, Vec<_>)>();

                let len = {
                    let mut iter = lens.into_iter();
                    let len = iter.next().unwrap();
                    if iter.any(|x| x != len) {
                        return None;
                    }
                    len
                };

                let inner = Self::utilize(inners.into_iter().map(|x| *x).collect())?;

                Some(Self::Array(inner.into(), len))
            } else {
                None
            }
        })();

        if special.is_none() {
            let inners = sets
                .into_iter()
                .map(|x| x.to_resolved_tys())
                .collect::<Vec<_>>();

            let inner = Self::intersections(inners)?;

            Some(Self::Types(inner))
        } else {
            special
        }
    }

    // 对于 ResolvedTypes -> ResolvedTypes 的情况，应用 utilize
    pub fn can_trans_to_target_type(&self, target: &Rc<ResolvedTy>) -> bool {
        match (self, target.as_ref()) {
            (Self::Types(types), _) => types
                .iter()
                .any(|x| x.can_trans_to_target_type(target, true)),
            (Self::Ref(inner, mutbl), ResolvedTy::Ref(target_inner, target_mutbl)) => {
                mutbl.can_trans_to(target_mutbl)
                    && (inner.can_trans_to_target_type(target_inner)
                        || inner.can_trans_to_target_type(target)/* 为了解决 &&a -> &a 的问题 */)
            }
            (Self::Array(inner, len), ResolvedTy::Array(target_inner, target_len)) => {
                len == target_len && inner.can_trans_to_target_type(target_inner)
            }
            (Self::Infer, _) => true,
            (Self::Never, _) => true,
            _ => false,
        }
    }

    pub fn is_number_type(&self) -> bool {
        match self {
            ResolvedTypes::Types(hash_set) => hash_set.iter().all(|x| x.is_number_type()),
            ResolvedTypes::Never => true,
            _ => false,
        }
    }

    pub fn is_signed_number_type(&self) -> bool {
        match self {
            ResolvedTypes::Types(hash_set) => hash_set.iter().all(|x| x.is_signed_number_type()),
            ResolvedTypes::Never => true,
            _ => false,
        }
    }

    pub fn deref_once(self) -> Option<(Self, Mutability)> {
        match self {
            ResolvedTypes::Types(hash_set) => {
                let (types, levels): (HashSet<TypePtr>, Vec<Mutability>) = hash_set
                    .into_iter()
                    .filter_map(|x| x.deref_once())
                    .collect();

                if types.is_empty() {
                    return None;
                }

                let mutbl = *levels.first().unwrap();
                debug_assert!(levels.iter().all(|x| *x == mutbl));

                Some((Self::Types(types), mutbl))
            }
            ResolvedTypes::Ref(resolved_types, mutability) => Some((*resolved_types, mutability)),
            _ => None,
        }
    }

    pub fn out_of_array(self) -> Option<(Self, DerefLevel)> {
        match self {
            ResolvedTypes::Types(hash_set) => {
                let (types, levels): (HashSet<TypePtr>, Vec<DerefLevel>) = hash_set
                    .into_iter()
                    .filter_map(|x| x.out_of_array())
                    .collect();

                if types.is_empty() {
                    return None;
                }

                let level = *levels.first().unwrap();
                debug_assert!(levels.iter().all(|x| *x == level));

                Some((Self::Types(types), level))
            }
            ResolvedTypes::Ref(resolved_types, mutability) => resolved_types
                .out_of_array()
                .map(|(a, b)| (a, b.merge(DerefLevel::Deref(mutability)))),
            ResolvedTypes::Array(resolved_types, _) => Some((*resolved_types, DerefLevel::Not)),
            _ => None,
        }
    }

    pub fn to_resolved_tys(&self) -> HashSet<TypePtr> {
        match self {
            ResolvedTypes::Types(hash_set) => hash_set.iter().cloned().collect(),
            ResolvedTypes::Ref(resolved_types, mutability) => resolved_types
                .to_resolved_tys()
                .iter()
                .flat_map(|x| {
                    let mut ret = vec![TypePtr::from(ResolvedTy::Ref(x.clone(), *mutability))];

                    if let ResolvedTy::Ref(inner, inner_mutbl) = x.as_ref() {
                        ret.push(
                            ResolvedTy::Ref(inner.clone(), inner_mutbl.merge(*mutability)).into(),
                        )
                    }

                    ret.into_iter()
                })
                .collect(),
            ResolvedTypes::Array(resolved_types, len) => resolved_types
                .to_resolved_tys()
                .iter()
                .map(|x| ResolvedTy::Array(x.clone(), *len).into())
                .collect(),
            ResolvedTypes::Infer => HashSet::new(),
            ResolvedTypes::Never => HashSet::from([ResolvedTy::Never.into()]),
        }
    }
}
