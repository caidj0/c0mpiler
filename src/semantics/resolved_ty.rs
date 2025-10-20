use std::{fmt::Debug, hash::Hash};

use ena::unify::UnifyKey;
use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        Ident, Mutability, NodeId, Symbol,
        path::{Path, QSelf},
        ty::Ty,
    },
    impossible, make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer, error::SemanticError, impls::DerefLevel,
        type_solver::TypeSolveError, utils::FullName,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumAsInner)]
pub enum TypeIntern {
    Never,
    Other(TypeKey),
}

impl TypeIntern {
    pub fn to_key(self) -> TypeKey {
        match self {
            TypeIntern::Never => impossible!(),
            TypeIntern::Other(type_key) => type_key,
        }
    }
}

impl From<TypeKey> for TypeIntern {
    fn from(value: TypeKey) -> Self {
        Self::Other(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeKey(u32);

impl UnifyKey for TypeKey {
    type Value = ResolvedTy;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        TypeKey(u)
    }

    fn tag() -> &'static str {
        "TypeKey"
    }
}

pub trait TypePtrFamily {
    type Ptr<T>;
}

pub struct InternFamily;
impl TypePtrFamily for InternFamily {
    type Ptr<T> = TypeIntern;
}

pub struct BoxFamily;
impl TypePtrFamily for BoxFamily {
    type Ptr<T> = Box<T>;
}

pub type ResolvedTyInstance = ResolvedTy<BoxFamily>;

pub struct ResolvedTy<F: TypePtrFamily = InternFamily> {
    pub names: Option<(FullName, Option<Vec<Symbol>>)>,
    pub kind: ResolvedTyKind<F::Ptr<ResolvedTy<F>>>,
}

////////// Rust 已经 Derive 不出来以下 Trait 了
impl Debug for ResolvedTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedTy")
            .field("names", &self.names)
            .field("kind", &self.kind)
            .finish()
    }
}

impl Debug for ResolvedTyInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedTy")
            .field("names", &self.names)
            .field("kind", &self.kind)
            .finish()
    }
}

impl PartialEq for ResolvedTy {
    fn eq(&self, other: &Self) -> bool {
        self.names == other.names && self.kind == other.kind
    }
}

impl PartialEq for ResolvedTyInstance {
    fn eq(&self, other: &Self) -> bool {
        self.names == other.names && self.kind == other.kind
    }
}

impl Eq for ResolvedTy {}
impl Eq for ResolvedTyInstance {}

impl Hash for ResolvedTy {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.names.hash(state);
        self.kind.hash(state);
    }
}

impl Hash for ResolvedTyInstance {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.names.hash(state);
        self.kind.hash(state);
    }
}

impl Clone for ResolvedTy {
    fn clone(&self) -> Self {
        Self {
            names: self.names.clone(),
            kind: self.kind.clone(),
        }
    }
}

impl Clone for ResolvedTyInstance {
    fn clone(&self) -> Self {
        Self {
            names: self.names.clone(),
            kind: self.kind.clone(),
        }
    }
}
//////////

impl ResolvedTy {
    pub fn is_integer(&self) -> bool {
        matches!(
            self.kind,
            ResolvedTyKind::BuiltIn(
                BuiltInTyKind::I32
                    | BuiltInTyKind::ISize
                    | BuiltInTyKind::U32
                    | BuiltInTyKind::USize
            ) | ResolvedTyKind::Any(AnyTyKind::AnyInt | AnyTyKind::AnySignedInt)
        )
    }

    pub fn is_signed_integer(&self) -> bool {
        matches!(
            self.kind,
            ResolvedTyKind::BuiltIn(BuiltInTyKind::I32 | BuiltInTyKind::ISize)
                | ResolvedTyKind::Any(AnyTyKind::AnySignedInt)
        )
    }
}

#[derive(Debug, Clone, EnumAsInner, PartialEq, Eq, Hash)]
pub enum ResolvedTyKind<T> {
    Placeholder,

    BuiltIn(BuiltInTyKind),
    Ref(T, RefMutability),
    Tup(Vec<T>),
    Enum,
    Trait,
    Array(T, Option<u32>),
    Fn(T, Vec<T>),
    Any(AnyTyKind),
    ImplicitSelf(T), // 为了 Trait
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

impl RefMutability {
    pub(crate) fn eq(left: &RefMutability, right: &RefMutability) -> Result<Self, TypeSolveError> {
        match (&left, &right) {
            (RefMutability::WeakMut, RefMutability::WeakMut) => Ok(RefMutability::WeakMut),

            (RefMutability::Not, RefMutability::Not)
            | (RefMutability::Not, RefMutability::WeakMut)
            | (RefMutability::WeakMut, RefMutability::Not) => Ok(RefMutability::Not),

            (RefMutability::Not, RefMutability::Mut) | (RefMutability::Mut, RefMutability::Not) => {
                Err(TypeSolveError::MutabilityMismatch)
            }

            (RefMutability::Mut, RefMutability::Mut)
            | (RefMutability::Mut, RefMutability::WeakMut)
            | (RefMutability::WeakMut, RefMutability::Mut) => Ok(RefMutability::Mut),
        }
    }
}

impl From<Mutability> for RefMutability {
    fn from(value: Mutability) -> Self {
        match value {
            Mutability::Not => RefMutability::Not,
            Mutability::Mut => RefMutability::Mut,
        }
    }
}

impl From<RefMutability> for Mutability {
    fn from(value: RefMutability) -> Self {
        match value {
            RefMutability::Not => Mutability::Not,
            RefMutability::Mut | RefMutability::WeakMut => Mutability::Mut,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyTyKind {
    Any,
    AnyInt,
    AnySignedInt,
}

impl AnyTyKind {
    pub fn can_cast_to<T>(&self, target: &ResolvedTyKind<T>) -> bool {
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

macro_rules! type_define {
    ($($name:ident: $e:expr),*) => {
        impl ResolvedTy {
            $(
                paste::paste!{
                    pub fn [<$name _type>]() -> ResolvedTy {
                        ResolvedTy {
                            names: None,
                            kind: $e,
                        }
                    }

                    pub fn [<is_ $name _type>](&self) -> bool {
                        self.kind == $e
                    }
                }
            )*
        }
    };
}

type_define!(
    bool: ResolvedTyKind::BuiltIn(BuiltInTyKind::Bool),
    char: ResolvedTyKind::BuiltIn(BuiltInTyKind::Char),
    i32: ResolvedTyKind::BuiltIn(BuiltInTyKind::I32),
    isize: ResolvedTyKind::BuiltIn(BuiltInTyKind::ISize),
    u32: ResolvedTyKind::BuiltIn(BuiltInTyKind::U32),
    usize: ResolvedTyKind::BuiltIn(BuiltInTyKind::USize),
    str: ResolvedTyKind::BuiltIn(BuiltInTyKind::Str),
    unit: ResolvedTyKind::Tup(vec![]),
    any: ResolvedTyKind::Any(AnyTyKind::Any),

    any_int: ResolvedTyKind::Any(AnyTyKind::AnyInt),
    any_signed_int: ResolvedTyKind::Any(AnyTyKind::AnySignedInt)
);

impl ResolvedTy {
    pub fn ref_type(ty: TypeIntern, mutbl: RefMutability) -> ResolvedTy {
        ResolvedTy {
            names: None,
            kind: ResolvedTyKind::Ref(ty, mutbl),
        }
    }

    pub fn array_type(ty: TypeIntern, len: Option<u32>) -> ResolvedTy {
        ResolvedTy {
            names: None,
            kind: ResolvedTyKind::Array(ty, len),
        }
    }

    pub fn tup_type(tys: Vec<TypeIntern>) -> ResolvedTy {
        ResolvedTy {
            names: None,
            kind: ResolvedTyKind::Tup(tys),
        }
    }

    pub fn fn_type(ret_ty: TypeIntern, args: Vec<TypeIntern>) -> ResolvedTy {
        ResolvedTy {
            names: None,
            kind: ResolvedTyKind::Fn(ret_ty, args),
        }
    }
}

impl SemanticAnalyzer {
    // 比较 Dirty 的实现
    pub fn set_type_kind(&mut self, key: TypeKey, kind: ResolvedTyKind<TypeIntern>) {
        let mut ut = self.ut.borrow_mut();
        let v = ut.probe_value(key);
        debug_assert!(v.names.is_some());
        let value = ResolvedTy { kind, ..v };
        ut.unify_var_value(key, value).unwrap();
    }

    pub fn intern_type(&self, ty: ResolvedTy) -> TypeKey {
        self.ut.borrow_mut().new_key(ty)
    }

    pub fn probe_type(&self, intern: TypeIntern) -> Option<ResolvedTy> {
        match intern {
            TypeIntern::Never => None,
            TypeIntern::Other(type_key) => Some(self.ut.borrow_mut().probe_value(type_key)),
        }
    }

    pub fn probe_type_instance(&self, intern: TypeIntern) -> Option<ResolvedTyInstance> {
        self.probe_type_instance_impl(intern, false, None)
    }

    pub fn probe_type_instance_impl(
        &self,
        intern: TypeIntern,
        remove_implicit_self: bool,
        alter: Option<&ResolvedTyInstance>,
    ) -> Option<ResolvedTyInstance> {
        let ty = self.probe_type(intern)?;
        let names = ty.names;
        Some(ResolvedTyInstance {
            names,
            kind: match ty.kind {
                ResolvedTyKind::Placeholder => ResolvedTyKind::Placeholder,
                ResolvedTyKind::BuiltIn(built_in_ty_kind) => {
                    ResolvedTyKind::BuiltIn(built_in_ty_kind)
                }
                ResolvedTyKind::Ref(inner, ref_mutability) => ResolvedTyKind::Ref(
                    Box::new(self.probe_type_instance_impl(inner, remove_implicit_self, alter)?),
                    ref_mutability,
                ),
                ResolvedTyKind::Tup(items) => ResolvedTyKind::Tup(
                    items
                        .iter()
                        .map(|x| {
                            self.probe_type_instance_impl(*x, remove_implicit_self, alter)
                                .map(Box::new)
                        })
                        .collect::<Option<Vec<_>>>()?,
                ),
                ResolvedTyKind::Enum => ResolvedTyKind::Enum,
                ResolvedTyKind::Trait => ResolvedTyKind::Trait,
                ResolvedTyKind::Array(t, l) => ResolvedTyKind::Array(
                    Box::new(self.probe_type_instance_impl(t, remove_implicit_self, alter)?),
                    l,
                ),
                ResolvedTyKind::Fn(ret_ty, args) => ResolvedTyKind::Fn(
                    Box::new(self.probe_type_instance_impl(ret_ty, remove_implicit_self, alter)?),
                    args.iter()
                        .map(|x| {
                            self.probe_type_instance_impl(*x, remove_implicit_self, alter)
                                .map(Box::new)
                        })
                        .collect::<Option<Vec<_>>>()?,
                ),
                ResolvedTyKind::Any(any_ty_kind) => ResolvedTyKind::Any(any_ty_kind),
                ResolvedTyKind::ImplicitSelf(inner) => {
                    if remove_implicit_self {
                        if let Some(alter) = alter {
                            return Some(alter.clone());
                        } else {
                            return self.probe_type_instance_impl(
                                inner,
                                remove_implicit_self,
                                alter,
                            );
                        }
                    } else {
                        ResolvedTyKind::ImplicitSelf(Box::new(self.probe_type_instance_impl(
                            inner,
                            remove_implicit_self,
                            alter,
                        )?))
                    }
                }
            },
        })
    }

    pub fn resolve_type(
        &mut self,
        Ty { kind, id: _, span }: &Ty,
        current_scope: Option<NodeId>,
    ) -> Result<TypeIntern, SemanticError> {
        use crate::ast::ty::TyKind::*;
        use crate::ast::ty::*;
        match kind {
            Slice(_) | TraitObject(_) | ImplTrait(_) => {
                return Err(make_semantic_error!(NoImplementation).set_span(span));
            }

            Array(ArrayTy(inner, len_expr)) => {
                let len_value =
                    self.const_eval(&len_expr.value, self.usize_type(), current_scope)?;
                let len = *len_value.as_constant_int().unwrap();
                let ty = self.resolve_type(&inner, current_scope)?;

                Ok(self
                    .intern_type(ResolvedTy::array_type(ty, Some(len)))
                    .into())
            }
            Ref(RefTy(MutTy { ty, mutbl })) => {
                let ty = self.resolve_type(ty, current_scope)?;
                Ok(self
                    .intern_type(ResolvedTy::ref_type(ty, (*mutbl).into()))
                    .into())
            }
            Tup(TupTy(tys, force)) => match &tys[..] {
                [] => Ok(self.unit_type()),
                [t1] => {
                    let inner = self.resolve_type(t1, current_scope)?;
                    if *force {
                        Ok(self.intern_type(ResolvedTy::tup_type(vec![inner])).into())
                    } else {
                        Ok(inner)
                    }
                }
                _ => {
                    let inners = tys
                        .iter()
                        .map(|x| self.resolve_type(&x, current_scope))
                        .collect::<Result<Vec<_>, SemanticError>>()?;
                    Ok(self.intern_type(ResolvedTy::tup_type(inners)).into())
                }
            },
            Path(path_ty) => self.resolve_path_type(&path_ty.0, &path_ty.1, current_scope),
            Infer(_) => Ok(self.new_any_type()),
            ImplicitSelf => self
                .get_self_type(current_scope, true)
                .map(|x| x.into())
                .ok_or(make_semantic_error!(UnknownSelfType).set_span(span)),
        }
    }

    pub fn resolve_path_type(
        &mut self,
        _: &Option<Box<QSelf>>,
        path: &Path,
        mut current_scope: Option<NodeId>,
    ) -> Result<TypeIntern, SemanticError> {
        let origin_scope = current_scope;

        let Ident { symbol, span } = path.get_ident();
        while let Some(id) = current_scope {
            if let Some(ty) = self.get_type(id, symbol) {
                return Ok(ty.into());
            }
            current_scope = self.get_parent_scope(id);
        }

        Ok(match symbol.0.as_str() {
            "bool" => self.bool_type(),
            "char" => self.char_type(),
            "i32" => self.i32_type(),
            "isize" => self.isize_type(),
            "u32" => self.u32_type(),
            "usize" => self.usize_type(),
            "str" => self.str_type(),
            "Self" => {
                return self
                    .get_self_type(origin_scope, false)
                    .map(|x| x.into())
                    .ok_or(make_semantic_error!(UnknownSelfType).set_span(span));
            }
            _ => return Err(make_semantic_error!(UnknownType).set_span(span)),
        })
    }

    pub fn get_self_type(
        &mut self,
        mut scope_id: Option<NodeId>,
        implicit: bool,
    ) -> Option<TypeKey> {
        let mut out_of_function = false;

        while let Some(id) = scope_id {
            let scope = self.get_scope(id);

            use super::scope::ScopeKind::*;
            match &scope.kind {
                Trait(type_ptr) => {
                    return if implicit {
                        let ty = ResolvedTy {
                            names: None,
                            kind: ResolvedTyKind::ImplicitSelf((*type_ptr).into()),
                        };
                        Some(self.intern_type(ty).into())
                    } else {
                        Some((*type_ptr).into())
                    };
                }
                Impl { ty: type_ptr, .. } => return Some((*type_ptr).into()),
                Struct(..) | Enum(..) => impossible!(),
                Fn {
                    ret_ty: _,
                    main_fn: _,
                } => {
                    if out_of_function {
                        return None;
                    }
                    out_of_function = true;
                }
                _ => {}
            }
            scope_id = self.get_parent_scope(id);
        }

        None
    }

    pub fn auto_deref<
        T,
        F: Fn(&mut SemanticAnalyzer, TypeIntern) -> Result<Option<T>, SemanticError>,
    >(
        &mut self,
        intern: TypeIntern,
        func: F,
    ) -> Result<Option<(DerefLevel, TypeIntern, T)>, SemanticError> {
        let mut intern = Some(intern);
        let mut level = DerefLevel::Not;
        while let Some(i) = intern {
            if let Some(t) = func(self, i)? {
                return Ok(Some((level, i, t)));
            }

            let Some(probe) = self.probe_type(i) else {
                return Ok(None);
            };
            if let ResolvedTyKind::Ref(inner, ref_mutbl) = probe.kind {
                intern = Some(inner);
                level.wrap(ref_mutbl.into());
            } else if probe.is_any_int_type() {
                intern = Some(self.u32_type()); // 为了 3.to_string
            } else {
                intern = None;
            }
        }

        Ok(None)
    }
}
