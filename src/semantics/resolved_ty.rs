use std::{cell::RefCell, hash::Hash, rc::Rc};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        Ident, Mutability, NodeId,
        path::{Path, QSelf},
        ty::{PathTy, Ty},
    },
    impossible, make_semantic_error,
    semantics::{analyzer::SemanticAnalyzer, error::SemanticError, utils::FullName},
};

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

    pub fn remove_implicit_self(&mut self, target: Option<&TypePtr>) {
        let mut r = self.0.borrow_mut();
        if let Some(t) = r.kind.as_implicit_self() {
            let ty = target.unwrap_or(t).clone();
            drop(r);
            *self = ty;
        } else {
            r.remove_implicit_self(target);
        }
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
            ResolvedTyKind::Placeholder => impossible!(),
            ResolvedTyKind::BuiltIn(built_in_ty_kind) => {
                ResolvedTyKind::BuiltIn(built_in_ty_kind.clone())
            }
            ResolvedTyKind::Ref(type_ptr, ref_mutability) => {
                ResolvedTyKind::Ref(type_ptr.deep_clone(), ref_mutability.clone())
            }
            ResolvedTyKind::Tup(type_ptrs) => {
                ResolvedTyKind::Tup(type_ptrs.iter().map(|x| x.deep_clone()).collect())
            }
            ResolvedTyKind::Enum => ResolvedTyKind::Enum,
            ResolvedTyKind::Array(type_ptr, len) => {
                ResolvedTyKind::Array(type_ptr.deep_clone(), len.clone())
            }
            ResolvedTyKind::Fn(type_ptr, type_ptrs) => ResolvedTyKind::Fn(
                type_ptr.deep_clone(),
                type_ptrs.iter().map(|x| x.deep_clone()).collect(),
            ),
            ResolvedTyKind::Never => ResolvedTyKind::Never,
            ResolvedTyKind::Trait => ResolvedTyKind::Trait,
            ResolvedTyKind::Any(any_ty_kind) => ResolvedTyKind::Any(any_ty_kind.clone()),
            ResolvedTyKind::ImplicitSelf(type_ptr) => {
                ResolvedTyKind::ImplicitSelf(type_ptr.clone())
            }
        };

        Self { name, kind }
    }

    pub fn remove_implicit_self(&mut self, target: Option<&TypePtr>) {
        match &mut self.kind {
            ResolvedTyKind::Placeholder => {}
            ResolvedTyKind::BuiltIn(_) => {}
            ResolvedTyKind::Ref(type_ptr, _) => {
                type_ptr.remove_implicit_self(target);
            }
            ResolvedTyKind::Tup(type_ptrs) => type_ptrs
                .iter_mut()
                .for_each(|x| x.remove_implicit_self(target)),
            ResolvedTyKind::Enum => {}
            ResolvedTyKind::Trait => {}
            ResolvedTyKind::Array(type_ptr, _) => type_ptr.remove_implicit_self(target),
            ResolvedTyKind::Fn(type_ptr, type_ptrs) => {
                type_ptr.remove_implicit_self(target);
                type_ptrs
                    .iter_mut()
                    .for_each(|x| x.remove_implicit_self(target));
            }
            ResolvedTyKind::Never => {}
            ResolvedTyKind::Any(_) => {}
            ResolvedTyKind::ImplicitSelf(_) => impossible!(),
        }
    }

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
pub enum ResolvedTyKind {
    Placeholder,

    BuiltIn(BuiltInTyKind),
    Ref(TypePtr, RefMutability),
    Tup(Vec<TypePtr>),
    Enum,
    Trait,
    Array(TypePtr, Option<u32>),
    Fn(TypePtr, Vec<TypePtr>),
    Never,
    Any(AnyTyKind),
    ImplicitSelf(TypePtr), // 为了 Trait
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

impl From<Mutability> for RefMutability {
    fn from(value: Mutability) -> Self {
        match value {
            Mutability::Not => RefMutability::Not,
            Mutability::Mut => RefMutability::Mut,
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

macro_rules! type_define {
    ($($name:ident: $e:expr),*) => {
        impl SemanticAnalyzer {
            $(
                paste::paste!{
                    pub fn [<$name _type>]() -> TypePtr {
                        TypePtr(Rc::new(RefCell::new(ResolvedTy {
                            name: None,
                            kind: $e,
                        })))
                    }
                }
            )*
        }

        impl ResolvedTy {
            $(
                paste::paste!{
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
    ref_str: ResolvedTyKind::Ref(SemanticAnalyzer::str_type(), RefMutability::Not),
    never: ResolvedTyKind::Never,
    unit: ResolvedTyKind::Tup(vec![]),
    any: ResolvedTyKind::Any(AnyTyKind::Any),

    any_int: ResolvedTyKind::Any(AnyTyKind::AnyInt),
    any_signed_int: ResolvedTyKind::Any(AnyTyKind::AnySignedInt)
);

impl SemanticAnalyzer {
    pub fn ref_type(ty: TypePtr, mutbl: RefMutability) -> TypePtr {
        TypePtr(Rc::new(RefCell::new(ResolvedTy {
            name: None,
            kind: ResolvedTyKind::Ref(ty, mutbl),
        })))
    }

    pub fn array_type(ty: TypePtr, len: Option<u32>) -> TypePtr {
        TypePtr(Rc::new(RefCell::new(ResolvedTy {
            name: None,
            kind: ResolvedTyKind::Array(ty, len),
        })))
    }

    pub fn fn_type(ret_ty: TypePtr, args: Vec<TypePtr>) -> TypePtr {
        TypePtr(Rc::new(RefCell::new(ResolvedTy {
            name: None,
            kind: ResolvedTyKind::Fn(ret_ty, args),
        })))
    }

    pub fn resolve_type(
        &mut self,
        Ty { kind, id: _, span }: &Ty,
        current_scope: Option<NodeId>,
    ) -> Result<TypePtr, SemanticError> {
        use crate::ast::ty::TyKind::*;
        use crate::ast::ty::*;
        match kind {
            Slice(_) | TraitObject(_) | ImplTrait(_) => {
                return Err(make_semantic_error!(NoImplementation).set_span(span));
            }

            Array(ArrayTy(inner, len_expr)) => {
                let len_value =
                    self.const_eval(&len_expr.value, &mut Self::usize_type(), current_scope)?;
                let len = *len_value.as_constant_int().unwrap();
                Ok(Self::array_type(
                    self.resolve_type(&inner, current_scope)?,
                    Some(len),
                ))
            }
            Ref(RefTy(MutTy { ty, mutbl })) => Ok(Self::ref_type(
                self.resolve_type(ty, current_scope)?,
                (*mutbl).into(),
            )),
            Tup(TupTy(tys)) => match &tys[..] {
                [] => Ok(Self::unit_type()),
                [t1] => self.resolve_type(t1, current_scope),
                _ => Err(make_semantic_error!(NoImplementation).set_span(span)),
            },
            Path(path_ty) => self.resolve_path_type(&path_ty.0, &path_ty.1, current_scope),
            Infer(_) => Ok(Self::any_type()),
            ImplicitSelf => self
                .get_self_type(current_scope, true)
                .ok_or(make_semantic_error!(UnknownSelfType).set_span(span)),
        }
    }

    pub fn resolve_path_type(
        &mut self,
        _: &Option<Box<QSelf>>,
        path: &Path,
        mut current_scope: Option<NodeId>,
    ) -> Result<TypePtr, SemanticError> {
        let origin_scope = current_scope;

        let Ident { symbol, span } = path.get_ident();
        while let Some(id) = current_scope {
            if let Some(ty) = self.get_type(id, symbol) {
                return Ok(ty);
            }
            current_scope = self.get_parent_scope(id);
        }

        Ok(match symbol.0.as_str() {
            "bool" => Self::bool_type(),
            "char" => Self::char_type(),
            "i32" => Self::i32_type(),
            "isize" => Self::isize_type(),
            "u32" => Self::u32_type(),
            "usize" => Self::usize_type(),
            "str" => Self::str_type(),
            "Self" => {
                return self
                    .get_self_type(origin_scope, false)
                    .ok_or(make_semantic_error!(UnknownSelfType).set_span(span));
            }
            _ => return Err(make_semantic_error!(UnknownType).set_span(span)),
        })
    }

    pub fn get_self_type(&self, mut scope_id: Option<NodeId>, implicit: bool) -> Option<TypePtr> {
        let mut out_of_function = false;

        while let Some(id) = scope_id {
            let scope = self.get_scope(id);

            use super::scope::ScopeKind::*;
            match &scope.kind {
                Trait(type_ptr) => {
                    return if implicit {
                        Some(TypePtr(Rc::new(RefCell::new(ResolvedTy {
                            name: None,
                            kind: ResolvedTyKind::ImplicitSelf(type_ptr.clone()),
                        }))))
                    } else {
                        Some(type_ptr.clone())
                    };
                }
                Impl { ty: type_ptr, .. } => return Some(type_ptr.clone()),
                Struct(_, _) | Enum(_, _) => impossible!(),
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
}
