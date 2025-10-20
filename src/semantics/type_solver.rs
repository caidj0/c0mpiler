use std::cell::RefMut;
use std::iter::zip;

use ena::unify::{InPlace, UnificationTable, UnifyValue};

use crate::semantics::resolved_ty::{ResolvedTy, TypeIntern};
use crate::semantics::{
    analyzer::SemanticAnalyzer,
    error::SemanticError,
    resolved_ty::{AnyTyKind, RefMutability, ResolvedTyKind, TypeKey},
};
use crate::{impossible, make_semantic_error};

impl TypeIntern {
    pub fn select(t1: &Self, t2: &Self) -> Self {
        match (t1, t2) {
            (TypeIntern::Never, TypeIntern::Never) => TypeIntern::Never,
            (TypeIntern::Never, TypeIntern::Other(type_key))
            | (TypeIntern::Other(type_key), TypeIntern::Never)
            | (TypeIntern::Other(type_key), TypeIntern::Other(..)) => TypeIntern::Other(*type_key),
        }
    }
}

impl UnifyValue for ResolvedTy {
    type Error = TypeSolveError;

    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, Self::Error> {
        if value2.names.is_some() {
            return Ok(value2.clone());
        } else if value1.names.is_some() {
            return Ok(value1.clone());
        }

        use ResolvedTyKind::*;
        match (&value1.kind, &value2.kind) {
            (_, Any(AnyTyKind::Any)) => Ok(value1.clone()),
            (Any(AnyTyKind::Any), _) => Ok(value2.clone()),

            (BuiltIn(b1), BuiltIn(b2)) => {
                if b1 != b2 {
                    return Err(TypeSolveError::BuiltInMismatch);
                }
                Ok(value1.clone())
            }
            (Ref(t1, m1), Ref(t2, m2)) => {
                let mutbl = RefMutability::eq(m1, m2)?;
                Ok(Self {
                    names: None,
                    kind: ResolvedTyKind::Ref(TypeIntern::select(t1, t2), mutbl),
                })
            }
            (Tup(t1), Tup(t2)) => {
                if t1.len() != t2.len() {
                    return Err(TypeSolveError::StructLengthMismatch);
                }

                Ok(Self {
                    names: None,
                    kind: ResolvedTyKind::Tup(
                        zip(t1, t2).map(|(x, y)| TypeIntern::select(x, y)).collect(),
                    ),
                })
            }
            (Enum, Enum) | (Trait, Trait) => impossible!(), // 它们必定有名字
            (Array(t1, l1), Array(t2, l2)) => {
                let len = match (l1, l2) {
                    (None, None) => None,
                    (None, Some(l)) | (Some(l), None) => Some(*l),
                    (Some(l1), Some(l2)) => {
                        if l1 != l2 {
                            return Err(TypeSolveError::ArrayLengthMismatch);
                        } else {
                            Some(*l1)
                        }
                    }
                };

                Ok(Self {
                    names: None,
                    kind: ResolvedTyKind::Array(TypeIntern::select(t1, t2), len),
                })
            }
            (Fn(r1, a1), Fn(r2, a2)) => Ok(Self {
                names: None,
                kind: ResolvedTyKind::Fn(
                    TypeIntern::select(r1, r2),
                    zip(a1, a2).map(|(x, y)| TypeIntern::select(x, y)).collect(),
                ),
            }),
            (l, Any(kind)) => {
                if kind.can_cast_to(l) {
                    Ok(value1.clone())
                } else {
                    Err(TypeSolveError::AnyTypeMismatch)
                }
            }
            (Any(kind), r) => {
                if kind.can_cast_to(r) {
                    Ok(value2.clone())
                } else {
                    Err(TypeSolveError::AnyTypeMismatch)
                }
            }
            _ => Err(TypeSolveError::GeneralTypeMismatch),
        }
    }
}

pub struct TypeSolver<'analyzer> {
    ut: RefMut<'analyzer, UnificationTable<InPlace<TypeKey>>>,
    downgrade: bool,
}

impl<'analyzer> TypeSolver<'analyzer> {
    pub fn eq(&mut self, left: TypeIntern, right: TypeIntern) -> Result<(), TypeSolveError> {
        let (TypeIntern::Other(left), TypeIntern::Other(right)) = (left, right) else {
            return Ok(());
        };

        if self.ut.unioned(left, right) {
            return Ok(());
        }

        let mut left_ty = self.ut.probe_value(left);
        let mut right_ty = self.ut.probe_value(right);

        match (&left_ty.kind, &right_ty.kind) {
            (ResolvedTyKind::ImplicitSelf(l), ResolvedTyKind::ImplicitSelf(r)) => {
                if self.ut.unioned(l.to_key(), r.to_key()) {
                    return Ok(());
                } else {
                    left_ty = self.ut.probe_value(l.to_key());
                    right_ty = self.ut.probe_value(r.to_key());
                }
            }
            (ResolvedTyKind::ImplicitSelf(l), _) => {
                if self.ut.unioned(l.to_key(), right) {
                    return Ok(());
                } else {
                    left_ty = self.ut.probe_value(l.to_key());
                }
            }
            (_, ResolvedTyKind::ImplicitSelf(r)) => {
                if self.ut.unioned(left, r.to_key()) {
                    return Ok(());
                } else {
                    right_ty = self.ut.probe_value(r.to_key());
                }
            }
            _ => {}
        };

        if self.downgrade && left_ty.names.is_none() && right_ty.names.is_none() {
            match (&left_ty.kind, &right_ty.kind) {
                (
                    ResolvedTyKind::Ref(inner1, RefMutability::Not),
                    ResolvedTyKind::Ref(inner2, RefMutability::Mut),
                ) => {
                    self.downgrade = false;
                    self.eq(*inner1, *inner2)?;
                    self.downgrade = true;
                    return Ok(());
                }
                _ => {}
            }
        }

        self.downgrade = false;

        self.ut.unify_var_var(left, right)?;

        self.eq_inner(left_ty, right_ty)
    }

    pub fn eq_with_type(
        &mut self,
        left: TypeIntern,
        right_ty: ResolvedTy,
    ) -> Result<(), TypeSolveError> {
        let TypeIntern::Other(left) = left else {
            return Ok(());
        };

        let left_ty = self.ut.probe_value(left);

        self.ut.unify_var_value(left, right_ty.clone())?;

        self.eq_inner(left_ty, right_ty)
    }

    fn eq_inner(
        &mut self,
        left_ty: ResolvedTy,
        right_ty: ResolvedTy,
    ) -> Result<(), TypeSolveError> {
        // 有名字的 type 一定是唯一的
        if left_ty.names != right_ty.names {
            if !(left_ty.is_any_type() || right_ty.is_any_type()) {
                println!("{left_ty:?}");
                println!("{right_ty:?}");
                return Err(TypeSolveError::NameMismatch);
            } else {
                return Ok(());
            }
        }

        use ResolvedTyKind::*;
        match (left_ty.kind, right_ty.kind) {
            (Ref(t1, _), Ref(t2, _)) => {
                self.eq(t1, t2)?;
            }
            (Tup(t1), Tup(t2)) => {
                zip(t1, t2)
                    .map(|(x, y)| self.eq(x, y))
                    .collect::<Result<(), TypeSolveError>>()?;
            }
            (Array(t1, _), Array(t2, _)) => {
                self.eq(t1, t2)?;
            }
            (Fn(r1, a1), Fn(r2, a2)) => {
                self.eq(r1, r2)?;
                zip(a1, a2)
                    .map(|(x, y)| self.eq(x, y))
                    .collect::<Result<(), TypeSolveError>>()?;
            }
            _ => {}
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum TypeSolveError {
    NameMismatch,
    BuiltInMismatch,
    MutabilityMismatch,
    StructLengthMismatch,
    ArrayLengthMismatch,
    AnyTypeMismatch,
    GeneralTypeMismatch,
}

macro_rules! to_semantic_error {
    ($e:expr) => {{
        let mut result = $e.map_err(|e| make_semantic_error!(TypeError(e)));
        #[cfg(debug_assertions)]
        {
            let caller_location = std::panic::Location::caller();
            if let Err(e) = result.as_mut() {
                e.file = caller_location.file();
                e.line = caller_location.line();
            };
        }
        result
    }};
}

impl SemanticAnalyzer {
    pub fn create_type_solver(&self, downgrade: bool) -> TypeSolver<'_> {
        TypeSolver {
            ut: self.ut.borrow_mut(),
            downgrade,
        }
    }

    #[track_caller]
    pub fn ty_intern_eq(&self, left: TypeIntern, right: TypeIntern) -> Result<(), SemanticError> {
        let mut solver = self.create_type_solver(false);
        to_semantic_error!(solver.eq(left, right))
    }

    #[track_caller]
    pub fn type_eq(&mut self, left: TypeIntern, right_ty: ResolvedTy) -> Result<(), SemanticError> {
        debug_assert!(right_ty.names.is_none());
        let mut solver = self.create_type_solver(false);
        to_semantic_error!(solver.eq_with_type(left, right_ty))
    }

    #[track_caller]
    pub fn ty_downgrade_eq(
        &self,
        lower: TypeIntern,
        higher: TypeIntern,
    ) -> Result<(), SemanticError> {
        let mut solver = self.create_type_solver(true);
        to_semantic_error!(solver.eq(lower, higher))
    }
}
