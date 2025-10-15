use std::rc::Rc;

use crate::semantics::resolved_ty::{AnyTyKind, RefMutability, ResolvedTyKind, TypePtr};

pub struct TypeSolver;

impl TypeSolver {
    pub fn eq(left: &mut TypePtr, right: &mut TypePtr) -> Result<(), TypeSolveError> {
        if Rc::ptr_eq(left, right) {
            return Ok(());
        }

        let mut left_ptr = left.borrow_mut();
        let mut right_ptr = right.borrow_mut();

        // 名字相同即认为类型相同，这一点需要主动保证
        match (&left_ptr.name, &right_ptr.name) {
            (None, None) => {}
            (None, Some(_)) | (Some(_), None) => return Err(TypeSolveError::NameMismatch),
            (Some(s1), Some(s2)) => {
                if s1 != s2 {
                    return Err(TypeSolveError::NameMismatch);
                }
            }
        }

        enum Leader {
            Left,
            Right,
        }

        use ResolvedTyKind::*;
        let lead = match (&mut left_ptr.kind, &mut right_ptr.kind) {
            (_, Any(AnyTyKind::Any)) => Leader::Left,
            (Any(AnyTyKind::Any), _) => Leader::Right,
            (_, Never) => Leader::Left,
            (Never, _) => Leader::Right,
            (BuiltIn(b1), BuiltIn(b2)) => {
                if b1 != b2 {
                    return Err(TypeSolveError::BuiltInMismatch);
                }
                Leader::Left
            }
            (Ref(t1, m1), Ref(t2, m2)) => {
                TypeSolver::eq(t1, t2)?;
                TypeSolver::mutability_eq(m1, m2)?;
                Leader::Left
            }
            (Struct(s1), Struct(s2)) => {
                TypeSolver::one_to_one_eq(s1, s2)?;
                Leader::Left
            }
            (Enum, Enum) => Leader::Left,
            (Array(t1, l1), Array(t2, l2)) => {
                TypeSolver::eq(t1, t2)?;
                if l1 != l2 {
                    return Err(TypeSolveError::ArrayLengthMismatch);
                }
                Leader::Left
            }
            (Fn(r1, a1), Fn(r2, a2)) => {
                TypeSolver::eq(r1, r2)?;
                TypeSolver::one_to_one_eq(a1, a2)?;
                Leader::Left
            }
            (ImplicitSelf, ImplicitSelf) => Leader::Left,
            (l, Any(kind)) => {
                if kind.can_cast_to(l) {
                    Leader::Left
                } else {
                    return Err(TypeSolveError::AnyTypeMismatch);
                }
            }
            (Any(kind), r) => {
                if kind.can_cast_to(r) {
                    Leader::Right
                } else {
                    return Err(TypeSolveError::AnyTypeMismatch);
                }
            }
            _ => return Err(TypeSolveError::GeneralTypeMismatch),
        };

        drop((left_ptr, right_ptr));

        match lead {
            Leader::Left => *right = left.clone(),
            Leader::Right => *left = right.clone(),
        }

        Ok(())
    }

    fn one_to_one_eq(left: &mut [TypePtr], right: &mut [TypePtr]) -> Result<(), TypeSolveError> {
        if left.len() != right.len() {
            return Err(TypeSolveError::StructLengthMismatch);
        }
        left.iter_mut()
            .zip(right.iter_mut())
            .map(|(t1, t2)| TypeSolver::eq(t1, t2))
            .collect::<Result<(), TypeSolveError>>()
    }

    fn mutability_eq(
        left: &mut RefMutability,
        right: &mut RefMutability,
    ) -> Result<(), TypeSolveError> {
        let mutbl = match (&left, &right) {
            (RefMutability::WeakMut, RefMutability::WeakMut) => RefMutability::WeakMut,

            (RefMutability::Not, RefMutability::Not)
            | (RefMutability::Not, RefMutability::WeakMut)
            | (RefMutability::WeakMut, RefMutability::Not) => RefMutability::Not,

            (RefMutability::Not, RefMutability::Mut) | (RefMutability::Mut, RefMutability::Not) => {
                return Err(TypeSolveError::MutabilityMismatch);
            }

            (RefMutability::Mut, RefMutability::Mut)
            | (RefMutability::Mut, RefMutability::WeakMut)
            | (RefMutability::WeakMut, RefMutability::Mut) => RefMutability::Mut,
        };

        *left = mutbl;
        *right = mutbl;
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
