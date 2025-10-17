use std::ops::{Add, BitOr};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, NodeId, Span},
    semantics::{
        analyzer::SemanticAnalyzer,
        resolved_ty::TypePtr,
        value::{Value, ValueIndex},
    },
};

#[derive(Debug)]
pub struct ExprExtra<'tmp> {
    pub(crate) target_ty: Option<&'tmp mut TypePtr>,
    pub(crate) scope_id: NodeId,
    pub(crate) self_id: NodeId,

    pub(crate) span: Span,
}

#[derive(Debug)]
pub struct ExprResult {
    pub value_index: ValueIndex,
    pub assignee: AssigneeKind,
    pub interrupt: ControlFlowInterruptKind,
}

#[derive(Debug)]
pub enum AssigneeKind {
    Place(Mutability),
    Not,
    Only,
}

#[derive(Debug, Clone, Copy, EnumAsInner)]
pub enum ControlFlowInterruptKind {
    Not,
    Loop,
    Return,
}

impl Add for ControlFlowInterruptKind {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (ControlFlowInterruptKind::Not, _) => rhs,
            (_, _) => self,
        }
    }
}

impl BitOr for ControlFlowInterruptKind {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ControlFlowInterruptKind::Not, _) | (_, ControlFlowInterruptKind::Not) => {
                ControlFlowInterruptKind::Not
            }
            (ControlFlowInterruptKind::Loop, _) | (_, ControlFlowInterruptKind::Loop) => {
                ControlFlowInterruptKind::Loop
            }
            (ControlFlowInterruptKind::Return, ControlFlowInterruptKind::Return) => {
                ControlFlowInterruptKind::Return
            }
        }
    }
}

impl SemanticAnalyzer {
    pub(crate) fn set_expr_value(&mut self, expr_id: NodeId, value: Value) {
        let replace = self.expr_value.insert(expr_id, value);
        debug_assert!(replace.is_none());
    }

    pub(crate) fn set_expr_result(&mut self, expr_id: NodeId, result: ExprResult) {
        let replace = self.expr_results.insert(expr_id, result);
        debug_assert!(replace.is_none());
    }

    pub(crate) fn set_expr_value_and_result(
        &mut self,
        expr_id: NodeId,
        value: Value,
        assignee: AssigneeKind,
        interrupt: ControlFlowInterruptKind,
    ) {
        let replace = self.expr_value.insert(expr_id, value);
        debug_assert!(replace.is_none());
        let replace = self.expr_results.insert(
            expr_id,
            ExprResult {
                value_index: ValueIndex::Expr(expr_id),
                assignee,
                interrupt,
            },
        );
        debug_assert!(replace.is_none());
    }

    pub(crate) fn get_expr_result(&self, expr_id: &NodeId) -> &ExprResult {
        self.expr_results.get(expr_id).unwrap()
    }
}
