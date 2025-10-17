use enum_as_inner::EnumAsInner;

use crate::{
    ast::NodeId,
    semantics::{analyzer::SemanticAnalyzer, expr::ControlFlowInterruptKind},
};

#[derive(Debug, EnumAsInner)]
pub enum StmtResult {
    Expr(NodeId),
    Else { interrupt: ControlFlowInterruptKind },
}

impl SemanticAnalyzer {
    pub(crate) fn set_stmt_result(&mut self, result: StmtResult, id: NodeId) {
        let replace = self.stmt_results.insert(id, result);
        debug_assert!(replace.is_none())
    }

    pub(crate) fn get_stmt_result(&self, id: &NodeId) -> &StmtResult {
        self.stmt_results.get(id).unwrap()
    }

    pub(crate) fn get_stmt_interrupt(&self, id: &NodeId) -> ControlFlowInterruptKind {
        match self.stmt_results.get(id).unwrap() {
            StmtResult::Expr(expr_id) => self.get_expr_result(expr_id).interrupt,
            StmtResult::Else { interrupt } => *interrupt,
        } 
    }
}
