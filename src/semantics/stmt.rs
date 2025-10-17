use crate::{
    ast::NodeId,
    semantics::{analyzer::SemanticAnalyzer, expr::ControlFlowInterruptKind},
};

#[derive(Debug)]
pub enum StmtResult {
    Expr(NodeId),
    Else { interrupt: ControlFlowInterruptKind },
}

impl SemanticAnalyzer {
    pub(crate) fn set_stmt_result(&mut self, result: StmtResult, id: NodeId) {
        let replace = self.stmt_results.insert(id, result);
        debug_assert!(replace.is_none())
    }
}
