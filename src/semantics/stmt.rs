use crate::{ast::NodeId, semantics::expr::ControlFlowInterruptKind};

#[derive(Debug)]
pub enum StmtResult {
    Expr(NodeId),
    Else { interrupt: ControlFlowInterruptKind },
}
