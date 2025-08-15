use crate::{
    ast::{Crate, expr::Expr, item::Item, pat::Pat, stmt::Stmt, ty::Ty},
    semantics::SemanticError,
};

pub trait Visitor {
    fn visit_crate(&mut self, krate: &Crate) -> Result<(), SemanticError>;
    fn visit_item(&mut self, item: &Item) -> Result<(), SemanticError>;
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), SemanticError>;
    fn visit_expr(&mut self, expr: &Expr) -> Result<Ty, SemanticError>;
    fn visit_pat(&mut self, pat: &Pat) -> Result<Ty, SemanticError>;
}
