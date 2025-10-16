use std::ops::Deref;

use crate::{ast::expr::Expr, semantics::{analyzer::SemanticAnalyzer, error::SemanticError, resolved_ty::TypePtr, value::{ConstantValue, Value}}};

pub struct ConstantValuePtr<'p>(&'p Value);

impl<'p> Deref for ConstantValuePtr<'p> {
    type Target = ConstantValue;

    fn deref(&self) -> &Self::Target {
        self.0.kind.as_constant().unwrap()
    }
}

impl SemanticAnalyzer {
    pub fn const_eval(&mut self, expr: &Expr, target_type: TypePtr) -> Result<ConstantValuePtr<'_>, SemanticError> {
        todo!()
    }
}