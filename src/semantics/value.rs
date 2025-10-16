use enum_as_inner::EnumAsInner;

use crate::{
    ast::{Mutability, NodeId, Symbol, expr::Expr},
    semantics::resolved_ty::TypePtr,
};

#[derive(Debug)]
pub struct Value {
    pub ty: TypePtr,
    pub mutbl: Mutability,
    pub kind: ValueKind,
}

#[derive(Debug, EnumAsInner)]
pub enum ValueKind {
    Anon,
    Constant(ConstantValue),
    Struct(Vec<ValueIndex>),
    Array(Vec<ValueIndex>),
}

#[derive(Debug, EnumAsInner)]
pub enum ConstantValue {
    Fn,
    ConstantInt(u32),
    ConstantString(String),
    ConstantArray(Vec<ConstantValue>),
    Unit,

    UnEval(UnEvalConstant),
    Placeholder, // Only for Trait
}

#[derive(Debug, Clone)]
pub struct UnEvalConstant(NodeId, *const Expr);

impl UnEvalConstant {
    pub fn new(scope: NodeId, expr: &Expr) -> Self {
        Self(scope, &raw const *expr)
    }

    pub fn to_ref(&self) -> (NodeId, &Expr) {
        unsafe { (self.0, &*self.1) }
    }
}

#[derive(Debug, Clone)]
pub enum ValueIndex {
    Expr { expr_id: NodeId },
    Global { scope_id: NodeId, name: Symbol },
}
