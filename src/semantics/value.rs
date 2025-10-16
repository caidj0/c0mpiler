use crate::{
    ast::{Mutability, NodeId, Symbol},
    semantics::resolved_ty::TypePtr,
};

#[derive(Debug)]
pub struct Value {
    pub ty: TypePtr,
    pub mutbl: Mutability,
    pub kind: ValueKind,
}

#[derive(Debug)]
pub enum ValueKind {
    Anon,
    Constant(ConstantValue),
    Struct(Vec<ValueIndex>),
    Array(Vec<ValueIndex>),

}

#[derive(Debug)]
pub enum ConstantValue {
    Fn,
    ConstantInt(u32),
    ConstantString(String),
}

#[derive(Debug, Clone)]
pub enum ValueIndex {
    Expr { expr_id: NodeId },
    Global { scope_id: NodeId, name: Symbol },
}