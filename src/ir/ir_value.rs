use std::rc::Rc;

use crate::{ast::NodeId, ir::ir_type::IRType};

pub struct Value {
    ty: IRType,
    kind: ValueKind,
    id: Option<NodeId>,
}

pub enum ValueKind {
    Named(String),
    IntConst(u32),
    Struct(Vec<Value>),
    Array(Vec<Value>),
    Ptr(Box<Value>),
    Function(),
    Call(),
    Element(),
    Binary(),
    Unary(),
    Void,
}
