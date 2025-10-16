use std::collections::HashMap;

use crate::{
    ast::{NodeId, Symbol},
    semantics::{resolved_ty::TypePtr, value::Value},
};

#[derive(Debug)]
pub struct ItemInfo {
    father: NodeId,
    name: Symbol,
    kind: ItemInfoKind,
}

#[derive(Debug)]
pub enum ItemInfoKind {
    Struct(StructInfo),
    Enum(EnumInfo),
    Trait(TraitInfo),
}

#[derive(Debug)]
pub struct StructInfo {
    fields: Vec<(Symbol, TypePtr)>,
}

#[derive(Debug)]
pub struct EnumInfo {
    variants: Vec<(Symbol, Value)>,
}

#[derive(Debug)]
pub struct TraitInfo {
    items: HashMap<Symbol, Value>,
}

#[derive(Debug)]
pub struct ItemExtra {
    scope_id: NodeId,
}
