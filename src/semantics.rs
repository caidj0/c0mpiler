pub mod visitor;

use std::collections::HashMap;

use crate::ast::{
    Ident, Mutability,
    item::{ConstItem, FieldDef, FnItem, TraitRef},
    path::Path,
    ty::Ty,
};

#[derive(Debug)]
pub enum SemanticError {}

#[derive(Debug)]
pub struct ImplInfo {
    pub trait_of: Option<TraitRef>,
    pub methods: Vec<FnItem>,
    pub consts: Vec<ConstItem>,
}

#[derive(Debug)]
pub enum TyNameSpace {
    Struct {
        fields: Vec<FieldDef>,
        impls: Vec<ImplInfo>,
    },
    Enum {
        variants: Vec<Ident>,
        impls: Vec<ImplInfo>,
    },
    Trait {
        methods: Vec<FnItem>,
        consts: Vec<ConstItem>,
    },
}

#[derive(Debug)]
pub enum ValueNameSpace {
    Fn(FnItem),
    Const(ConstItem),
}

#[derive(Debug)]
pub struct Variable {
    pub ty: Ty,
    pub mutbl: Mutability,
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub type_names: HashMap<Ident, TyNameSpace>,
    pub value_names: HashMap<Ident, ValueNameSpace>,
    pub current_module: Path,
    pub stacks: Vec<HashMap<Ident, Variable>>,
}
