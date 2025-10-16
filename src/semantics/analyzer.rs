use std::collections::HashMap;

use crate::{
    ast::{Crate, NodeId, expr::*, item::*, pat::*, stmt::*},
    semantics::{
        error::SemanticError,
        expr::ExprResult,
        impls::Impls,
        item::ItemInfo,
        pat::PatResult,
        resolved_ty::TypePtr,
        scope::Scope,
        utils::{AnalyzeStage, STAGES},
        value::Value,
        visitor::Visitor,
    },
};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    scopes: HashMap<NodeId, Scope>,
    impls: HashMap<TypePtr, Impls>,
    expr_results: HashMap<NodeId, ExprResult>,
    expr_value: HashMap<NodeId, Value>,
    item_infos: HashMap<NodeId, ItemInfo>,

    stage: AnalyzeStage,
}
