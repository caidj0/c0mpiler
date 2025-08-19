use crate::{
    ast::{
        Crate,
        expr::Expr,
        item::{ConstItem, EnumItem, FnItem, ImplItem, Item, ModItem, StructItem, TraitItem},
        pat::Pat,
        stmt::Stmt,
    },
    semantics::{ResolvedTy, SemanticError},
};

pub trait Visitor {
    fn visit_crate(&mut self, krate: &Crate) -> Result<(), SemanticError>;

    fn visit_item(&mut self, item: &Item) -> Result<(), SemanticError>;
    fn visit_const_item(&mut self, item: &ConstItem) -> Result<(), SemanticError>;
    fn visit_fn_item(&mut self, item: &FnItem) -> Result<(), SemanticError>;
    fn visit_mod_item(&mut self, item: &ModItem) -> Result<(), SemanticError>;
    fn visit_enum_item(&mut self, item: &EnumItem) -> Result<(), SemanticError>;
    fn visit_struct_item(&mut self, item: &StructItem) -> Result<(), SemanticError>;
    fn visit_trait_item(&mut self, item: &TraitItem) -> Result<(), SemanticError>;
    fn visit_impl_item(&mut self, item: &ImplItem) -> Result<(), SemanticError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), SemanticError>;
    fn visit_expr(&mut self, expr: &Expr) -> Result<ResolvedTy, SemanticError>;
    fn visit_pat(&mut self, pat: &Pat) -> Result<ResolvedTy, SemanticError>;
}
