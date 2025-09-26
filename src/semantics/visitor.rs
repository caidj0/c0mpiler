use crate::{
    ast::{
        Crate,
        expr::{
            AddrOfExpr, ArrayExpr, AssignExpr, AssignOpExpr, BinaryExpr, BlockExpr, BreakExpr,
            CallExpr, CastExpr, ConstBlockExpr, ContinueExpr, Expr, FieldExpr, ForLoopExpr, IfExpr,
            IndexExpr, LetExpr, LitExpr, LoopExpr, MatchExpr, MethodCallExpr, PathExpr, RangeExpr,
            RepeatExpr, RetExpr, StructExpr, TupExpr, UnaryExpr, UnderscoreExpr, WhileExpr,
        },
        item::{
            AssocItemKind, ConstItem, EnumItem, FnItem, ImplItem, Item, ModItem, StructItem,
            TraitItem,
        },
        pat::{
            IdentPat, LitPat, OrPat, Pat, PatKind, PathPat, RangePat, RefPat, RestPat, SlicePat,
            StructPat, TuplePat, WildPat,
        },
        stmt::{LocalStmt, Stmt},
    },
    semantics::TypePtr,
};

pub trait Visitor<'ast> {
    type DefaultRes;
    type ExprRes;
    type PatRes;
    type StmtRes;

    fn visit_crate(&mut self, krate: &'ast Crate) -> Self::DefaultRes;

    fn visit_item(&mut self, item: &'ast Item) -> Self::DefaultRes;
    fn visit_associate_item(&mut self, item: &'ast Item<AssocItemKind>) -> Self::DefaultRes;
    fn visit_const_item(&mut self, item: &'ast ConstItem) -> Self::DefaultRes;
    fn visit_fn_item(&mut self, item: &'ast FnItem) -> Self::DefaultRes;
    fn visit_mod_item(&mut self, item: &'ast ModItem) -> Self::DefaultRes;
    fn visit_enum_item(&mut self, item: &'ast EnumItem) -> Self::DefaultRes;
    fn visit_struct_item(&mut self, item: &'ast StructItem) -> Self::DefaultRes;
    fn visit_trait_item(&mut self, item: &'ast TraitItem) -> Self::DefaultRes;
    fn visit_impl_item(&mut self, item: &'ast ImplItem) -> Self::DefaultRes;

    fn visit_stmt(&mut self, stmt: &'ast Stmt) -> Self::StmtRes;
    fn visit_let_stmt(&mut self, stmt: &'ast LocalStmt) -> Self::StmtRes;

    fn visit_expr(&mut self, expr: &'ast Expr) -> Self::ExprRes;
    fn visit_array_expr(&mut self, expr: &'ast ArrayExpr) -> Self::ExprRes;
    fn visit_const_block_expr(&mut self, expr: &'ast ConstBlockExpr) -> Self::ExprRes;
    fn visit_call_expr(&mut self, expr: &'ast CallExpr) -> Self::ExprRes;
    fn visit_method_call_expr(&mut self, expr: &'ast MethodCallExpr) -> Self::ExprRes;
    fn visit_tup_expr(&mut self, expr: &'ast TupExpr) -> Self::ExprRes;
    fn visit_binary_expr(&mut self, expr: &'ast BinaryExpr) -> Self::ExprRes;
    fn visit_unary_expr(&mut self, expr: &'ast UnaryExpr) -> Self::ExprRes;
    fn visit_lit_expr(&mut self, expr: &'ast LitExpr) -> Self::ExprRes;
    fn visit_cast_expr(&mut self, expr: &'ast CastExpr) -> Self::ExprRes;
    fn visit_let_expr(&mut self, expr: &'ast LetExpr) -> Self::ExprRes;
    fn visit_if_expr(&mut self, expr: &'ast IfExpr) -> Self::ExprRes;
    fn visit_while_expr(&mut self, expr: &'ast WhileExpr) -> Self::ExprRes;
    fn visit_for_loop_expr(&mut self, expr: &'ast ForLoopExpr) -> Self::ExprRes;
    fn visit_loop_expr(&mut self, expr: &'ast LoopExpr) -> Self::ExprRes;
    fn visit_match_expr(&mut self, expr: &'ast MatchExpr) -> Self::ExprRes;
    fn visit_block_expr(&mut self, expr: &'ast BlockExpr) -> Self::ExprRes;
    fn visit_assign_expr(&mut self, expr: &'ast AssignExpr) -> Self::ExprRes;
    fn visit_assign_op_expr(&mut self, expr: &'ast AssignOpExpr) -> Self::ExprRes;
    fn visit_field_expr(&mut self, expr: &'ast FieldExpr) -> Self::ExprRes;
    fn visit_index_expr(&mut self, expr: &'ast IndexExpr) -> Self::ExprRes;
    fn visit_range_expr(&mut self, expr: &'ast RangeExpr) -> Self::ExprRes;
    fn visit_underscore_expr(&mut self, expr: &'ast UnderscoreExpr) -> Self::ExprRes;
    fn visit_path_expr(&mut self, expr: &'ast PathExpr) -> Self::ExprRes;
    fn visit_addr_of_expr(&mut self, expr: &'ast AddrOfExpr) -> Self::ExprRes;
    fn visit_break_expr(&mut self, expr: &'ast BreakExpr) -> Self::ExprRes;
    fn visit_continue_expr(&mut self, expr: &'ast ContinueExpr) -> Self::ExprRes;
    fn visit_ret_expr(&mut self, expr: &'ast RetExpr) -> Self::ExprRes;
    fn visit_struct_expr(&mut self, expr: &'ast StructExpr) -> Self::ExprRes;
    fn visit_repeat_expr(&mut self, expr: &'ast RepeatExpr) -> Self::ExprRes;

    fn visit_pat(&mut self, pat: &'ast Pat, expected_ty: TypePtr) -> Self::PatRes {
        match &pat.kind {
            PatKind::Wild(wild_pat) => self.visit_wild_pat(wild_pat, expected_ty),
            PatKind::Ident(ident_pat) => self.visit_ident_pat(ident_pat, expected_ty),
            PatKind::Struct(struct_pat) => self.visit_struct_pat(struct_pat, expected_ty),
            PatKind::Or(or_pat) => self.visit_or_pat(or_pat, expected_ty),
            PatKind::Path(path_pat) => self.visit_path_pat(path_pat, expected_ty),
            PatKind::Tuple(tuple_pat) => self.visit_tuple_pat(tuple_pat, expected_ty),
            PatKind::Ref(ref_pat) => self.visit_ref_pat(ref_pat, expected_ty),
            PatKind::Lit(lit_pat) => self.visit_lit_pat(lit_pat, expected_ty),
            PatKind::Range(range_pat) => self.visit_range_pat(range_pat, expected_ty),
            PatKind::Slice(slice_pat) => self.visit_slice_pat(slice_pat, expected_ty),
            PatKind::Rest(rest_pat) => self.visit_rest_pat(rest_pat, expected_ty),
        }
    }
    fn visit_wild_pat(&mut self, pat: &'ast WildPat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_ident_pat(&mut self, pat: &'ast IdentPat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_struct_pat(&mut self, pat: &'ast StructPat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_or_pat(&mut self, pat: &'ast OrPat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_path_pat(&mut self, pat: &'ast PathPat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_tuple_pat(&mut self, pat: &'ast TuplePat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_ref_pat(&mut self, pat: &'ast RefPat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_lit_pat(&mut self, pat: &'ast LitPat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_range_pat(&mut self, pat: &'ast RangePat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_slice_pat(&mut self, pat: &'ast SlicePat, expected_ty: TypePtr) -> Self::PatRes;
    fn visit_rest_pat(&mut self, pat: &'ast RestPat, expected_ty: TypePtr) -> Self::PatRes;
}
