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
    semantics::TypeId,
};

pub trait Visitor {
    type DefaultRes;
    type ExprRes;
    type PatRes;

    fn visit_crate(&mut self, krate: &Crate) -> Self::DefaultRes;

    fn visit_item(&mut self, item: &Item) -> Self::DefaultRes;
    fn visit_associate_item(&mut self, item: &Item<AssocItemKind>) -> Self::DefaultRes;
    fn visit_const_item(&mut self, item: &ConstItem) -> Self::DefaultRes;
    fn visit_fn_item(&mut self, item: &FnItem) -> Self::DefaultRes;
    fn visit_mod_item(&mut self, item: &ModItem) -> Self::DefaultRes;
    fn visit_enum_item(&mut self, item: &EnumItem) -> Self::DefaultRes;
    fn visit_struct_item(&mut self, item: &StructItem) -> Self::DefaultRes;
    fn visit_trait_item(&mut self, item: &TraitItem) -> Self::DefaultRes;
    fn visit_impl_item(&mut self, item: &ImplItem) -> Self::DefaultRes;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::ExprRes;
    fn visit_let_stmt(&mut self, stmt: &LocalStmt) -> Self::ExprRes;

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprRes;
    fn visit_array_expr(&mut self, expr: &ArrayExpr) -> Self::ExprRes;
    fn visit_const_block_expr(&mut self, expr: &ConstBlockExpr) -> Self::ExprRes;
    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::ExprRes;
    fn visit_method_call_expr(&mut self, expr: &MethodCallExpr) -> Self::ExprRes;
    fn visit_tup_expr(&mut self, expr: &TupExpr) -> Self::ExprRes;
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::ExprRes;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::ExprRes;
    fn visit_lit_expr(&mut self, expr: &LitExpr) -> Self::ExprRes;
    fn visit_cast_expr(&mut self, expr: &CastExpr) -> Self::ExprRes;
    fn visit_let_expr(&mut self, expr: &LetExpr) -> Self::ExprRes;
    fn visit_if_expr(&mut self, expr: &IfExpr) -> Self::ExprRes;
    fn visit_while_expr(&mut self, expr: &WhileExpr) -> Self::ExprRes;
    fn visit_for_loop_expr(&mut self, expr: &ForLoopExpr) -> Self::ExprRes;
    fn visit_loop_expr(&mut self, expr: &LoopExpr) -> Self::ExprRes;
    fn visit_match_expr(&mut self, expr: &MatchExpr) -> Self::ExprRes;
    fn visit_block_expr(&mut self, expr: &BlockExpr) -> Self::ExprRes;
    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::ExprRes;
    fn visit_assign_op_expr(&mut self, expr: &AssignOpExpr) -> Self::ExprRes;
    fn visit_field_expr(&mut self, expr: &FieldExpr) -> Self::ExprRes;
    fn visit_index_expr(&mut self, expr: &IndexExpr) -> Self::ExprRes;
    fn visit_range_expr(&mut self, expr: &RangeExpr) -> Self::ExprRes;
    fn visit_underscore_expr(&mut self, expr: &UnderscoreExpr) -> Self::ExprRes;
    fn visit_path_expr(&mut self, expr: &PathExpr) -> Self::ExprRes;
    fn visit_addr_of_expr(&mut self, expr: &AddrOfExpr) -> Self::ExprRes;
    fn visit_break_expr(&mut self, expr: &BreakExpr) -> Self::ExprRes;
    fn visit_continue_expr(&mut self, expr: &ContinueExpr) -> Self::ExprRes;
    fn visit_ret_expr(&mut self, expr: &RetExpr) -> Self::ExprRes;
    fn visit_struct_expr(&mut self, expr: &StructExpr) -> Self::ExprRes;
    fn visit_repeat_expr(&mut self, expr: &RepeatExpr) -> Self::ExprRes;

    fn visit_pat(&mut self, pat: &Pat, expected_ty: TypeId) -> Self::PatRes {
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
    fn visit_wild_pat(&mut self, pat: &WildPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_ident_pat(&mut self, pat: &IdentPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_struct_pat(&mut self, pat: &StructPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_or_pat(&mut self, pat: &OrPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_path_pat(&mut self, pat: &PathPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_tuple_pat(&mut self, pat: &TuplePat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_ref_pat(&mut self, pat: &RefPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_lit_pat(&mut self, pat: &LitPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_range_pat(&mut self, pat: &RangePat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_slice_pat(&mut self, pat: &SlicePat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_rest_pat(&mut self, pat: &RestPat, expected_ty: TypeId) -> Self::PatRes;
}
