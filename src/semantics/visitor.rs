use crate::{
    ast::{
        Crate,
        expr::{
            AddrOfExpr, ArrayExpr, AssignExpr, AssignOpExpr, BinaryExpr, BlockExpr, BreakExpr,
            CallExpr, CastExpr, ConstBlockExpr, ContinueExpr, Expr, ExprKind, FieldExpr,
            ForLoopExpr, IfExpr, IndexExpr, LetExpr, LitExpr, LoopExpr, MatchExpr, MethodCallExpr,
            PathExpr, RangeExpr, RepeatExpr, RetExpr, StructExpr, TupExpr, UnaryExpr,
            UnderscoreExpr, WhileExpr,
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
    fn visit_let_stmt(&mut self, stmt: &LocalStmt) -> Self::DefaultRes;

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprRes {
        match &expr.kind {
            ExprKind::Array(array_expr) => self.visit_array_expr(array_expr),
            ExprKind::ConstBlock(const_block_expr) => self.visit_const_block_expr(const_block_expr),
            ExprKind::Call(call_expr) => self.visit_call_expr(call_expr),
            ExprKind::MethodCall(method_call_expr) => self.visit_method_call_expr(method_call_expr),
            ExprKind::Tup(tup_expr) => self.visit_tup_expr(tup_expr),
            ExprKind::Binary(binary_expr) => self.visit_binary_expr(binary_expr),
            ExprKind::Unary(unary_expr) => self.visit_unary_expr(unary_expr),
            ExprKind::Lit(lit_expr) => self.visit_lit_expr(lit_expr),
            ExprKind::Cast(cast_expr) => self.visit_cast_expr(cast_expr),
            ExprKind::Let(let_expr) => self.visit_let_expr(let_expr),
            ExprKind::If(if_expr) => self.visit_if_expr(if_expr),
            ExprKind::While(while_expr) => self.visit_while_expr(while_expr),
            ExprKind::ForLoop(for_loop_expr) => self.visit_for_loop_expr(for_loop_expr),
            ExprKind::Loop(loop_expr) => self.visit_loop_expr(loop_expr),
            ExprKind::Match(match_expr) => self.visit_match_expr(match_expr),
            ExprKind::Block(block_expr) => self.visit_block_expr(block_expr),
            ExprKind::Assign(assign_expr) => self.visit_assign_expr(assign_expr),
            ExprKind::AssignOp(assign_op_expr) => self.visit_assign_op_expr(assign_op_expr),
            ExprKind::Field(field_expr) => self.visit_field_expr(field_expr),
            ExprKind::Index(index_expr) => self.visit_index_expr(index_expr),
            ExprKind::Range(range_expr) => self.visit_range_expr(range_expr),
            ExprKind::Underscore(underscore_expr) => self.visit_underscore_expr(underscore_expr),
            ExprKind::Path(path_expr) => self.visit_path_expr(path_expr),
            ExprKind::AddrOf(addr_of_expr) => self.visit_addr_of_expr(addr_of_expr),
            ExprKind::Break(break_expr) => self.visit_break_expr(break_expr),
            ExprKind::Continue(continue_expr) => self.visit_continue_expr(continue_expr),
            ExprKind::Ret(ret_expr) => self.visit_ret_expr(ret_expr),
            ExprKind::Struct(struct_expr) => self.visit_struct_expr(struct_expr),
            ExprKind::Repeat(repeat_expr) => self.visit_repeat_expr(repeat_expr),
        }
    }
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
    fn visit_wild_pat(&mut self, expr: &WildPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_ident_pat(&mut self, expr: &IdentPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_struct_pat(&mut self, expr: &StructPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_or_pat(&mut self, expr: &OrPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_path_pat(&mut self, expr: &PathPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_tuple_pat(&mut self, expr: &TuplePat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_ref_pat(&mut self, expr: &RefPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_lit_pat(&mut self, expr: &LitPat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_range_pat(&mut self, expr: &RangePat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_slice_pat(&mut self, expr: &SlicePat, expected_ty: TypeId) -> Self::PatRes;
    fn visit_rest_pat(&mut self, expr: &RestPat, expected_ty: TypeId) -> Self::PatRes;
}
