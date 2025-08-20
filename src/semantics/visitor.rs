use crate::{
    ast::{
        expr::{
            AddrOfExpr, ArrayExpr, AssignExpr, AssignOpExpr, BinaryExpr, BlockExpr, BreakExpr,
            CallExpr, CastExpr, ConstBlockExpr, ContinueExpr, Expr, ExprKind, FieldExpr,
            ForLoopExpr, IfExpr, IndexExpr, LetExpr, LitExpr, LoopExpr, MatchExpr, MethodCallExpr,
            PathExpr, RangeExpr, RepeatExpr, RetExpr, StructExpr, TupExpr, UnaryExpr,
            UnderscoreExpr, WhileExpr,
        }, item::{
            AssocItemKind, ConstItem, EnumItem, FnItem, ImplItem, Item, ItemKind, ModItem, StructItem, TraitItem
        }, pat::Pat, stmt::{LocalStmt, Stmt}, Crate
    },
    semantics::{ExprResult, SemanticError, TypeId},
};

pub trait Visitor {
    fn visit_crate(&mut self, krate: &Crate) -> Result<(), SemanticError>;

    fn visit_item(&mut self, item: &Item) -> Result<(), SemanticError>;
    fn visit_associate_item(&mut self, item: &Item<AssocItemKind>) -> Result<(), SemanticError>;
    fn visit_const_item(&mut self, item: &ConstItem) -> Result<(), SemanticError>;
    fn visit_fn_item(&mut self, item: &FnItem) -> Result<(), SemanticError>;
    fn visit_mod_item(&mut self, item: &ModItem) -> Result<(), SemanticError>;
    fn visit_enum_item(&mut self, item: &EnumItem) -> Result<(), SemanticError>;
    fn visit_struct_item(&mut self, item: &StructItem) -> Result<(), SemanticError>;
    fn visit_trait_item(&mut self, item: &TraitItem) -> Result<(), SemanticError>;
    fn visit_impl_item(&mut self, item: &ImplItem) -> Result<(), SemanticError>;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<ExprResult, SemanticError>;
    fn visit_let_stmt(&mut self, stmt: &LocalStmt) -> Result<(), SemanticError>;

    fn visit_expr(&mut self, expr: &Expr) -> Result<ExprResult, SemanticError> {
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
    fn visit_array_expr(&mut self, expr: &ArrayExpr) -> Result<ExprResult, SemanticError>;
    fn visit_const_block_expr(&mut self, expr: &ConstBlockExpr) -> Result<ExprResult, SemanticError>;
    fn visit_call_expr(&mut self, expr: &CallExpr) -> Result<ExprResult, SemanticError>;
    fn visit_method_call_expr(&mut self, expr: &MethodCallExpr) -> Result<ExprResult, SemanticError>;
    fn visit_tup_expr(&mut self, expr: &TupExpr) -> Result<ExprResult, SemanticError>;
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Result<ExprResult, SemanticError>;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Result<ExprResult, SemanticError>;
    fn visit_lit_expr(&mut self, expr: &LitExpr) -> Result<ExprResult, SemanticError>;
    fn visit_cast_expr(&mut self, expr: &CastExpr) -> Result<ExprResult, SemanticError>;
    fn visit_let_expr(&mut self, expr: &LetExpr) -> Result<ExprResult, SemanticError>;
    fn visit_if_expr(&mut self, expr: &IfExpr) -> Result<ExprResult, SemanticError>;
    fn visit_while_expr(&mut self, expr: &WhileExpr) -> Result<ExprResult, SemanticError>;
    fn visit_for_loop_expr(&mut self, expr: &ForLoopExpr) -> Result<ExprResult, SemanticError>;
    fn visit_loop_expr(&mut self, expr: &LoopExpr) -> Result<ExprResult, SemanticError>;
    fn visit_match_expr(&mut self, expr: &MatchExpr) -> Result<ExprResult, SemanticError>;
    fn visit_block_expr(&mut self, expr: &BlockExpr) -> Result<ExprResult, SemanticError>;
    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Result<ExprResult, SemanticError>;
    fn visit_assign_op_expr(&mut self, expr: &AssignOpExpr) -> Result<ExprResult, SemanticError>;
    fn visit_field_expr(&mut self, expr: &FieldExpr) -> Result<ExprResult, SemanticError>;
    fn visit_index_expr(&mut self, expr: &IndexExpr) -> Result<ExprResult, SemanticError>;
    fn visit_range_expr(&mut self, expr: &RangeExpr) -> Result<ExprResult, SemanticError>;
    fn visit_underscore_expr(&mut self, expr: &UnderscoreExpr) -> Result<ExprResult, SemanticError>;
    fn visit_path_expr(&mut self, expr: &PathExpr) -> Result<ExprResult, SemanticError>;
    fn visit_addr_of_expr(&mut self, expr: &AddrOfExpr) -> Result<ExprResult, SemanticError>;
    fn visit_break_expr(&mut self, expr: &BreakExpr) -> Result<ExprResult, SemanticError>;
    fn visit_continue_expr(&mut self, expr: &ContinueExpr) -> Result<ExprResult, SemanticError>;
    fn visit_ret_expr(&mut self, expr: &RetExpr) -> Result<ExprResult, SemanticError>;
    fn visit_struct_expr(&mut self, expr: &StructExpr) -> Result<ExprResult, SemanticError>;
    fn visit_repeat_expr(&mut self, expr: &RepeatExpr) -> Result<ExprResult, SemanticError>;

    fn visit_pat(&mut self, pat: &Pat) -> Result<ExprResult, SemanticError>;
}
