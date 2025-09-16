pub mod literal_eval;

use std::{collections::HashMap, iter::repeat_n};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        Symbol,
        expr::{ArrayExpr, CastExpr, ExprKind, FieldExpr, LitExpr, RepeatExpr},
    },
    semantics::{
        SemanticAnalyzer, error::SemanticError, resolved_ty::ResolvedTy, utils::FullName,
        visitor::Visitor,
    },
};

#[derive(Debug, Clone, EnumAsInner)]
pub enum ConstEvalValue {
    Placeholder,
    U32(u32),
    I32(i32),
    USize(u32),
    ISize(i32),
    Integer(u32),
    SignedInteger(i32),
    UnitStruct(FullName),
    Bool(bool),
    Char(char),
    RefStr(String),
    Array(Vec<ConstEvalValue>),
    Struct(FullName, HashMap<Symbol, ConstEvalValue>),
}

impl ConstEvalValue {
    pub fn is_number(&self) -> bool {
        matches!(
            self,
            Self::U32(_)
                | Self::I32(_)
                | Self::USize(_)
                | Self::ISize(_)
                | Self::Integer(_)
                | Self::SignedInteger(_)
        )
    }

    pub fn is_signed_number(&self) -> bool {
        matches!(self, Self::I32(_) | Self::ISize(_) | Self::SignedInteger(_))
    }

    pub fn cast(self, ty: &ResolvedTy) -> Result<Self, ConstEvalError> {
        fn cast_to_integer<T>(num: T, ty: &ResolvedTy) -> Result<ConstEvalValue, ConstEvalError>
        where
            T: TryInto<u32> + TryInto<i32>,
        {
            Ok(if *ty == ResolvedTy::u32() {
                ConstEvalValue::U32(num.try_into().map_err(|_| ConstEvalError::Overflow)?)
            } else if *ty == ResolvedTy::usize() {
                ConstEvalValue::USize(num.try_into().map_err(|_| ConstEvalError::Overflow)?)
            } else if *ty == ResolvedTy::i32() {
                ConstEvalValue::I32(num.try_into().map_err(|_| ConstEvalError::Overflow)?)
            } else if *ty == ResolvedTy::isize() {
                ConstEvalValue::ISize(num.try_into().map_err(|_| ConstEvalError::Overflow)?)
            } else {
                return Err(ConstEvalError::NotSupportedCast);
            })
        }

        match self {
            ConstEvalValue::U32(u32)
            | ConstEvalValue::USize(u32)
            | ConstEvalValue::Integer(u32) => cast_to_integer(u32, ty),
            ConstEvalValue::I32(i32)
            | ConstEvalValue::ISize(i32)
            | ConstEvalValue::SignedInteger(i32) => cast_to_integer(i32, ty),
            ConstEvalValue::Bool(b) => cast_to_integer(b, ty),
            ConstEvalValue::Char(c) => cast_to_integer(c as u32, ty),
            ConstEvalValue::Placeholder
            | ConstEvalValue::UnitStruct(_)
            | ConstEvalValue::RefStr(_)
            | ConstEvalValue::Array(_)
            | ConstEvalValue::Struct(_, _) => Err(ConstEvalError::NotSupportedCast),
        }
    }
}

impl Into<ResolvedTy> for &ConstEvalValue {
    fn into(self) -> ResolvedTy {
        match self {
            ConstEvalValue::Placeholder => ResolvedTy::Infer,
            ConstEvalValue::U32(_) => ResolvedTy::u32(),
            ConstEvalValue::I32(_) => ResolvedTy::i32(),
            ConstEvalValue::USize(_) => ResolvedTy::usize(),
            ConstEvalValue::ISize(_) => ResolvedTy::isize(),
            ConstEvalValue::Integer(_) => ResolvedTy::integer(),
            ConstEvalValue::SignedInteger(_) => ResolvedTy::signed_integer(),
            ConstEvalValue::UnitStruct(full_name) | ConstEvalValue::Struct(full_name, _) => {
                ResolvedTy::Named(full_name.clone())
            }
            ConstEvalValue::Bool(_) => ResolvedTy::bool(),
            ConstEvalValue::Char(_) => ResolvedTy::char(),
            ConstEvalValue::RefStr(_) => ResolvedTy::ref_str(),
            ConstEvalValue::Array(const_eval_values) => ResolvedTy::Array(
                Box::new(
                    const_eval_values
                        .first()
                        .map_or(ResolvedTy::Infer, |x| x.into()),
                ),
                const_eval_values.len().try_into().unwrap(),
            ),
        }
    }
}

#[derive(Debug)]
pub enum ConstEvalError {
    NotSupportedExpr,
    IncorrectSuffix,
    Overflow,
    InvaildDigit,
    TypeMisMatch,
    Semantic(Box<SemanticError>),
    NotAStruct,
    NotStructField,
    NotSupportedCast,
}

pub struct ConstEvaler<'a> {
    analyzer: &'a SemanticAnalyzer,
}

impl<'a> ConstEvaler<'a> {
    pub fn new(analyzer: &'a SemanticAnalyzer) -> Self {
        Self { analyzer }
    }
}

impl<'a> Visitor for ConstEvaler<'a> {
    type DefaultRes = Result<(), ConstEvalError>;
    type ExprRes = Result<ConstEvalValue, ConstEvalError>;
    type PatRes = Self::DefaultRes;
    type StmtRes = Self::DefaultRes;

    fn visit_crate(&mut self, _krate: &crate::ast::Crate) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_item(&mut self, _item: &crate::ast::item::Item) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_associate_item(
        &mut self,
        _item: &crate::ast::item::Item<crate::ast::item::AssocItemKind>,
    ) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_const_item(&mut self, _item: &crate::ast::item::ConstItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_fn_item(&mut self, _item: &crate::ast::item::FnItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_mod_item(&mut self, _item: &crate::ast::item::ModItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_enum_item(&mut self, _item: &crate::ast::item::EnumItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_struct_item(&mut self, _item: &crate::ast::item::StructItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_trait_item(&mut self, _item: &crate::ast::item::TraitItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_impl_item(&mut self, _item: &crate::ast::item::ImplItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_stmt(&mut self, _stmt: &crate::ast::stmt::Stmt) -> Self::StmtRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_let_stmt(&mut self, _stmt: &crate::ast::stmt::LocalStmt) -> Self::StmtRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_expr(&mut self, expr: &crate::ast::expr::Expr) -> Self::ExprRes {
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

    fn visit_array_expr(&mut self, ArrayExpr(exprs): &ArrayExpr) -> Self::ExprRes {
        let values = exprs
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<_>, ConstEvalError>>()?;

        let tys = values.iter().map(|x| x.into()).collect::<Vec<ResolvedTy>>();
        let ut_ty = ResolvedTy::utilize(tys).ok_or(ConstEvalError::TypeMisMatch)?;
        

        // TODO: 统一类型

        Ok(ConstEvalValue::Array(values))
    }

    fn visit_const_block_expr(
        &mut self,
        _expr: &crate::ast::expr::ConstBlockExpr,
    ) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_call_expr(&mut self, _expr: &crate::ast::expr::CallExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_method_call_expr(
        &mut self,
        _expr: &crate::ast::expr::MethodCallExpr,
    ) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_tup_expr(&mut self, _expr: &crate::ast::expr::TupExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_binary_expr(&mut self, expr: &crate::ast::expr::BinaryExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_unary_expr(&mut self, expr: &crate::ast::expr::UnaryExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_lit_expr(&mut self, expr: &LitExpr) -> Self::ExprRes {
        expr.try_into()
    }

    fn visit_cast_expr(&mut self, CastExpr(expr, cast_ty): &CastExpr) -> Self::ExprRes {
        let cast_ty = self
            .analyzer
            .resolve_ty(&cast_ty)
            .map_err(|x| ConstEvalError::Semantic(Box::new(x)))?;

        let expr_res = self.visit_expr(expr)?;
        expr_res.cast(&cast_ty)
    }

    fn visit_let_expr(&mut self, _expr: &crate::ast::expr::LetExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_if_expr(&mut self, _expr: &crate::ast::expr::IfExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_while_expr(&mut self, _expr: &crate::ast::expr::WhileExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_for_loop_expr(&mut self, _expr: &crate::ast::expr::ForLoopExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_loop_expr(&mut self, _expr: &crate::ast::expr::LoopExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_match_expr(&mut self, _expr: &crate::ast::expr::MatchExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_block_expr(&mut self, _expr: &crate::ast::expr::BlockExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_assign_expr(&mut self, _expr: &crate::ast::expr::AssignExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_assign_op_expr(&mut self, _expr: &crate::ast::expr::AssignOpExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_field_expr(&mut self, FieldExpr(expr, ident): &FieldExpr) -> Self::ExprRes {
        let old_target = self.target_ty.clone();
        self.target_ty = ResolvedTy::Infer;
        let ret = match self.visit_expr(&expr)? {
            ConstEvalValue::Struct(_, mut hash_map) => match hash_map.remove(&ident.symbol) {
                Some(value) => {
                    if !value.type_equal(&self.target_ty) {
                        return Err(ConstEvalError::TypeMisMatch);
                    }
                    value
                }
                None => return Err(ConstEvalError::NotStructField),
            },
            _ => return Err(ConstEvalError::NotAStruct),
        };
        self.target_ty = old_target;
        Ok(ret)
    }

    fn visit_index_expr(&mut self, expr: &crate::ast::expr::IndexExpr) -> Self::ExprRes {
        let old_target = self.target_ty.clone();

        todo!()
    }

    fn visit_range_expr(&mut self, _expr: &crate::ast::expr::RangeExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_underscore_expr(&mut self, _expr: &crate::ast::expr::UnderscoreExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_path_expr(&mut self, expr: &crate::ast::expr::PathExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_addr_of_expr(&mut self, _expr: &crate::ast::expr::AddrOfExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_break_expr(&mut self, _expr: &crate::ast::expr::BreakExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_continue_expr(&mut self, _expr: &crate::ast::expr::ContinueExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_ret_expr(&mut self, _expr: &crate::ast::expr::RetExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_struct_expr(&mut self, expr: &crate::ast::expr::StructExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_repeat_expr(&mut self, RepeatExpr(expr, rep_time_expr): &RepeatExpr) -> Self::ExprRes {
        let old_target = self.target_ty.clone();

        let ResolvedTy::Array(resolved_ty, size) = &old_target else {
            return Err(ConstEvalError::TypeMisMatch);
        };

        self.target_ty = ResolvedTy::usize();
        let rep_time = self
            .visit_expr(&rep_time_expr.value)?
            .into_u_size()
            .unwrap();

        if *size != rep_time {
            return Err(ConstEvalError::TypeMisMatch);
        }

        self.target_ty = resolved_ty.as_ref().clone();

        let value = self.visit_expr(&expr)?;

        let values: Vec<ConstEvalValue> = repeat_n(value, *size as usize).collect();

        Ok(ConstEvalValue::Array(values))
    }

    fn visit_wild_pat(
        &mut self,
        _pat: &crate::ast::pat::WildPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_ident_pat(
        &mut self,
        _pat: &crate::ast::pat::IdentPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_struct_pat(
        &mut self,
        _pat: &crate::ast::pat::StructPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_or_pat(
        &mut self,
        _pat: &crate::ast::pat::OrPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_path_pat(
        &mut self,
        _pat: &crate::ast::pat::PathPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_tuple_pat(
        &mut self,
        _pat: &crate::ast::pat::TuplePat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_ref_pat(
        &mut self,
        _pat: &crate::ast::pat::RefPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_lit_pat(
        &mut self,
        _pat: &crate::ast::pat::LitPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_range_pat(
        &mut self,
        _pat: &crate::ast::pat::RangePat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_slice_pat(
        &mut self,
        _pat: &crate::ast::pat::SlicePat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_rest_pat(
        &mut self,
        _pat: &crate::ast::pat::RestPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_pat(
        &mut self,
        _pat: &crate::ast::pat::Pat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }
}
