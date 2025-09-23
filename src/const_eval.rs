pub mod literal_eval;

use std::{collections::HashMap, iter::repeat_n};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        NodeId, Span, Symbol,
        expr::{ArrayExpr, BinOp, BinaryExpr, CastExpr, Expr, ExprKind, FieldExpr, IndexExpr, LitExpr, PathExpr, RepeatExpr, StructExpr, TupExpr, UnOp, UnaryExpr},
        item::ConstItem,
    },
    extract_extra, make_const_eval_error, make_semantic_error,
    semantics::{
        AnalyzerState, SemanticAnalyzer,
        error::ConstEvalError,
        resolved_ty::ResolvedTy,
        utils::{FullName, ImplInfoItem, TypeKind, ValueContainer, Variable, VariableKind},
        visitor::Visitor,
    },
};

#[derive(Debug, Clone, EnumAsInner)]
pub enum ConstEvalValue {
    Placeholder,
    UnEvaled(NodeId, Span, *const ConstItem),
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
    Enum(FullName, Symbol),
}

impl ConstEvalValue {
    pub fn is_number(&self) -> bool {
        matches!(self, Self::U32(_) | Self::I32(_) | Self::USize(_) | Self::ISize(_) | Self::Integer(_) | Self::SignedInteger(_))
    }

    pub fn is_signed_number(&self) -> bool {
        matches!(self, Self::I32(_) | Self::ISize(_) | Self::SignedInteger(_))
    }

    pub fn is_same_type(&self, ty: &ResolvedTy) -> bool {
        Into::<ResolvedTy>::into(self) == *ty
    }

    pub fn cast(self, ty: &ResolvedTy, explicit: bool) -> Result<Self, ConstEvalError> {
        fn cast_to_integer<T>(num: T, ty: &ResolvedTy) -> Result<ConstEvalValue, ConstEvalError>
        where
            T: TryInto<u32> + TryInto<i32>,
        {
            fn try_into<T, S>(num: T) -> Result<S, ConstEvalError>
            where
                T: TryInto<S>,
            {
                num.try_into().map_err(|_| make_const_eval_error!(Overflow))
            }

            Ok(if *ty == ResolvedTy::u32() {
                ConstEvalValue::U32(try_into(num)?)
            } else if *ty == ResolvedTy::usize() {
                ConstEvalValue::USize(try_into(num)?)
            } else if *ty == ResolvedTy::i32() {
                ConstEvalValue::I32(try_into(num)?)
            } else if *ty == ResolvedTy::isize() {
                ConstEvalValue::ISize(try_into(num)?)
            } else if *ty == ResolvedTy::signed_integer() {
                ConstEvalValue::SignedInteger(try_into(num)?)
            } else if *ty == ResolvedTy::integer() {
                ConstEvalValue::Integer(try_into(num)?)
            } else {
                return Err(make_const_eval_error!(NotSupportedCast));
            })
        }

        if Into::<ResolvedTy>::into(&self) == *ty {
            return Ok(self);
        }

        if !(explicit || self.is_integer() || (self.is_signed_integer() && ty.is_signed_number_type()) || self.is_array()) {
            return Err(make_const_eval_error!(TypeMisMatch));
        }

        match self {
            ConstEvalValue::U32(u32) | ConstEvalValue::USize(u32) | ConstEvalValue::Integer(u32) => cast_to_integer(u32, ty),
            ConstEvalValue::I32(i32) | ConstEvalValue::ISize(i32) | ConstEvalValue::SignedInteger(i32) => cast_to_integer(i32, ty),
            ConstEvalValue::Bool(b) => cast_to_integer(b, ty),
            ConstEvalValue::Char(c) => cast_to_integer(c as u32, ty),
            ConstEvalValue::Array(values) => {
                let ResolvedTy::Array(elm_ty, size) = ty else {
                    return Err(make_const_eval_error!(TypeMisMatch));
                };

                if values.len() != *size as usize {
                    return Err(make_const_eval_error!(TypeMisMatch));
                }

                let elms = values.into_iter().map(|x| x.cast(elm_ty, explicit)).collect::<Result<Vec<ConstEvalValue>, ConstEvalError>>()?;

                Ok(ConstEvalValue::Array(elms))
            }
            ConstEvalValue::Placeholder | ConstEvalValue::UnitStruct(_) | ConstEvalValue::RefStr(_) | ConstEvalValue::Struct(_, _) | ConstEvalValue::Enum(..) => {
                if Into::<ResolvedTy>::into(&self) == *ty {
                    Ok(self)
                } else {
                    Err(make_const_eval_error!(NotSupportedCast))
                }
            }
            ConstEvalValue::UnEvaled(..) => panic!("Impossible!"),
        }
    }
}

impl From<&ConstEvalValue> for ResolvedTy {
    fn from(val: &ConstEvalValue) -> Self {
        match val {
            ConstEvalValue::Placeholder => ResolvedTy::Infer,
            ConstEvalValue::U32(_) => ResolvedTy::u32(),
            ConstEvalValue::I32(_) => ResolvedTy::i32(),
            ConstEvalValue::USize(_) => ResolvedTy::usize(),
            ConstEvalValue::ISize(_) => ResolvedTy::isize(),
            ConstEvalValue::Integer(_) => ResolvedTy::integer(),
            ConstEvalValue::SignedInteger(_) => ResolvedTy::signed_integer(),
            ConstEvalValue::UnitStruct(full_name) | ConstEvalValue::Struct(full_name, _) | ConstEvalValue::Enum(full_name, _) => ResolvedTy::Named(full_name.clone()),
            ConstEvalValue::Bool(_) => ResolvedTy::bool(),
            ConstEvalValue::Char(_) => ResolvedTy::char(),
            ConstEvalValue::RefStr(_) => ResolvedTy::ref_str(),
            ConstEvalValue::Array(const_eval_values) => ResolvedTy::Array(Box::new(const_eval_values.first().map_or(ResolvedTy::Infer, |x| x.into())), const_eval_values.len().try_into().unwrap()),
            ConstEvalValue::UnEvaled(..) => panic!("Impossible!"),
        }
    }
}

pub struct ConstEvaler<'a> {
    analyzer: &'a mut SemanticAnalyzer,
}

impl<'a> ConstEvaler<'a> {
    pub fn eval<'ast, 'tmp>(analyzer: &'a mut SemanticAnalyzer, expr: &'ast Expr, expected_ty: &'tmp ResolvedTy) -> Result<ConstEvalValue, ConstEvalError> {
        let mut evaler = Self { analyzer };
        evaler.visit_expr(expr, Some(expected_ty))
    }
}

impl<'a, 'ast> Visitor<'ast> for ConstEvaler<'a> {
    type DefaultRes = Result<(), ConstEvalError>;
    type ExprRes = Result<ConstEvalValue, ConstEvalError>;
    type PatRes = Self::DefaultRes;
    type StmtRes = Self::DefaultRes;

    type ExprExtra<'tmp> = Option<&'tmp ResolvedTy>;
    type ItemExtra<'tmp> = ();
    type PatExtra<'tmp> = ();
    type StmtExtra<'tmp> = ();

    fn visit_crate(&mut self, _krate: &'ast crate::ast::Crate) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_item<'tmp>(&mut self, _item: &'ast crate::ast::item::Item, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_associate_item<'tmp>(&mut self, _item: &'ast crate::ast::item::Item<crate::ast::item::AssocItemKind>, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_const_item<'tmp>(&mut self, _item: &'ast crate::ast::item::ConstItem, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_fn_item<'tmp>(&mut self, _item: &'ast crate::ast::item::FnItem, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_mod_item<'tmp>(&mut self, _item: &'ast crate::ast::item::ModItem, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_enum_item<'tmp>(&mut self, _item: &'ast crate::ast::item::EnumItem, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_struct_item<'tmp>(&mut self, _item: &'ast crate::ast::item::StructItem, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_trait_item<'tmp>(&mut self, _item: &'ast crate::ast::item::TraitItem, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_impl_item<'tmp>(&mut self, _item: &'ast crate::ast::item::ImplItem, extra: Self::ItemExtra<'tmp>) -> Self::DefaultRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_stmt<'tmp>(&mut self, _stmt: &'ast crate::ast::stmt::Stmt, extra: Self::StmtExtra<'tmp>) -> Self::StmtRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_local_stmt<'tmp>(&mut self, _stmt: &'ast crate::ast::stmt::LocalStmt, extra: Self::StmtExtra<'tmp>) -> Self::StmtRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_expr<'tmp>(&mut self, expr: &'ast crate::ast::expr::Expr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        match &expr.kind {
            ExprKind::Array(array_expr) => self.visit_array_expr(array_expr, extra),
            ExprKind::ConstBlock(const_block_expr) => self.visit_const_block_expr(const_block_expr, extra),
            ExprKind::Call(call_expr) => self.visit_call_expr(call_expr, extra),
            ExprKind::MethodCall(method_call_expr) => self.visit_method_call_expr(method_call_expr, extra),
            ExprKind::Tup(tup_expr) => self.visit_tup_expr(tup_expr, extra),
            ExprKind::Binary(binary_expr) => self.visit_binary_expr(binary_expr, extra),
            ExprKind::Unary(unary_expr) => self.visit_unary_expr(unary_expr, extra),
            ExprKind::Lit(lit_expr) => self.visit_lit_expr(lit_expr, extra),
            ExprKind::Cast(cast_expr) => self.visit_cast_expr(cast_expr, extra),
            ExprKind::Let(let_expr) => self.visit_let_expr(let_expr, extra),
            ExprKind::If(if_expr) => self.visit_if_expr(if_expr, extra),
            ExprKind::While(while_expr) => self.visit_while_expr(while_expr, extra),
            ExprKind::ForLoop(for_loop_expr) => self.visit_for_loop_expr(for_loop_expr, extra),
            ExprKind::Loop(loop_expr) => self.visit_loop_expr(loop_expr, extra),
            ExprKind::Match(match_expr) => self.visit_match_expr(match_expr, extra),
            ExprKind::Block(block_expr) => self.visit_block_expr(block_expr, extra),
            ExprKind::Assign(assign_expr) => self.visit_assign_expr(assign_expr, extra),
            ExprKind::AssignOp(assign_op_expr) => self.visit_assign_op_expr(assign_op_expr, extra),
            ExprKind::Field(field_expr) => self.visit_field_expr(field_expr, extra),
            ExprKind::Index(index_expr) => self.visit_index_expr(index_expr, extra),
            ExprKind::Range(range_expr) => self.visit_range_expr(range_expr, extra),
            ExprKind::Underscore(underscore_expr) => self.visit_underscore_expr(underscore_expr, extra),
            ExprKind::Path(path_expr) => self.visit_path_expr(path_expr, extra),
            ExprKind::AddrOf(addr_of_expr) => self.visit_addr_of_expr(addr_of_expr, extra),
            ExprKind::Break(break_expr) => self.visit_break_expr(break_expr, extra),
            ExprKind::Continue(continue_expr) => self.visit_continue_expr(continue_expr, extra),
            ExprKind::Ret(ret_expr) => self.visit_ret_expr(ret_expr, extra),
            ExprKind::Struct(struct_expr) => self.visit_struct_expr(struct_expr, extra),
            ExprKind::Repeat(repeat_expr) => self.visit_repeat_expr(repeat_expr, extra),
        }
    }

    fn visit_array_expr<'tmp>(&mut self, ArrayExpr(exprs): &'ast ArrayExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let expected_ty = extract_extra!(extra, ResolvedTy::Array(inner, len), CON, {
            if *len as usize != exprs.len() {
                return Err(make_const_eval_error!(TypeMisMatch));
            }
            inner.as_ref()
        });

        let values = exprs.iter().map(|x| self.visit_expr(x, expected_ty)).collect::<Result<Vec<_>, ConstEvalError>>()?;

        let tys = values.iter().map(|x| x.into()).collect::<Vec<ResolvedTy>>();
        let ut_ty = ResolvedTy::utilize(tys).ok_or(make_const_eval_error!(TypeMisMatch))?;

        let values = values.into_iter().map(|x| x.cast(&ut_ty, false)).collect::<Result<Vec<_>, ConstEvalError>>()?;

        Ok(ConstEvalValue::Array(values))
    }

    fn visit_const_block_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::ConstBlockExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_call_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::CallExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_method_call_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::MethodCallExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_tup_expr<'tmp>(&mut self, TupExpr(exprs): &'ast TupExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        match &exprs[..] {
            [expr] => self.visit_expr(expr, extra),
            _ => Err(make_const_eval_error!(NotSupportedExpr)),
        }
    }

    fn visit_binary_expr<'tmp>(&mut self, BinaryExpr(binop, expr1, expr2): &'ast BinaryExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let value1 = self.visit_expr(expr1, None)?;
        let value2 = self.visit_expr(expr2, None)?;

        let res = if let (ConstEvalValue::Bool(value1), ConstEvalValue::Bool(value2)) = (&value1, &value2) {
            ConstEvalValue::Bool(match binop {
                BinOp::And => *value1 && *value2,
                BinOp::Or => *value1 || *value2,
                BinOp::BitXor => value1 ^ value2,
                BinOp::BitAnd => value1 & value2,
                BinOp::BitOr => value1 | value2,
                BinOp::Eq => value1 == value2,
                BinOp::Lt => value1 < value2,
                BinOp::Le => value1 <= value2,
                BinOp::Ne => value1 != value2,
                BinOp::Ge => value1 >= value2,
                BinOp::Gt => value1 > value2,
                _ => return Err(make_const_eval_error!(NotSupportedBinary)),
            })
        } else if value1.is_number() && value2.is_number() {
            match binop {
                BinOp::Shl | BinOp::Shr => {
                    let value2 = value2.cast(&ResolvedTy::u32(), true)?.into_u32().unwrap();

                    macro_rules! number_operation {
                        ($value1:expr, $value2:expr, [$($num_ty:ident),*]) => {
                            match $value1 {
                                $(
                                    ConstEvalValue::$num_ty(value1) => match binop {
                                        BinOp::Shl => ConstEvalValue::$num_ty(
                                            value1.checked_shl(value2).ok_or(make_const_eval_error!(Overflow))?,
                                        ),
                                        BinOp::Shr => ConstEvalValue::$num_ty(
                                            value1.checked_shr(value2).ok_or(make_const_eval_error!(Overflow))?,
                                        ),
                                        _ => return Err(make_const_eval_error!(NotSupportedBinary)),
                                    },
                                )*
                                _ => panic!("Impossible!"),
                            }
                        };
                    }

                    number_operation!(value1, value2, [U32, I32, USize, ISize, Integer, SignedInteger])
                }
                _ => {
                    let ty = ResolvedTy::utilize(vec![(&value1).into(), (&value2).into()]).ok_or(make_const_eval_error!(NotSupportedBinary))?;

                    let value1 = value1.cast(&ty, false).unwrap();
                    let value2 = value2.cast(&ty, false).unwrap();

                    macro_rules! number_operation {
                        ($value1:expr, $value2: expr, [$($num_ty:ident),*]) => {
                            match ($value1, $value2) {
                                $(
                                    (ConstEvalValue::$num_ty(value1), ConstEvalValue::$num_ty(value2)) => match binop {
                                        BinOp::BitXor => ConstEvalValue::$num_ty(value1 ^ value2),
                                        BinOp::BitAnd => ConstEvalValue::$num_ty(value1 & value2),
                                        BinOp::BitOr => ConstEvalValue::$num_ty(value1 | value2),
                                        BinOp::Eq => ConstEvalValue::Bool(value1 == value2),
                                        BinOp::Lt => ConstEvalValue::Bool(value1 < value2),
                                        BinOp::Le => ConstEvalValue::Bool(value1 <= value2),
                                        BinOp::Ne => ConstEvalValue::Bool(value1 != value2),
                                        BinOp::Ge => ConstEvalValue::Bool(value1 >= value2),
                                        BinOp::Gt => ConstEvalValue::Bool(value1 > value2),
                                        BinOp::Add => ConstEvalValue::$num_ty(
                                            value1.checked_add(value2).ok_or(make_const_eval_error!(Overflow))?,
                                        ),
                                        BinOp::Sub => ConstEvalValue::$num_ty(
                                            value1.checked_sub(value2).ok_or(make_const_eval_error!(Overflow))?,
                                        ),
                                        BinOp::Mul => ConstEvalValue::$num_ty(
                                            value1.checked_mul(value2).ok_or(make_const_eval_error!(Overflow))?,
                                        ),
                                        BinOp::Div => ConstEvalValue::$num_ty(
                                            value1.checked_div(value2).ok_or(make_const_eval_error!(Overflow))?,
                                        ),
                                        BinOp::Rem => ConstEvalValue::$num_ty(
                                            value1.checked_rem(value2).ok_or(make_const_eval_error!(Overflow))?,
                                        ),
                                        _ => return Err(make_const_eval_error!(NotSupportedBinary)),
                                    },
                                )*
                                _ => panic!("Impossible!"),
                            }
                        };
                    }

                    number_operation!(value1, value2, [U32, I32, USize, ISize, Integer, SignedInteger])
                }
            }
        } else {
            return Err(make_const_eval_error!(NotSupportedBinary));
        };

        match extra {
            Some(ty) => res.cast(ty, false),
            None => Ok(res),
        }
    }

    fn visit_unary_expr<'tmp>(&mut self, UnaryExpr(unop, expr): &'ast UnaryExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let value = self.visit_expr(expr, extra)?;

        match unop {
            UnOp::Deref => Err(make_const_eval_error!(NotSupportedExpr)),
            UnOp::Not => match value {
                ConstEvalValue::U32(v) => Ok(ConstEvalValue::U32(!v)),
                ConstEvalValue::I32(v) => Ok(ConstEvalValue::I32(!v)),
                ConstEvalValue::USize(v) => Ok(ConstEvalValue::USize(!v)),
                ConstEvalValue::ISize(v) => Ok(ConstEvalValue::ISize(!v)),
                ConstEvalValue::Integer(v) => Ok(ConstEvalValue::Integer(!v)),
                ConstEvalValue::SignedInteger(v) => Ok(ConstEvalValue::SignedInteger(!v)),
                ConstEvalValue::Bool(v) => Ok(ConstEvalValue::Bool(!v)),
                _ => Err(make_const_eval_error!(NotSupportedExpr)),
            },
            UnOp::Neg => match value {
                ConstEvalValue::I32(v) => Ok(ConstEvalValue::I32(v.checked_neg().ok_or(make_const_eval_error!(Overflow))?)),
                ConstEvalValue::ISize(v) => Ok(ConstEvalValue::ISize(v.checked_neg().ok_or(make_const_eval_error!(Overflow))?)),
                ConstEvalValue::Integer(v) => Ok(ConstEvalValue::SignedInteger(TryInto::<i32>::try_into(v).map_err(|_| make_const_eval_error!(Overflow))?.checked_neg().ok_or(make_const_eval_error!(Overflow))?)),
                ConstEvalValue::SignedInteger(v) => Ok(ConstEvalValue::SignedInteger(v.checked_neg().ok_or(make_const_eval_error!(Overflow))?)),
                _ => Err(make_const_eval_error!(NotSupportedExpr)),
            },
        }
    }

    fn visit_lit_expr<'tmp>(&mut self, expr: &'ast LitExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let value: ConstEvalValue = expr.try_into()?;
        match extra {
            Some(ty) => value.cast(ty, false),
            None => Ok(value),
        }
    }

    fn visit_cast_expr<'tmp>(&mut self, CastExpr(expr, cast_ty): &'ast CastExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let cast_ty = self.analyzer.resolve_ty(cast_ty)?;

        let expr_res = self.visit_expr(expr, None)?;
        expr_res.cast(&cast_ty, true)
    }

    fn visit_let_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::LetExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_if_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::IfExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_while_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::WhileExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_for_loop_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::ForLoopExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_loop_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::LoopExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_match_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::MatchExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_block_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::BlockExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_assign_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::AssignExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_assign_op_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::AssignOpExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_field_expr<'tmp>(&mut self, FieldExpr(expr, ident): &'ast FieldExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let ret = match self.visit_expr(expr, None)? {
            ConstEvalValue::Struct(_, mut hash_map) => match hash_map.remove(&ident.symbol) {
                Some(value) => {
                    if let Some(ty) = extra
                        && !value.is_same_type(ty)
                    {
                        return Err(make_const_eval_error!(TypeMisMatch));
                    }
                    value
                }
                None => return Err(make_const_eval_error!(NotStructField)),
            },
            _ => return Err(make_const_eval_error!(NotAStruct)),
        };
        Ok(ret)
    }

    fn visit_index_expr<'tmp>(&mut self, IndexExpr(array, index): &'ast IndexExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let array = self.visit_expr(array, None)?; // array 长度无法确定
        let index = self.visit_expr(index, Some(&ResolvedTy::usize()))?;

        let index = match index {
            ConstEvalValue::USize(index) | ConstEvalValue::Integer(index) => index,
            _ => return Err(make_const_eval_error!(TypeMisMatch)),
        } as usize;

        let ConstEvalValue::Array(mut array) = array else {
            return Err(make_const_eval_error!(TypeMisMatch));
        };

        if index >= array.len() {
            return Err(make_const_eval_error!(OutOfBound));
        }

        let res = array.remove(index);
        match extra {
            Some(ty) => res.cast(ty, false),
            None => Ok(res),
        }
    }

    fn visit_range_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::RangeExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_underscore_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::UnderscoreExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_path_expr<'tmp>(&mut self, PathExpr(qself, path): &'ast PathExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let old_state = self.analyzer.state;
        let value = self.analyzer.search_value_by_path(qself, path)?;

        match value {
            ValueContainer::Variable(v) => match &v.kind {
                VariableKind::Decl | VariableKind::Inited | VariableKind::Fn => Err(make_const_eval_error!(NonConstVariable)),
                VariableKind::Constant(const_eval_value) => {
                    let value = match const_eval_value {
                        ConstEvalValue::UnEvaled(node_id, span, item_ptr) => {
                            let var_ptr = &raw const *v;

                            let item = unsafe { &**item_ptr };
                            self.analyzer.state = AnalyzerState { current_ast_id: *node_id, current_span: *span };

                            let ty = self.analyzer.resolve_ty(&item.ty)?;
                            let ty_id = self.analyzer.intern_type(ty.clone());
                            let evaled_value = self.analyzer.const_eval(ty, item.expr.as_ref().unwrap())?;

                            let var = self.analyzer.search_value_mut(&item.ident.symbol)?;

                            assert!(std::ptr::eq(var_ptr, &raw const *var));

                            *var = Variable { ty: ty_id, mutbl: crate::ast::Mutability::Not, kind: VariableKind::Constant(evaled_value.clone()) };

                            self.analyzer.state = old_state;

                            evaled_value
                        }
                        _ => const_eval_value.clone(),
                    };

                    if let Some(ty) = extra
                        && !value.is_same_type(ty)
                    {
                        return Err(make_const_eval_error!(TypeMisMatch));
                    }

                    Ok(value)
                }
            },
            ValueContainer::ImplInfoItem(_, ImplInfoItem::Constant(constant)) => Ok(constant.value.clone()),
            ValueContainer::ImplInfoItem(_, ImplInfoItem::Method(_)) => Err(make_const_eval_error!(NotSupportedExpr)),
            ValueContainer::Temp(variable) => match &variable.kind {
                VariableKind::Decl | VariableKind::Inited | VariableKind::Fn => Err(make_const_eval_error!(NonConstVariable)),
                VariableKind::Constant(const_eval_value) => Ok(const_eval_value.clone()),
            },
        }
    }

    fn visit_addr_of_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::AddrOfExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_break_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::BreakExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_continue_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::ContinueExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_ret_expr<'tmp>(&mut self, _expr: &'ast crate::ast::expr::RetExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_struct_expr<'tmp>(&mut self, StructExpr { qself, path, fields, rest }: &'ast StructExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        if !matches!(rest, crate::ast::expr::StructRest::None) {
            return Err(make_const_eval_error!(NotSupportedExpr));
        };

        let (id, struct_info) = self.analyzer.search_type_by_path(qself, path)?;
        let struct_name = struct_info.name.clone();

        match &struct_info.kind {
            TypeKind::Placeholder => panic!("Impossible"),
            TypeKind::Struct { fields: struct_fields } => {
                match extra {
                    Some(ty) => {
                        if self.analyzer.resolve_ty_in_scope_by_symbol(&struct_name, id) != *ty {
                            return Err(make_const_eval_error!(TypeMisMatch));
                        }
                    }
                    None => {}
                }

                let mut dic = HashMap::new();

                let mut struct_fields = struct_fields.clone();

                for field in fields {
                    let name = &field.ident.symbol;
                    let expr = &field.expr;

                    let Some(struct_field) = struct_fields.remove(name) else {
                        return Err(make_semantic_error!(UnknownField).into());
                    };

                    let ty = self.analyzer.get_type_by_id(struct_field);

                    let value = self.visit_expr(expr, Some(&ty))?;

                    dic.insert(name.clone(), value);
                }

                if !struct_fields.is_empty() {
                    return Err(make_semantic_error!(MissingField).into());
                }

                Ok(ConstEvalValue::Struct(self.analyzer.get_full_name_from(id, struct_name), dic))
            }
            TypeKind::Enum { fields: _ } | TypeKind::Trait { methods: _, constants: _ } => Err(make_const_eval_error!(NotStructType)),
        }
    }

    fn visit_repeat_expr<'tmp>(&mut self, RepeatExpr(expr, rep_time_expr): &'ast RepeatExpr, extra: Self::ExprExtra<'tmp>) -> Self::ExprRes {
        let rep_time = self.visit_expr(&rep_time_expr.value, Some(&ResolvedTy::usize()))?.into_u_size().unwrap();

        let ty = extract_extra!(extra, ResolvedTy::Array(inner, len), CON, {
            if rep_time != *len {
                return Err(make_const_eval_error!(TypeMisMatch));
            }

            inner.as_ref()
        });

        let value = self.visit_expr(expr, ty)?;

        let values: Vec<ConstEvalValue> = repeat_n(value, rep_time as usize).collect();

        Ok(ConstEvalValue::Array(values))
    }

    fn visit_wild_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::WildPat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_ident_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::IdentPat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_struct_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::StructPat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_or_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::OrPat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_path_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::PathPat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_tuple_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::TuplePat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_ref_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::RefPat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_lit_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::LitPat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_range_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::RangePat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_slice_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::SlicePat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_rest_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::RestPat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }

    fn visit_pat<'tmp>(&mut self, _pat: &'ast crate::ast::pat::Pat, extra: Self::PatExtra<'tmp>) -> Self::PatRes {
        Err(make_const_eval_error!(NotSupportedExpr))
    }
}
