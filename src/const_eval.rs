pub mod literal_eval;

use std::{collections::HashMap, iter::repeat_n};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        NodeId, Span, Symbol,
        expr::{
            ArrayExpr, BinOp, BinaryExpr, CastExpr, Expr, ExprKind, FieldExpr, IndexExpr, LitExpr,
            PathExpr, RepeatExpr, StructExpr, TupExpr, UnOp, UnaryExpr,
        },
        item::ConstItem,
    },
    semantics::{
        AnalyzerState, SemanticAnalyzer,
        error::SemanticError,
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
            fn try_into<T, S>(num: T) -> Result<S, ConstEvalError>
            where
                T: TryInto<S>,
            {
                num.try_into().map_err(|_| ConstEvalError::Overflow)
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
            ConstEvalValue::Array(values) => {
                let ResolvedTy::Array(elm_ty, size) = ty else {
                    return Err(ConstEvalError::TypeMisMatch);
                };

                if values.len() != *size as usize {
                    return Err(ConstEvalError::TypeMisMatch);
                }

                let elms = values
                    .into_iter()
                    .map(|x| x.cast(elm_ty))
                    .collect::<Result<Vec<ConstEvalValue>, ConstEvalError>>()?;

                Ok(ConstEvalValue::Array(elms))
            }
            ConstEvalValue::Placeholder
            | ConstEvalValue::UnitStruct(_)
            | ConstEvalValue::RefStr(_)
            | ConstEvalValue::Struct(_, _)
            | ConstEvalValue::Enum(..) => {
                if Into::<ResolvedTy>::into(&self) == *ty {
                    Ok(self)
                } else {
                    Err(ConstEvalError::NotSupportedCast)
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
            ConstEvalValue::UnitStruct(full_name)
            | ConstEvalValue::Struct(full_name, _)
            | ConstEvalValue::Enum(full_name, _) => ResolvedTy::Named(full_name.clone()),
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
            ConstEvalValue::UnEvaled(..) => panic!("Impossible!"),
        }
    }
}

#[derive(Debug)]
pub enum ConstEvalError {
    NotSupportedExpr,
    IncorrectSuffix,
    Overflow,
    InvalidDigit,
    TypeMisMatch,
    Semantic(Box<SemanticError>),
    NotAStruct,
    NotStructField,
    NotSupportedCast,
    OutOfBound,
    NonConstVariable,
    NotStructType,
    NotSupportedBinary,
}

impl From<SemanticError> for ConstEvalError {
    fn from(value: SemanticError) -> Self {
        match value {
            SemanticError::ConstEvalError(err) => err,
            _ => Self::Semantic(Box::new(value)),
        }
    }
}

pub struct ConstEvaler<'a> {
    analyzer: &'a mut SemanticAnalyzer,
}

impl<'a> ConstEvaler<'a> {
    pub fn eval<'ast>(
        analyzer: &'a mut SemanticAnalyzer,
        expr: &'ast Expr,
    ) -> Result<ConstEvalValue, ConstEvalError> {
        let mut evaler = Self { analyzer };
        evaler.visit_expr(expr)
    }
}

impl<'a, 'ast> Visitor<'ast> for ConstEvaler<'a> {
    type DefaultRes = Result<(), ConstEvalError>;
    type ExprRes = Result<ConstEvalValue, ConstEvalError>;
    type PatRes = Self::DefaultRes;
    type StmtRes = Self::DefaultRes;

    fn visit_crate(&mut self, _krate: &'ast crate::ast::Crate) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_item(&mut self, _item: &'ast crate::ast::item::Item) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_associate_item(
        &mut self,
        _item: &'ast crate::ast::item::Item<crate::ast::item::AssocItemKind>,
    ) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_const_item(&mut self, _item: &'ast crate::ast::item::ConstItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_fn_item(&mut self, _item: &'ast crate::ast::item::FnItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_mod_item(&mut self, _item: &'ast crate::ast::item::ModItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_enum_item(&mut self, _item: &'ast crate::ast::item::EnumItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_struct_item(&mut self, _item: &'ast crate::ast::item::StructItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_trait_item(&mut self, _item: &'ast crate::ast::item::TraitItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_impl_item(&mut self, _item: &'ast crate::ast::item::ImplItem) -> Self::DefaultRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_stmt(&mut self, _stmt: &'ast crate::ast::stmt::Stmt) -> Self::StmtRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_let_stmt(&mut self, _stmt: &'ast crate::ast::stmt::LocalStmt) -> Self::StmtRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_expr(&mut self, expr: &'ast crate::ast::expr::Expr) -> Self::ExprRes {
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

    fn visit_array_expr(&mut self, ArrayExpr(exprs): &'ast ArrayExpr) -> Self::ExprRes {
        let values = exprs
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<_>, ConstEvalError>>()?;

        let tys = values.iter().map(|x| x.into()).collect::<Vec<ResolvedTy>>();
        let ut_ty = ResolvedTy::utilize(tys).ok_or(ConstEvalError::TypeMisMatch)?;

        let values = values
            .into_iter()
            .map(|x| x.cast(&ut_ty))
            .collect::<Result<Vec<_>, ConstEvalError>>()?;

        Ok(ConstEvalValue::Array(values))
    }

    fn visit_const_block_expr(
        &mut self,
        _expr: &'ast crate::ast::expr::ConstBlockExpr,
    ) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_call_expr(&mut self, _expr: &'ast crate::ast::expr::CallExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_method_call_expr(
        &mut self,
        _expr: &'ast crate::ast::expr::MethodCallExpr,
    ) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_tup_expr(&mut self, TupExpr(exprs): &'ast TupExpr) -> Self::ExprRes {
        match &exprs[..] {
            [expr] => self.visit_expr(expr),
            _ => Err(ConstEvalError::NotSupportedExpr),
        }
    }

    fn visit_binary_expr(
        &mut self,
        BinaryExpr(binop, expr1, expr2): &'ast BinaryExpr,
    ) -> Self::ExprRes {
        let value1 = self.visit_expr(expr1)?;
        let value2 = self.visit_expr(expr2)?;

        if let (ConstEvalValue::Bool(value1), ConstEvalValue::Bool(value2)) = (&value1, &value2) {
            Ok(ConstEvalValue::Bool(match binop {
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
                _ => return Err(ConstEvalError::NotSupportedBinary),
            }))
        } else if value1.is_number() && value2.is_number() {
            match binop {
                BinOp::Shl | BinOp::Shr => {
                    let value2 = value2.cast(&ResolvedTy::u32())?.into_u32().unwrap();

                    macro_rules! number_operation {
                        ($value1:expr, $value2:expr, [$($num_ty:ident),*]) => {
                            match $value1 {
                                $(
                                    ConstEvalValue::$num_ty(value1) => Ok(match binop {
                                        BinOp::Shl => ConstEvalValue::$num_ty(
                                            value1.checked_shl(value2).ok_or(ConstEvalError::Overflow)?,
                                        ),
                                        BinOp::Shr => ConstEvalValue::$num_ty(
                                            value1.checked_shr(value2).ok_or(ConstEvalError::Overflow)?,
                                        ),
                                        _ => return Err(ConstEvalError::NotSupportedBinary),
                                    }),
                                )*
                                _ => panic!("Impossible!"),
                            }
                        };
                    }

                    number_operation!(
                        value1,
                        value2,
                        [U32, I32, USize, ISize, Integer, SignedInteger]
                    )
                }
                _ => {
                    let ty = ResolvedTy::utilize(vec![(&value1).into(), (&value2).into()])
                        .ok_or(ConstEvalError::NotSupportedBinary)?;

                    let value1 = value1.cast(&ty).unwrap();
                    let value2 = value2.cast(&ty).unwrap();

                    macro_rules! number_operation {
                        ($value1:expr, $value2: expr, [$($num_ty:ident),*]) => {
                            match ($value1, $value2) {
                                $(
                                    (ConstEvalValue::$num_ty(value1), ConstEvalValue::$num_ty(value2)) => Ok(match binop {
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
                                            value1.checked_add(value2).ok_or(ConstEvalError::Overflow)?,
                                        ),
                                        BinOp::Sub => ConstEvalValue::$num_ty(
                                            value1.checked_sub(value2).ok_or(ConstEvalError::Overflow)?,
                                        ),
                                        BinOp::Mul => ConstEvalValue::$num_ty(
                                            value1.checked_mul(value2).ok_or(ConstEvalError::Overflow)?,
                                        ),
                                        BinOp::Div => ConstEvalValue::$num_ty(
                                            value1.checked_div(value2).ok_or(ConstEvalError::Overflow)?,
                                        ),
                                        BinOp::Rem => ConstEvalValue::$num_ty(
                                            value1.checked_rem(value2).ok_or(ConstEvalError::Overflow)?,
                                        ),
                                        _ => return Err(ConstEvalError::NotSupportedBinary),
                                    }),
                                )*
                                _ => panic!("Impossible!"),
                            }
                        };
                    }

                    number_operation!(
                        value1,
                        value2,
                        [U32, I32, USize, ISize, Integer, SignedInteger]
                    )
                }
            }
        } else {
            Err(ConstEvalError::NotSupportedBinary)
        }
    }

    fn visit_unary_expr(&mut self, UnaryExpr(unop, expr): &'ast UnaryExpr) -> Self::ExprRes {
        let value = self.visit_expr(expr)?;

        match unop {
            UnOp::Deref => Err(ConstEvalError::NotSupportedExpr),
            UnOp::Not => match value {
                ConstEvalValue::U32(v) => Ok(ConstEvalValue::U32(!v)),
                ConstEvalValue::I32(v) => Ok(ConstEvalValue::I32(!v)),
                ConstEvalValue::USize(v) => Ok(ConstEvalValue::USize(!v)),
                ConstEvalValue::ISize(v) => Ok(ConstEvalValue::ISize(!v)),
                ConstEvalValue::Integer(v) => Ok(ConstEvalValue::Integer(!v)),
                ConstEvalValue::SignedInteger(v) => Ok(ConstEvalValue::SignedInteger(!v)),
                ConstEvalValue::Bool(v) => Ok(ConstEvalValue::Bool(!v)),
                _ => Err(ConstEvalError::NotSupportedExpr),
            },
            UnOp::Neg => match value {
                ConstEvalValue::I32(v) => Ok(ConstEvalValue::I32(
                    v.checked_neg().ok_or(ConstEvalError::Overflow)?,
                )),
                ConstEvalValue::ISize(v) => Ok(ConstEvalValue::ISize(
                    v.checked_neg().ok_or(ConstEvalError::Overflow)?,
                )),
                ConstEvalValue::Integer(v) => Ok(ConstEvalValue::SignedInteger(
                    TryInto::<i32>::try_into(v)
                        .map_err(|_| ConstEvalError::Overflow)?
                        .checked_neg()
                        .ok_or(ConstEvalError::Overflow)?,
                )),
                ConstEvalValue::SignedInteger(v) => Ok(ConstEvalValue::SignedInteger(
                    v.checked_neg().ok_or(ConstEvalError::Overflow)?,
                )),
                _ => Err(ConstEvalError::NotSupportedExpr),
            },
        }
    }

    fn visit_lit_expr(&mut self, expr: &'ast LitExpr) -> Self::ExprRes {
        expr.try_into()
    }

    fn visit_cast_expr(&mut self, CastExpr(expr, cast_ty): &'ast CastExpr) -> Self::ExprRes {
        let cast_ty = self.analyzer.resolve_ty(cast_ty)?;

        let expr_res = self.visit_expr(expr)?;
        expr_res.cast(&cast_ty)
    }

    fn visit_let_expr(&mut self, _expr: &'ast crate::ast::expr::LetExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_if_expr(&mut self, _expr: &'ast crate::ast::expr::IfExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_while_expr(&mut self, _expr: &'ast crate::ast::expr::WhileExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_for_loop_expr(&mut self, _expr: &'ast crate::ast::expr::ForLoopExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_loop_expr(&mut self, _expr: &'ast crate::ast::expr::LoopExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_match_expr(&mut self, _expr: &'ast crate::ast::expr::MatchExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_block_expr(&mut self, _expr: &'ast crate::ast::expr::BlockExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_assign_expr(&mut self, _expr: &'ast crate::ast::expr::AssignExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_assign_op_expr(
        &mut self,
        _expr: &'ast crate::ast::expr::AssignOpExpr,
    ) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_field_expr(&mut self, FieldExpr(expr, ident): &'ast FieldExpr) -> Self::ExprRes {
        let ret = match self.visit_expr(expr)? {
            ConstEvalValue::Struct(_, mut hash_map) => match hash_map.remove(&ident.symbol) {
                Some(value) => value,
                None => return Err(ConstEvalError::NotStructField),
            },
            _ => return Err(ConstEvalError::NotAStruct),
        };
        Ok(ret)
    }

    fn visit_index_expr(&mut self, IndexExpr(array, index): &'ast IndexExpr) -> Self::ExprRes {
        let array = self.visit_expr(array)?;
        let index = self.visit_expr(index)?;

        let index = match index {
            ConstEvalValue::USize(index) | ConstEvalValue::Integer(index) => index,
            _ => return Err(ConstEvalError::TypeMisMatch),
        } as usize;

        let ConstEvalValue::Array(mut array) = array else {
            return Err(ConstEvalError::TypeMisMatch);
        };

        if index >= array.len() {
            return Err(ConstEvalError::OutOfBound);
        }

        Ok(array.remove(index))
    }

    fn visit_range_expr(&mut self, _expr: &'ast crate::ast::expr::RangeExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_underscore_expr(
        &mut self,
        _expr: &'ast crate::ast::expr::UnderscoreExpr,
    ) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_path_expr(&mut self, PathExpr(qself, path): &'ast PathExpr) -> Self::ExprRes {
        let old_state = self.analyzer.state;
        let value = self.analyzer.search_value_by_path(qself, path)?;

        match value {
            ValueContainer::Variable(v) => match &v.kind {
                VariableKind::Decl | VariableKind::Inited | VariableKind::Fn => {
                    Err(ConstEvalError::NonConstVariable)
                }
                VariableKind::Constant(const_eval_value) => match const_eval_value {
                    ConstEvalValue::UnEvaled(node_id, span, item_ptr) => {
                        let var_ptr = &raw const *v;

                        let item = unsafe { &**item_ptr };
                        self.analyzer.state = AnalyzerState {
                            current_ast_id: *node_id,
                            current_span: *span,
                        };

                        let ty = self.analyzer.resolve_ty(&item.ty)?;
                        let ty_id = self.analyzer.intern_type(ty.clone());
                        let evaled_value =
                            self.analyzer.const_eval(ty, item.expr.as_ref().unwrap())?;

                        let var = self.analyzer.search_value_mut(&item.ident.symbol)?;

                        assert!(std::ptr::eq(var_ptr, &raw const *var));

                        *var = Variable {
                            ty: ty_id,
                            mutbl: crate::ast::Mutability::Not,
                            kind: VariableKind::Constant(evaled_value.clone()),
                        };

                        self.analyzer.state = old_state;

                        Ok(evaled_value)
                    }
                    _ => Ok(const_eval_value.clone()),
                },
            },
            ValueContainer::ImplInfoItem(_, ImplInfoItem::Constant(constant)) => {
                Ok(constant.value.clone())
            }
            ValueContainer::ImplInfoItem(_, ImplInfoItem::Method(_)) => {
                Err(ConstEvalError::NotSupportedExpr)
            }
            ValueContainer::Temp(variable) => match &variable.kind {
                VariableKind::Decl | VariableKind::Inited | VariableKind::Fn => {
                    Err(ConstEvalError::NonConstVariable)
                }
                VariableKind::Constant(const_eval_value) => Ok(const_eval_value.clone()),
            },
        }
    }

    fn visit_addr_of_expr(&mut self, _expr: &'ast crate::ast::expr::AddrOfExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_break_expr(&mut self, _expr: &'ast crate::ast::expr::BreakExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_continue_expr(
        &mut self,
        _expr: &'ast crate::ast::expr::ContinueExpr,
    ) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_ret_expr(&mut self, _expr: &'ast crate::ast::expr::RetExpr) -> Self::ExprRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_struct_expr(
        &mut self,
        StructExpr {
            qself,
            path,
            fields,
            rest,
        }: &'ast StructExpr,
    ) -> Self::ExprRes {
        if !matches!(rest, crate::ast::expr::StructRest::None) {
            return Err(ConstEvalError::NotSupportedExpr);
        };

        let exp_fields = fields
            .iter()
            .map(|x| -> Result<(Symbol, ConstEvalValue), ConstEvalError> {
                Ok((x.ident.symbol.clone(), self.visit_expr(&x.expr)?))
            })
            .collect::<Result<Vec<_>, ConstEvalError>>()?;

        let (id, struct_info) = self.analyzer.search_type_by_path(qself, path)?;

        match &struct_info.kind {
            TypeKind::Placeholder => panic!("Impossible"),
            TypeKind::Struct { fields } => {
                let mut dic = HashMap::new();

                for x in exp_fields.into_iter() {
                    if dic.insert(x.0, x.1).is_some() {
                        return Err(ConstEvalError::Semantic(Box::new(
                            SemanticError::MultiSpecifiedField,
                        )));
                    }
                }

                for (field_ident, field_type_id) in fields {
                    let Some((s, res)) = dic.remove_entry(field_ident) else {
                        return Err(ConstEvalError::Semantic(Box::new(
                            SemanticError::MissingField,
                        )));
                    };
                    let field_ty = self.analyzer.get_type_by_id(*field_type_id);
                    dic.insert(s, res.cast(&field_ty)?);
                }

                Ok(ConstEvalValue::Struct(
                    self.analyzer
                        .get_full_name_from(id, struct_info.name.clone()),
                    dic,
                ))
            }
            TypeKind::Enum { fields: _ }
            | TypeKind::Trait {
                methods: _,
                constants: _,
            } => Err(ConstEvalError::NotStructType),
        }
    }

    fn visit_repeat_expr(
        &mut self,
        RepeatExpr(expr, rep_time_expr): &'ast RepeatExpr,
    ) -> Self::ExprRes {
        let rep_time = self
            .visit_expr(&rep_time_expr.value)?
            .into_u_size()
            .unwrap();

        let value = self.visit_expr(expr)?;

        let values: Vec<ConstEvalValue> = repeat_n(value, rep_time as usize).collect();

        Ok(ConstEvalValue::Array(values))
    }

    fn visit_wild_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::WildPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_ident_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::IdentPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_struct_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::StructPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_or_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::OrPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_path_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::PathPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_tuple_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::TuplePat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_ref_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::RefPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_lit_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::LitPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_range_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::RangePat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_slice_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::SlicePat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_rest_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::RestPat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }

    fn visit_pat(
        &mut self,
        _pat: &'ast crate::ast::pat::Pat,
        _expected_ty: crate::semantics::utils::TypeId,
    ) -> Self::PatRes {
        Err(ConstEvalError::NotSupportedExpr)
    }
}
