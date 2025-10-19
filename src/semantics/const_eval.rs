use crate::{
    ast::{NodeId, expr::*, item::*, pat::*, stmt::*},
    impossible, make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        resolved_ty::{ResolvedTy, TypeIntern, TypeKey},
        value::{ConstantValue, UnEvalConstant},
        visitor::Visitor,
    },
};

impl SemanticAnalyzer {
    pub fn const_eval(
        &mut self,
        expr: &Expr,
        target_type: TypeIntern,
        scope: Option<NodeId>,
    ) -> Result<ConstantValue, SemanticError> {
        let mut evaler = ConstEvaler {
            analyzer: self,
            scope_id: scope,
        };
        evaler.visit_expr(expr, target_type.to_key().into())
    }

    pub fn eval_unevaling(
        &mut self,
        uneval: &UnEvalConstant,
        target_type: TypeKey,
    ) -> Result<ConstantValue, SemanticError> {
        let (scope, e) = uneval.to_ref();
        self.const_eval(e, target_type.into(), Some(scope))
    }
}

struct Extra {
    ty: TypeKey,
    allow_i32_max: bool,
}

impl From<TypeKey> for Extra {
    fn from(value: TypeKey) -> Self {
        Self {
            ty: value,
            allow_i32_max: false,
        }
    }
}

struct ConstEvaler<'analyzer> {
    analyzer: &'analyzer mut SemanticAnalyzer,
    scope_id: Option<NodeId>,
}

impl<'ast, 'analyzer> Visitor<'ast> for ConstEvaler<'analyzer> {
    type DefaultRes<'res>
        = Result<(), SemanticError>
    where
        Self: 'res;

    type ExprRes<'res>
        = Result<ConstantValue, SemanticError>
    where
        Self: 'res;

    type PatRes<'res>
        = Result<(), SemanticError>
    where
        Self: 'res;

    type StmtRes<'res>
        = Result<(), SemanticError>
    where
        Self: 'res;

    type CrateExtra<'tmp> = ();

    type ItemExtra<'tmp> = ();

    type StmtExtra<'tmp> = ();

    type ExprExtra<'tmp> = Extra;

    type PatExtra<'tmp> = ();

    fn visit_crate<'tmp>(
        &mut self,
        _krate: &'ast crate::ast::Crate,
        _extra: Self::CrateExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_item<'tmp>(
        &mut self,
        item: &'ast Item,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation).set_span(&item.span))
    }

    fn visit_const_item<'tmp>(
        &mut self,
        _item: &'ast ConstItem,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_fn_item<'tmp>(
        &mut self,
        _item: &'ast FnItem,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_mod_item<'tmp>(
        &mut self,
        _item: &'ast ModItem,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_enum_item<'tmp>(
        &mut self,
        _item: &'ast EnumItem,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_struct_item<'tmp>(
        &mut self,
        _item: &'ast StructItem,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_trait_item<'tmp>(
        &mut self,
        _item: &'ast TraitItem,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_impl_item<'tmp>(
        &mut self,
        _item: &'ast ImplItem,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_associate_item<'tmp>(
        &mut self,
        _item: &'ast crate::ast::item::Item<crate::ast::item::AssocItemKind>,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        impossible!()
    }

    fn visit_stmt<'tmp>(
        &mut self,
        stmt: &'ast Stmt,
        _extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation).set_span(&stmt.span))
    }

    fn visit_local_stmt<'tmp>(
        &mut self,
        _stmt: &'ast LocalStmt,
        _extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        impossible!()
    }

    fn visit_expr<'tmp>(
        &mut self,
        Expr { kind, span, id: _ }: &'ast Expr,
        mut extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        if !matches!(kind, ExprKind::Unary(_) | ExprKind::Lit(_)) {
            extra.allow_i32_max = false;
        }

        match kind {
            ExprKind::Array(e) => self.visit_array_expr(e, extra),
            ExprKind::ConstBlock(e) => self.visit_const_block_expr(e, extra),
            ExprKind::Call(e) => self.visit_call_expr(e, extra),
            ExprKind::MethodCall(e) => self.visit_method_call_expr(e, extra),
            ExprKind::Tup(e) => self.visit_tup_expr(e, extra),
            ExprKind::Binary(e) => self.visit_binary_expr(e, extra),
            ExprKind::Unary(e) => self.visit_unary_expr(e, extra),
            ExprKind::Lit(e) => self.visit_lit_expr(e, extra),
            ExprKind::Cast(e) => self.visit_cast_expr(e, extra),
            ExprKind::Let(e) => self.visit_let_expr(e, extra),
            ExprKind::If(e) => self.visit_if_expr(e, extra),
            ExprKind::While(e) => self.visit_while_expr(e, extra),
            ExprKind::ForLoop(e) => self.visit_for_loop_expr(e, extra),
            ExprKind::Loop(e) => self.visit_loop_expr(e, extra),
            ExprKind::Match(e) => self.visit_match_expr(e, extra),
            ExprKind::Block(e) => self.visit_block_expr(e, extra),
            ExprKind::Assign(e) => self.visit_assign_expr(e, extra),
            ExprKind::AssignOp(e) => self.visit_assign_op_expr(e, extra),
            ExprKind::Field(e) => self.visit_field_expr(e, extra),
            ExprKind::Index(e) => self.visit_index_expr(e, extra),
            ExprKind::Range(e) => self.visit_range_expr(e, extra),
            ExprKind::Underscore(e) => self.visit_underscore_expr(e, extra),
            ExprKind::Path(e) => self.visit_path_expr(e, extra),
            ExprKind::AddrOf(e) => self.visit_addr_of_expr(e, extra),
            ExprKind::Break(e) => self.visit_break_expr(e, extra),
            ExprKind::Continue(e) => self.visit_continue_expr(e, extra),
            ExprKind::Ret(e) => self.visit_ret_expr(e, extra),
            ExprKind::Struct(e) => self.visit_struct_expr(e, extra),
            ExprKind::Repeat(e) => self.visit_repeat_expr(e, extra),
        }
        .map_err(|e| e.set_span(span))
    }

    fn visit_array_expr<'tmp>(
        &mut self,
        ArrayExpr(exprs): &'ast ArrayExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let right_ty =
            ResolvedTy::array_type(self.analyzer.new_any_type(), Some(exprs.len() as u32));
        self.analyzer.type_eq(extra.ty.into(), right_ty)?;
        let ty = self.analyzer.probe_type(extra.ty.into()).unwrap();
        let (inner, _) = ty.kind.into_array().unwrap();

        let values = exprs
            .iter()
            .map(|x| self.visit_expr(x, inner.to_key().into()))
            .collect::<Result<Vec<_>, SemanticError>>()?;

        Ok(ConstantValue::ConstantArray(values))
    }

    fn visit_const_block_expr<'tmp>(
        &mut self,
        _expr: &'ast ConstBlockExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_call_expr<'tmp>(
        &mut self,
        _expr: &'ast CallExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_method_call_expr<'tmp>(
        &mut self,
        _expr: &'ast MethodCallExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_tup_expr<'tmp>(
        &mut self,
        TupExpr(exprs): &'ast TupExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        match &exprs[..] {
            [] => {
                self.analyzer
                    .ty_intern_eq(extra.ty.into(), self.analyzer.unit_type())?;
                Ok(ConstantValue::Unit)
            }
            [e] => self.visit_expr(e, extra),
            _ => Err(make_semantic_error!(ConstEvalNoImplementation)),
        }
    }

    fn visit_binary_expr<'tmp>(
        &mut self,
        BinaryExpr(bin_op, expr1, expr2): &'ast BinaryExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let intern = extra.ty;
        let ty = self.analyzer.probe_type(intern.into()).unwrap();

        let v = match bin_op {
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Rem
            | BinOp::BitXor
            | BinOp::BitAnd
            | BinOp::BitOr => {
                let v1 = self.visit_expr(expr1, intern.into())?;
                let v2 = self.visit_expr(expr2, intern.into())?;

                if let (ConstantValue::ConstantInt(i1), ConstantValue::ConstantInt(i2)) = (v1, v2) {
                    if ty.is_any_int_type() {
                        match bin_op {
                            BinOp::Add => i1.wrapping_add(i2),
                            BinOp::Sub => i1.wrapping_sub(i2),
                            BinOp::Mul => i1.wrapping_mul(i2),
                            BinOp::Div => {
                                if ty.is_any_signed_int_type() {
                                    (i1 as i32).wrapping_div(i2 as i32) as u32
                                } else {
                                    i1.wrapping_div(i2)
                                }
                            }
                            BinOp::Rem => {
                                if ty.is_any_signed_int_type() {
                                    (i1 as i32).wrapping_rem(i2 as i32) as u32
                                } else {
                                    i1.wrapping_div(i2)
                                }
                            }
                            BinOp::BitXor => i1 ^ i2,
                            BinOp::BitAnd => i1 & i2,
                            BinOp::BitOr => i1 | i2,
                            _ => impossible!(),
                        }
                    } else if ty.is_bool_type() {
                        match bin_op {
                            BinOp::BitXor => i1 ^ i2,
                            BinOp::BitAnd => i1 & i2,
                            BinOp::BitOr => i1 | i2,
                            _ => return Err(make_semantic_error!(NoBinaryOperation)),
                        }
                    } else {
                        return Err(make_semantic_error!(NoBinaryOperation));
                    }
                } else {
                    return Err(make_semantic_error!(NoBinaryOperation));
                }
            }

            BinOp::And | BinOp::Or => {
                let v1 = self
                    .visit_expr(expr1, self.analyzer.bool_type().to_key().into())?
                    .into_constant_int()
                    .unwrap();
                let v2 = self
                    .visit_expr(expr2, self.analyzer.bool_type().to_key().into())?
                    .into_constant_int()
                    .unwrap();
                match bin_op {
                    BinOp::And => ((v1 != 0) && (v2 != 0)) as u32,
                    BinOp::Or => ((v1 != 0) || (v2 != 0)) as u32,
                    _ => impossible!(),
                }
            }

            BinOp::Shl | BinOp::Shr => {
                let right = self.analyzer.new_any_int_type();
                self.analyzer.ty_intern_eq(intern.into(), right)?;
                let v1 = self
                    .visit_expr(expr1, intern.into())?
                    .into_constant_int()
                    .unwrap();
                let new_any_int_type = self.analyzer.new_any_int_type();
                let v2 = self
                    .visit_expr(expr2, new_any_int_type.to_key().into())?
                    .into_constant_int()
                    .unwrap();
                match bin_op {
                    BinOp::Shl => v1.unbounded_shl(v2),
                    BinOp::Shr => {
                        if ty.is_signed_integer() {
                            (v1 as i32).wrapping_shr(v2) as u32
                        } else {
                            v1.wrapping_shr(v2)
                        }
                    }
                    _ => impossible!(),
                }
            }

            BinOp::Eq | BinOp::Lt | BinOp::Le | BinOp::Ne | BinOp::Ge | BinOp::Gt => {
                let exp_intern = self.analyzer.new_any_type();
                let v1 = self.visit_expr(expr1, exp_intern.to_key().into())?;
                let v2 = self.visit_expr(expr2, exp_intern.to_key().into())?;
                let exp_ty = self.analyzer.probe_type(exp_intern).unwrap();
                if let (ConstantValue::ConstantInt(i1), ConstantValue::ConstantInt(i2)) = (v1, v2) {
                    (match bin_op {
                        BinOp::Eq => i1 == i2,
                        BinOp::Lt => {
                            if exp_ty.is_signed_integer() {
                                (i1 as i32) < (i2 as i32)
                            } else {
                                i1 < i2
                            }
                        }
                        BinOp::Le => {
                            if exp_ty.is_signed_integer() {
                                (i1 as i32) <= (i2 as i32)
                            } else {
                                i1 <= i2
                            }
                        }
                        BinOp::Ne => i1 != i2,
                        BinOp::Ge => {
                            if exp_ty.is_signed_integer() {
                                (i1 as i32) >= (i2 as i32)
                            } else {
                                i1 >= i2
                            }
                        }
                        BinOp::Gt => {
                            if exp_ty.is_signed_integer() {
                                (i1 as i32) > (i2 as i32)
                            } else {
                                i1 > i2
                            }
                        }
                        _ => impossible!(),
                    }) as u32
                } else {
                    return Err(make_semantic_error!(NoBinaryOperation));
                }
            }
        };

        Ok(ConstantValue::ConstantInt(v))
    }

    fn visit_unary_expr<'tmp>(
        &mut self,
        UnaryExpr(un_op, expr): &'ast UnaryExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let mut expr_value = self.visit_expr(
            expr,
            Extra {
                allow_i32_max: if matches!(un_op, UnOp::Neg) {
                    !extra.allow_i32_max
                } else {
                    extra.allow_i32_max
                },
                ..extra
            },
        )?;

        match un_op {
            UnOp::Deref => return Err(make_semantic_error!(ConstEvalNoImplementation)),
            UnOp::Not => match &mut expr_value {
                ConstantValue::ConstantInt(i) => *i = !*i,
                _ => return Err(make_semantic_error!(ConstEvalNoImplementation)),
            },
            UnOp::Neg => match &mut expr_value {
                ConstantValue::ConstantInt(i) => *i = (-(*i as i32)) as u32,
                _ => return Err(make_semantic_error!(ConstEvalNoImplementation)),
            },
        }

        Ok(expr_value)
    }

    fn visit_lit_expr<'tmp>(
        &mut self,
        LitExpr {
            kind,
            symbol,
            suffix,
        }: &'ast LitExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let (ty, value) = match kind {
            LitKind::Bool => (
                self.analyzer.bool_type(),
                ConstantValue::ConstantInt(if symbol == "true" { 1 } else { 0 }),
            ),
            LitKind::Char => (
                self.analyzer.char_type(),
                ConstantValue::ConstantInt(symbol.chars().next().unwrap() as u32),
            ),
            LitKind::Integer => {
                let intern = match suffix.as_ref().map(|x| x.as_ref()) {
                    Some("i32") => self.analyzer.i32_type(),
                    Some("isize") => self.analyzer.isize_type(),
                    Some("u32") => self.analyzer.u32_type(),
                    Some("usize") => self.analyzer.usize_type(),
                    None => self.analyzer.new_any_int_type(),
                    Some(_) => return Err(make_semantic_error!(UnknownSuffix)),
                };

                self.analyzer.ty_intern_eq(extra.ty.into(), intern)?;

                let ty = self.analyzer.probe_type(intern).unwrap();

                let value: u32 = symbol.parse().map_err(|_| make_semantic_error!(Overflow))?;

                use crate::semantics::resolved_ty::BuiltInTyKind::*;
                use crate::semantics::resolved_ty::ResolvedTyKind::*;
                if matches!(&ty.kind, BuiltIn(I32) | BuiltIn(ISize))
                    && ((value > 2147483648) || (value == 2147483648 && !extra.allow_i32_max))
                {
                    return Err(make_semantic_error!(Overflow));
                }

                (intern, ConstantValue::ConstantInt(value))
            }
            LitKind::Str | LitKind::StrRaw(_) => (
                self.analyzer.ref_str_type(),
                ConstantValue::ConstantString(symbol.clone()),
            ),
            _ => impossible!(),
        };

        self.analyzer.ty_intern_eq(extra.ty.into(), ty)?;
        Ok(value)
    }

    fn visit_cast_expr<'tmp>(
        &mut self,
        _expr: &'ast CastExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_let_expr<'tmp>(
        &mut self,
        _expr: &'ast LetExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_if_expr<'tmp>(
        &mut self,
        _expr: &'ast IfExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_while_expr<'tmp>(
        &mut self,
        _expr: &'ast WhileExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_for_loop_expr<'tmp>(
        &mut self,
        _expr: &'ast ForLoopExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_loop_expr<'tmp>(
        &mut self,
        _expr: &'ast LoopExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_match_expr<'tmp>(
        &mut self,
        _expr: &'ast MatchExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_block_expr<'tmp>(
        &mut self,
        _expr: &'ast BlockExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_assign_expr<'tmp>(
        &mut self,
        _expr: &'ast AssignExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_assign_op_expr<'tmp>(
        &mut self,
        _expr: &'ast AssignOpExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_field_expr<'tmp>(
        &mut self,
        _expr: &'ast FieldExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_index_expr<'tmp>(
        &mut self,
        IndexExpr(array, index): &'ast IndexExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let index_value = self
            .visit_expr(&index, self.analyzer.usize_type().to_key().into())?
            .into_constant_int()
            .unwrap();

        let array_ty = ResolvedTy::array_type(extra.ty.into(), None);
        let array_intern = self.analyzer.intern_type(array_ty);
        let mut array_value = self
            .visit_expr(&array, array_intern.into())?
            .into_constant_array()
            .unwrap();

        if array_value.len() >= index_value as usize {
            return Err(make_semantic_error!(IndexOutOfBound));
        }

        Ok(array_value.remove(index_value as usize))
    }

    fn visit_range_expr<'tmp>(
        &mut self,
        _expr: &'ast RangeExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_underscore_expr<'tmp>(
        &mut self,
        _expr: &'ast UnderscoreExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_path_expr<'tmp>(
        &mut self,
        PathExpr(qself, path): &'ast PathExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let value_index =
            self.analyzer
                .search_value_by_path(self.scope_id.unwrap(), qself, path)?;

        let value = self.analyzer.get_place_value_by_index(&value_index).clone();
        let ty = value.value.ty.clone();
        self.analyzer.ty_intern_eq(extra.ty.into(), ty)?;
        let const_value = value
            .value
            .kind
            .as_constant()
            .cloned()
            .ok_or(make_semantic_error!(NotConstantValue))?;

        Ok(const_value)
    }

    fn visit_addr_of_expr<'tmp>(
        &mut self,
        _expr: &'ast AddrOfExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_break_expr<'tmp>(
        &mut self,
        _expr: &'ast BreakExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_continue_expr<'tmp>(
        &mut self,
        _expr: &'ast ContinueExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_ret_expr<'tmp>(
        &mut self,
        _expr: &'ast RetExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_struct_expr<'tmp>(
        &mut self,
        _expr: &'ast StructExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_repeat_expr<'tmp>(
        &mut self,
        _expr: &'ast RepeatExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation))
    }

    fn visit_pat<'tmp>(
        &mut self,
        pat: &'ast Pat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        Err(make_semantic_error!(ConstEvalNoImplementation).set_span(&pat.span))
    }

    fn visit_wild_pat<'tmp>(
        &mut self,
        _pat: &'ast WildPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_ident_pat<'tmp>(
        &mut self,
        _pat: &'ast IdentPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_struct_pat<'tmp>(
        &mut self,
        _pat: &'ast StructPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_or_pat<'tmp>(
        &mut self,
        _pat: &'ast OrPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_path_pat<'tmp>(
        &mut self,
        _pat: &'ast PathPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_tuple_pat<'tmp>(
        &mut self,
        _pat: &'ast TuplePat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_ref_pat<'tmp>(
        &mut self,
        _pat: &'ast RefPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_lit_pat<'tmp>(
        &mut self,
        _pat: &'ast LitPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_range_pat<'tmp>(
        &mut self,
        _pat: &'ast RangePat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_slice_pat<'tmp>(
        &mut self,
        _pat: &'ast SlicePat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }

    fn visit_rest_pat<'tmp>(
        &mut self,
        _pat: &'ast RestPat,
        _extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        impossible!()
    }
}
