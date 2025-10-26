use std::iter::zip;

use crate::{
    ast::{BindingMode, Crate, Mutability, expr::*, item::*, pat::*, stmt::*},
    impossible,
    ir::{globalxxx::FunctionPtr, ir_value::GlobalObjectPtr},
    irgen::{
        IRGenerator,
        extra::{ExprExtra, PatExtra},
        value::{ContainerKind, ValuePtrContainer},
    },
    semantics::{analyzer::SemanticAnalyzer, value::PlaceValueIndex, visitor::Visitor},
};

impl<'ast, 'analyzer> Visitor<'ast> for IRGenerator<'analyzer> {
    type DefaultRes<'res>
        = ()
    where
        Self: 'res;

    type ExprRes<'res>
        = Option<ValuePtrContainer>
    where
        Self: 'res;

    type PatRes<'res>
        = ()
    where
        Self: 'res;

    type StmtRes<'res>
        = Option<ValuePtrContainer>
    where
        Self: 'res;

    type CrateExtra<'tmp> = ();

    type ItemExtra<'tmp> = ExprExtra;

    type StmtExtra<'tmp> = ExprExtra;

    type ExprExtra<'tmp> = ExprExtra;

    type PatExtra<'tmp> = PatExtra;

    fn visit_crate<'tmp>(
        &mut self,
        Crate { items, id }: &'ast crate::ast::Crate,
        _extra: Self::CrateExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        for item in items {
            self.visit_item(
                item,
                ExprExtra {
                    scope_id: *id,
                    self_id: 0,
                },
            );
        }
    }

    fn visit_item<'tmp>(
        &mut self,
        Item { kind, id, span: _ }: &'ast Item,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        let new_extra = ExprExtra {
            self_id: *id,
            ..extra
        };

        match kind {
            ItemKind::Const(const_item) => self.visit_const_item(const_item, new_extra),
            ItemKind::Fn(fn_item) => self.visit_fn_item(fn_item, new_extra),
            ItemKind::Mod(mod_item) => self.visit_mod_item(mod_item, new_extra),
            ItemKind::Enum(enum_item) => self.visit_enum_item(enum_item, new_extra),
            ItemKind::Struct(struct_item) => self.visit_struct_item(struct_item, new_extra),
            ItemKind::Trait(trait_item) => self.visit_trait_item(trait_item, new_extra),
            ItemKind::Impl(impl_item) => self.visit_impl_item(impl_item, new_extra),
        }
    }

    fn visit_const_item<'tmp>(
        &mut self,
        _item: &'ast ConstItem,
        _extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
    }

    fn visit_fn_item<'tmp>(
        &mut self,
        FnItem {
            ident,
            generics: _,
            sig: FnSig { decl, span: _ },
            body,
        }: &'ast FnItem,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        let full_name = SemanticAnalyzer::get_full_name(extra.scope_id, ident.symbol.clone());
        let name_string = full_name.to_string();
        let fn_ptr = self.module.get_function(&name_string).unwrap();

        let bb = self.context.append_basic_block(&fn_ptr, "entry");
        let loc = self.builder.get_location();
        self.builder.locate(fn_ptr.clone(), bb);

        let (ret_type, arg_types) = self.functions.get(&name_string).unwrap().clone();

        let args = fn_ptr.as_function().args();

        zip(args, arg_types)
            .zip(&decl.inputs)
            .for_each(|((arg, ty), param)| {
                self.visit_pat(
                    &param.pat,
                    PatExtra {
                        value: ValuePtrContainer {
                            value_ptr: arg.clone().into(),
                            kind: if ty.is_aggregate_type() {
                                ContainerKind::Ptr(ty)
                            } else {
                                ContainerKind::Raw
                            },
                        },
                        self_id: 0,
                    },
                )
            });

        let value = self.visit_block_expr(
            body.as_ref().unwrap(),
            ExprExtra {
                scope_id: extra.self_id,
                self_id: 0,
            },
        );
        self.builder.build_return(if let Some(value) = value {
            Some(if ret_type.is_aggregate_type() {
                self.get_value_ptr(value).value_ptr
            } else {
                self.get_raw_value(value)
            })
        } else {
            None
        });

        self.builder.set_location(loc);
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
        ImplItem {
            generics,
            of_trait,
            self_ty,
            items,
        }: &'ast ImplItem,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        todo!()
    }

    fn visit_associate_item<'tmp>(
        &mut self,
        item: &'ast crate::ast::item::Item<crate::ast::item::AssocItemKind>,
        extra: Self::ItemExtra<'tmp>,
    ) -> Self::DefaultRes<'_> {
        todo!()
    }

    fn visit_stmt<'tmp>(
        &mut self,
        Stmt { kind, id, span: _ }: &'ast Stmt,
        extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        let new_extra = ExprExtra {
            self_id: *id,
            ..extra
        };
        match &kind {
            StmtKind::Let(local_stmt) => self.visit_local_stmt(local_stmt, new_extra),
            StmtKind::Item(item) => {
                self.visit_item(item, new_extra);
                None
            }
            StmtKind::Expr(expr) => self.visit_expr(expr, new_extra),
            StmtKind::Semi(expr) => {
                self.visit_expr(expr, new_extra);
                None
            }
            StmtKind::Empty(_) => None,
        }
    }

    fn visit_local_stmt<'tmp>(
        &mut self,
        LocalStmt {
            pat,
            ty: _,
            kind,
            id: _,
            span: _,
        }: &'ast LocalStmt,
        extra: Self::StmtExtra<'tmp>,
    ) -> Self::StmtRes<'_> {
        let value = match kind {
            LocalKind::Decl => impossible!(),
            LocalKind::Init(expr) => self.visit_expr(&expr, extra).unwrap(),
        };

        self.visit_pat(pat, PatExtra { value, self_id: 0 });

        None
    }

    fn visit_expr<'tmp>(
        &mut self,
        Expr { kind, span: _, id }: &'ast Expr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let new_extra = ExprExtra {
            self_id: *id,
            ..extra
        };
        match kind {
            ExprKind::Array(expr) => self.visit_array_expr(expr, new_extra),
            ExprKind::ConstBlock(expr) => self.visit_const_block_expr(expr, new_extra),
            ExprKind::Call(expr) => self.visit_call_expr(expr, new_extra),
            ExprKind::MethodCall(expr) => self.visit_method_call_expr(expr, new_extra),
            ExprKind::Tup(expr) => self.visit_tup_expr(expr, new_extra),
            ExprKind::Binary(expr) => self.visit_binary_expr(expr, new_extra),
            ExprKind::Unary(expr) => self.visit_unary_expr(expr, new_extra),
            ExprKind::Lit(expr) => self.visit_lit_expr(expr, new_extra),
            ExprKind::Cast(expr) => self.visit_cast_expr(expr, new_extra),
            ExprKind::Let(expr) => self.visit_let_expr(expr, new_extra),
            ExprKind::If(expr) => self.visit_if_expr(expr, new_extra),
            ExprKind::While(expr) => self.visit_while_expr(expr, new_extra),
            ExprKind::ForLoop(expr) => self.visit_for_loop_expr(expr, new_extra),
            ExprKind::Loop(expr) => self.visit_loop_expr(expr, new_extra),
            ExprKind::Match(expr) => self.visit_match_expr(expr, new_extra),
            ExprKind::Block(expr) => self.visit_block_expr(expr, new_extra),
            ExprKind::Assign(expr) => self.visit_assign_expr(expr, new_extra),
            ExprKind::AssignOp(expr) => self.visit_assign_op_expr(expr, new_extra),
            ExprKind::Field(expr) => self.visit_field_expr(expr, new_extra),
            ExprKind::Index(expr) => self.visit_index_expr(expr, new_extra),
            ExprKind::Range(expr) => self.visit_range_expr(expr, new_extra),
            ExprKind::Underscore(expr) => self.visit_underscore_expr(expr, new_extra),
            ExprKind::Path(expr) => self.visit_path_expr(expr, new_extra),
            ExprKind::AddrOf(expr) => self.visit_addr_of_expr(expr, new_extra),
            ExprKind::Break(expr) => self.visit_break_expr(expr, new_extra),
            ExprKind::Continue(expr) => self.visit_continue_expr(expr, new_extra),
            ExprKind::Ret(expr) => self.visit_ret_expr(expr, new_extra),
            ExprKind::Struct(expr) => self.visit_struct_expr(expr, new_extra),
            ExprKind::Repeat(expr) => self.visit_repeat_expr(expr, new_extra),
        }
    }

    fn visit_array_expr<'tmp>(
        &mut self,
        ArrayExpr(exprs): &'ast ArrayExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let intern = self.analyzer.get_expr_type(&extra.self_id);
        let ty = self.transform_interned_ty_faithfully(intern);

        let inner_ty = ty.as_array().unwrap().0.clone();
        let value = self.builder.build_alloca(ty.clone(), None);

        for (i, expr) in exprs.iter().enumerate() {
            let v = self.visit_expr(expr, extra)?;
            let ith = self.builder.build_getelementptr(
                inner_ty.clone(),
                value.clone().into(),
                vec![self.context.get_i32(i as u32).into()],
                None,
            );
            self.store_to_ptr(ith.into(), v);
        }

        Some(ValuePtrContainer {
            value_ptr: value.into(),
            kind: ContainerKind::Ptr(ty),
        })
    }

    fn visit_const_block_expr<'tmp>(
        &mut self,
        _expr: &'ast ConstBlockExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_call_expr<'tmp>(
        &mut self,
        CallExpr(fn_expr, args_expr): &'ast CallExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let fn_value = self.visit_expr(fn_expr, extra)?;
        debug_assert!(fn_value.value_ptr.is_function_type());
        let args = args_expr
            .iter()
            .map(|x| self.visit_expr(&x, extra))
            .collect::<Option<Vec<_>>>()?;

        let (ret_ty, arg_tys) = self
            .functions
            .get(fn_value.value_ptr.get_name().as_ref().unwrap())
            .unwrap();

        let ins = self.builder.build_call(
            FunctionPtr(GlobalObjectPtr(fn_value.value_ptr)),
            args.into_iter()
                .zip(arg_tys)
                .into_iter()
                .map(|(x, ty)| {
                    if ty.is_aggregate_type() {
                        self.get_value_ptr(x).value_ptr
                    } else {
                        self.get_raw_value(x)
                    }
                })
                .collect(),
            None,
        );

        Some(ValuePtrContainer {
            value_ptr: ins.into(),
            kind: if ret_ty.is_aggregate_type() {
                ContainerKind::Ptr(ret_ty.clone())
            } else {
                ContainerKind::Raw
            },
        })
    }

    fn visit_method_call_expr<'tmp>(
        &mut self,
        expr: &'ast MethodCallExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        todo!()
    }

    fn visit_tup_expr<'tmp>(
        &mut self,
        TupExpr(exprs, force): &'ast TupExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        match (&exprs[..], force) {
            ([], _) => None,
            ([expr], false) => self.visit_expr(expr, extra),
            _ => {
                let intern = self.analyzer.get_expr_type(&extra.self_id);
                let ty = self.transform_interned_ty_faithfully(intern);
                let p = self.builder.build_alloca(ty.clone(), None);
                for (i, expr) in exprs.iter().enumerate() {
                    let expr_value = self.visit_expr(&expr, extra)?;
                    let gep = self.builder.build_getelementptr(
                        ty.clone(),
                        p.clone().into(),
                        vec![
                            self.context.get_i32(0).into(),
                            self.context.get_i32(i as u32).into(),
                        ],
                        None,
                    );
                    self.store_to_ptr(gep.into(), expr_value);
                }

                Some(ValuePtrContainer {
                    value_ptr: p.into(),
                    kind: ContainerKind::Ptr(ty),
                })
            }
        }
    }

    fn visit_binary_expr<'tmp>(
        &mut self,
        BinaryExpr(bin_op, expr1, expr2): &'ast BinaryExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        match bin_op {
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Rem
            | BinOp::BitXor
            | BinOp::BitAnd
            | BinOp::BitOr
            | BinOp::Shl
            | BinOp::Shr => self.visit_binary(*bin_op, expr1, expr2, extra),

            BinOp::And | BinOp::Or => self.visit_logic(*bin_op, expr1, expr2, extra),

            BinOp::Eq | BinOp::Lt | BinOp::Le | BinOp::Ne | BinOp::Ge | BinOp::Gt => {
                self.visit_compare(*bin_op, expr1, expr2, extra)
            }
        }
    }

    fn visit_unary_expr<'tmp>(
        &mut self,
        expr: &'ast UnaryExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_lit_expr<'tmp>(
        &mut self,
        expr: &'ast LitExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_cast_expr<'tmp>(
        &mut self,
        expr: &'ast CastExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_let_expr<'tmp>(
        &mut self,
        expr: &'ast LetExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_if_expr<'tmp>(
        &mut self,
        expr: &'ast IfExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_while_expr<'tmp>(
        &mut self,
        expr: &'ast WhileExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_for_loop_expr<'tmp>(
        &mut self,
        expr: &'ast ForLoopExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_loop_expr<'tmp>(
        &mut self,
        expr: &'ast LoopExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_match_expr<'tmp>(
        &mut self,
        expr: &'ast MatchExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_block_expr<'tmp>(
        &mut self,
        BlockExpr { stmts, id, span: _ }: &'ast BlockExpr,
        _extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        let mut v = None;

        for stmt in stmts {
            let t = self.visit_stmt(
                stmt,
                ExprExtra {
                    scope_id: *id,
                    self_id: 0,
                },
            );
            if let Some(i) = t {
                v.insert(i);
            }
        }

        v
    }

    fn visit_assign_expr<'tmp>(
        &mut self,
        expr: &'ast AssignExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_assign_op_expr<'tmp>(
        &mut self,
        expr: &'ast AssignOpExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_field_expr<'tmp>(
        &mut self,
        expr: &'ast FieldExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_index_expr<'tmp>(
        &mut self,
        expr: &'ast IndexExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_range_expr<'tmp>(
        &mut self,
        expr: &'ast RangeExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_underscore_expr<'tmp>(
        &mut self,
        expr: &'ast UnderscoreExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_path_expr<'tmp>(
        &mut self,
        expr: &'ast PathExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_addr_of_expr<'tmp>(
        &mut self,
        expr: &'ast AddrOfExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_break_expr<'tmp>(
        &mut self,
        expr: &'ast BreakExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_continue_expr<'tmp>(
        &mut self,
        expr: &'ast ContinueExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_ret_expr<'tmp>(
        &mut self,
        expr: &'ast RetExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_struct_expr<'tmp>(
        &mut self,
        expr: &'ast StructExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_repeat_expr<'tmp>(
        &mut self,
        expr: &'ast RepeatExpr,
        extra: Self::ExprExtra<'tmp>,
    ) -> Self::ExprRes<'_> {
        impossible!()
    }

    fn visit_pat<'tmp>(
        &mut self,
        Pat { kind, id, span: _ }: &'ast Pat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        let new_extra = PatExtra {
            self_id: *id,
            ..extra
        };
        match kind {
            PatKind::Wild(pat) => self.visit_wild_pat(pat, new_extra),
            PatKind::Ident(pat) => self.visit_ident_pat(pat, new_extra),
            PatKind::Struct(pat) => self.visit_struct_pat(pat, new_extra),
            PatKind::Or(pat) => self.visit_or_pat(pat, new_extra),
            PatKind::Path(pat) => self.visit_path_pat(pat, new_extra),
            PatKind::Tuple(pat) => self.visit_tuple_pat(pat, new_extra),
            PatKind::Ref(pat) => self.visit_ref_pat(pat, new_extra),
            PatKind::Lit(pat) => self.visit_lit_pat(pat, new_extra),
            PatKind::Range(pat) => self.visit_range_pat(pat, new_extra),
            PatKind::Slice(pat) => self.visit_slice_pat(pat, new_extra),
            PatKind::Rest(pat) => self.visit_rest_pat(pat, new_extra),
        }
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
        IdentPat(mode, ident, _restriction): &'ast IdentPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        self.visit_ident_pat_impl(mode, ident, extra)
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
        PathPat(_, path): &'ast PathPat,
        extra: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        let ident = path.get_ident();
        let mode = BindingMode(crate::ast::ByRef::No, Mutability::Not);
        self.visit_ident_pat_impl(&mode, ident, extra)
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
        RefPat(pat, _): &'ast RefPat,
        PatExtra { value, self_id: _ }: Self::PatExtra<'tmp>,
    ) -> Self::PatRes<'_> {
        let new_value = self.get_value_ptr(value);
        self.visit_pat(
            pat,
            PatExtra {
                value: new_value,
                self_id: 0,
            },
        )
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
