use crate::{
    ast::expr::{BinOp, Expr},
    impossible,
    ir::ir_value::{BinaryOpcode, ICmpCode},
    irgen::{
        IRGenerator,
        extra::ExprExtra,
        value::{ContainerKind, ValuePtrContainer},
    },
    semantics::visitor::Visitor,
};

impl<'analyzer> IRGenerator<'analyzer> {
    pub(crate) fn visit_binary(
        &mut self,
        bin_op: BinOp,
        expr1: &Expr,
        expr2: &Expr,
        extra: ExprExtra,
    ) -> Option<ValuePtrContainer> {
        let value1 = self.visit_expr(expr1, extra)?;
        let value2 = self.visit_expr(expr2, extra)?;
        let raw1 = self.get_raw_value(value1);
        let raw2 = self.get_raw_value(value2);

        let intern = self.analyzer.get_expr_type(&extra.self_id);

        Some(self.visit_binary_impl(bin_op, raw1, raw2, intern))
    }

    pub(crate) fn visit_binary_impl(
        &mut self,
        bin_op: BinOp,
        raw1: std::rc::Rc<crate::ir::ir_value::Value>,
        raw2: std::rc::Rc<crate::ir::ir_value::Value>,
        intern: crate::semantics::resolved_ty::TypeIntern,
    ) -> ValuePtrContainer {
        let resolved_ty = self.analyzer.probe_type(intern).unwrap();
        let is_signed = resolved_ty.is_signed_integer();
        let ty = self.transform_ty_faithfully(&resolved_ty);

        let op_code = match bin_op {
            BinOp::Add => BinaryOpcode::Add,
            BinOp::Sub => BinaryOpcode::Sub,
            BinOp::Mul => BinaryOpcode::Mul,
            BinOp::Div => {
                if is_signed {
                    BinaryOpcode::SDiv
                } else {
                    BinaryOpcode::UDiv
                }
            }
            BinOp::Rem => {
                if is_signed {
                    BinaryOpcode::SRem
                } else {
                    BinaryOpcode::URem
                }
            }
            BinOp::BitXor => BinaryOpcode::Xor,
            BinOp::BitAnd => BinaryOpcode::And,
            BinOp::BitOr => BinaryOpcode::Or,
            BinOp::Shl => BinaryOpcode::Shl,
            BinOp::Shr => {
                if is_signed {
                    BinaryOpcode::AShr
                } else {
                    BinaryOpcode::LShr
                }
            }
            _ => impossible!(),
        };

        let value = self.builder.build_binary(op_code, ty, raw1, raw2, None);

        ValuePtrContainer {
            value_ptr: value.into(),
            kind: ContainerKind::Raw,
        }
    }

    pub(crate) fn visit_compare(
        &mut self,
        bin_op: BinOp,
        expr1: &Expr,
        expr2: &Expr,
        extra: ExprExtra,
    ) -> Option<ValuePtrContainer> {
        let value1 = self.visit_expr(expr1, extra)?;
        let value2 = self.visit_expr(expr2, extra)?;

        let intern1 = self.analyzer.get_expr_type(&expr1.id);
        let resolved_ty1 = self.analyzer.probe_type(intern1).unwrap();
        let is_signed = resolved_ty1.is_signed_integer();

        let op_code = match bin_op {
            BinOp::Eq => ICmpCode::Eq,
            BinOp::Lt => {
                if is_signed {
                    ICmpCode::Sle
                } else {
                    ICmpCode::Ult
                }
            }
            BinOp::Le => {
                if is_signed {
                    ICmpCode::Sle
                } else {
                    ICmpCode::Ule
                }
            }
            BinOp::Ne => ICmpCode::Ne,
            BinOp::Ge => {
                if is_signed {
                    ICmpCode::Sge
                } else {
                    ICmpCode::Uge
                }
            }
            BinOp::Gt => {
                if is_signed {
                    ICmpCode::Sgt
                } else {
                    ICmpCode::Ugt
                }
            }
            _ => impossible!(),
        };

        let value = self.builder.build_icmp(
            op_code,
            self.get_raw_value(value1),
            self.get_raw_value(value2),
            None,
        );

        Some(ValuePtrContainer {
            value_ptr: value.into(),
            kind: ContainerKind::Raw,
        })
    }

    pub(crate) fn visit_logic(
        &mut self,
        bin_op: BinOp,
        expr1: &Expr,
        expr2: &Expr,
        extra: ExprExtra,
    ) -> Option<ValuePtrContainer> {
        let value1 = self.visit_expr(expr1, extra)?;

        let current_fn = self.builder.get_current_function().clone();
        let current_bb = self.builder.get_current_basic_block().clone();
        let right_bb = self.context.append_basic_block(&current_fn, ".right");
        let next_bb = self.context.append_basic_block(&current_fn, ".next");

        let raw1 = self.get_raw_value(value1);

        match bin_op {
            BinOp::And => self.builder.build_conditional_branch(
                raw1.clone(),
                right_bb.clone(),
                next_bb.clone(),
            ),
            BinOp::Or => self.builder.build_conditional_branch(
                raw1.clone(),
                next_bb.clone(),
                right_bb.clone(),
            ),
            _ => impossible!(),
        };

        self.builder.locate(current_fn.clone(), right_bb.clone());
        let value2 = self.visit_expr(expr2, extra)?;
        let raw2 = self.get_raw_value(value2);
        self.builder.build_branch(next_bb.clone());

        self.builder.locate(current_fn.clone(), next_bb.clone());
        let value = self.builder.build_phi(
            self.context.i1_type().into(),
            vec![(raw1, current_bb), (raw2, right_bb)],
            None,
        );

        Some(ValuePtrContainer {
            value_ptr: value.into(),
            kind: ContainerKind::Raw,
        })
    }

    pub(crate) fn visit_ret_expr_impl(&mut self, inner_expr: Option<&Expr>, extra: ExprExtra) {
        let v = if let Some(e) = inner_expr {
            self.visit_expr(e, extra)
        } else {
            None
        };

        self.builder
            .build_return(v.map(|x| self.get_value_presentation(x).value_ptr));
    }
}
