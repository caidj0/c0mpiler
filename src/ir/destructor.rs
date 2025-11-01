use std::collections::HashMap;

use crate::ast::{Symbol, expr::*};
use crate::irgen::extra::ExprExtra;
use crate::irgen::value::{ContainerKind, ValuePtrContainer};
use crate::{irgen::IRGenerator, semantics::visitor::Visitor};

impl<'ast, 'analyzer> IRGenerator<'ast, 'analyzer> {
    pub(crate) fn destructing_assign<'tmp>(
        &mut self,
    expr: &'ast Expr,
        extra: ExprExtra<'tmp>,
        right_value: ValuePtrContainer,
    ) -> Option<()> {
        let right_value = self.get_value_presentation(right_value);
        match &expr.kind {
            ExprKind::Array(ArrayExpr(exprs)) => {
                let inner_ty = right_value
                    .kind
                    .as_ptr()
                    .unwrap()
                    .as_array()
                    .unwrap()
                    .0
                    .clone();
                exprs
                    .iter()
                    .enumerate()
                    .map(|(i, expr)| {
                        let v = self.builder.build_getelementptr(
                            inner_ty.clone(),
                            right_value.value_ptr.clone(),
                            vec![self.context.get_i32(i as u32).into()],
                            None,
                        );
                        self.destructing_assign(
                            expr,
                            extra,
                            ValuePtrContainer {
                                value_ptr: v.into(),
                                kind: ContainerKind::Ptr(inner_ty.clone()),
                            },
                        )
                    })
                    .collect::<Option<()>>()
            }
            ExprKind::Tup(TupExpr(exprs, force)) => match (&exprs[..], force) {
                ([], false) => Some(()),
                ([expr], false) => self.destructing_assign(expr, extra, right_value),
                _ => {
                    let struct_type = right_value.kind.as_ptr().unwrap();
                    let inner_tys = struct_type.as_struct().unwrap().get_body().unwrap();
                    exprs
                        .iter()
                        .zip(inner_tys)
                        .enumerate()
                        .map(|(i, (expr, inner_ty))| {
                            let v = self.builder.build_getelementptr(
                                struct_type.clone(),
                                right_value.value_ptr.clone(),
                                vec![
                                    self.context.get_i32(0).into(),
                                    self.context.get_i32(i as u32).into(),
                                ],
                                None,
                            );
                            self.destructing_assign(
                                expr,
                                extra,
                                ValuePtrContainer {
                                    value_ptr: v.into(),
                                    kind: ContainerKind::Ptr(inner_ty.clone()),
                                },
                            )
                        })
                        .collect::<Option<()>>()
                }
            },
            ExprKind::Underscore(_) => Some(()),
            ExprKind::Struct(StructExpr { fields, .. }) => {
                let intern = self.analyzer.get_expr_type(&expr.id);
                let probe = self.analyzer.probe_type(intern).unwrap();
                let field_names = probe.names.unwrap().1.unwrap();
                let order: HashMap<Symbol, usize> =
                    HashMap::from_iter(field_names.into_iter().enumerate().map(|(i, s)| (s, i)));

                let struct_type = right_value.kind.as_ptr().unwrap();
                let inner_tys = struct_type.as_struct().unwrap().get_body().unwrap();

                for ExprField { ident, expr, .. } in fields {
                    let index = order.get(&ident.symbol).unwrap();
                    let v = self.builder.build_getelementptr(
                        struct_type.clone(),
                        right_value.value_ptr.clone(),
                        vec![
                            self.context.get_i32(0).into(),
                            self.context.get_i32(*index as u32).into(),
                        ],
                        None,
                    );
                    self.destructing_assign(
                        expr,
                        extra,
                        ValuePtrContainer {
                            value_ptr: v.into(),
                            kind: ContainerKind::Ptr(inner_tys.get(*index).unwrap().clone()),
                        },
                    )?;
                }

                Some(())
            }
            _ => {
                let expr_value = self.visit_expr(expr, extra)?;
                let ptr = self.get_value_ptr(expr_value);
                self.store_to_ptr(ptr.value_ptr, right_value);
                Some(())
            }
        }
    }
}
