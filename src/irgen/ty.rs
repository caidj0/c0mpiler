use crate::{
    impossible,
    ir::ir_type::TypePtr,
    irgen::IRGenerator,
    semantics::resolved_ty::{AnyTyKind, BuiltInTyKind, ResolvedTy, ResolvedTyKind, TypeIntern},
};

/*
    转换方式：
    1. Faithful：除了 never type 转换为 void type 外，其他原样转换
        复合类型（tup, array）内部始终使用 Faithful 转换
    2. FirstClass: never type -> void type, 简单类型原样转换，复合类型 -> ptr type
        函数参数使用 FirstClass 转换
    2. FirstClassNoUnit：never type, unit type -> void type，简单类型原样转换，复合类型 -> ptr type
        函数返回值使用 FirstClassNoUnit 转换
*/
pub(crate) enum TransfromTypeConfig {
    Faithful,
    FirstClass,
    FirstClassNoUnit,
}

impl TransfromTypeConfig {
    fn no_aggregate_type(&self) -> bool {
        matches!(self, Self::FirstClass | Self::FirstClassNoUnit)
    }

    fn no_unit(&self) -> bool {
        matches!(self, Self::FirstClassNoUnit)
    }
}

impl<'analyzer> IRGenerator<'analyzer> {
    pub(crate) fn is_aggregate_type(&self, x: &ResolvedTy) -> bool {
        matches!(
            x.kind,
            ResolvedTyKind::Array(..) | ResolvedTyKind::Fn(..) | ResolvedTyKind::Tup(..)
        )
    }

    pub(crate) fn wraped_ptr_type(&self) -> TypePtr {
        self.context
            .struct_type(
                vec![
                    self.context.i32_type().into(),
                    self.context.ptr_type().into(),
                ],
                false,
            )
            .into()
    }

    pub(crate) fn transform_interned_ty_faithfully(&self, intern: TypeIntern) -> TypePtr {
        self.transform_interned_ty_impl(intern, TransfromTypeConfig::Faithful)
    }

    pub(crate) fn transform_interned_ty_for_reg(&self, intern: TypeIntern) -> TypePtr {
        self.transform_interned_ty_impl(intern, TransfromTypeConfig::FirstClass)
    }

    pub(crate) fn transform_ty_faithfully(&self, ty: &ResolvedTy) -> TypePtr {
        self.transform_ty_impl(ty, TransfromTypeConfig::Faithful)
    }

    pub(crate) fn transform_interned_ty_impl(
        &self,
        intern: TypeIntern,
        cfg: TransfromTypeConfig,
    ) -> TypePtr {
        let Some(ty) = self.analyzer.probe_type(intern) else {
            return self.context.void_type().into();
        };
        self.transform_ty_impl(&ty, cfg)
    }

    // 空长度类型怎么办？
    pub(crate) fn transform_ty_impl(&self, ty: &ResolvedTy, cfg: TransfromTypeConfig) -> TypePtr {
        if cfg.no_aggregate_type() && self.is_aggregate_type(ty) {
            return self.context.ptr_type().into();
        }
        use crate::semantics::resolved_ty::ResolvedTyKind::*;
        match &ty.kind {
            Placeholder => todo!(),
            BuiltIn(built_in_ty_kind) => match built_in_ty_kind {
                BuiltInTyKind::Bool => self.context.i1_type().into(),
                BuiltInTyKind::Char => self.context.i8_type().into(),
                BuiltInTyKind::I32
                | BuiltInTyKind::ISize
                | BuiltInTyKind::U32
                | BuiltInTyKind::USize => self.context.i32_type().into(),
                BuiltInTyKind::Str => impossible!(),
            },
            Ref(inner, _) => {
                let inner_ty = self.analyzer.probe_type(*inner).unwrap();
                if inner_ty.is_str_type() {
                    self.wraped_ptr_type()
                } else {
                    self.context.ptr_type().into()
                }
            }
            Tup(items) => {
                if let Some((name, _)) = &ty.names {
                    self.context
                        .get_named_struct_type(&name.to_string())
                        .unwrap()
                        .into()
                } else {
                    if items.len() == 0 && cfg.no_unit() {
                        self.context.void_type().into()
                    } else {
                        self.context
                            .struct_type(
                                items
                                    .iter()
                                    .map(|x| {
                                        self.transform_interned_ty_impl(
                                            *x,
                                            TransfromTypeConfig::Faithful,
                                        )
                                    })
                                    .collect(),
                                false,
                            )
                            .into()
                    }
                }
            }
            Enum => self.context.i32_type().into(),
            Trait => impossible!(),
            Array(inner, len) => self
                .context
                .array_type(
                    self.transform_interned_ty_impl(*inner, TransfromTypeConfig::Faithful),
                    len.unwrap(),
                )
                .into(),
            Fn(ret_ty, items) => self
                .context
                .function_type(
                    self.transform_interned_ty_impl(*ret_ty, TransfromTypeConfig::FirstClassNoUnit),
                    items
                        .iter()
                        .map(|x| {
                            self.transform_interned_ty_impl(*x, TransfromTypeConfig::FirstClass)
                        })
                        .collect(),
                )
                .into(),
            Any(AnyTyKind::Any) => impossible!(),
            Any(AnyTyKind::AnyInt) | Any(AnyTyKind::AnySignedInt) => self.context.i32_type().into(),
            ImplicitSelf(_) => impossible!(),
        }
    }
}
