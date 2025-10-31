pub mod expr;
pub mod extra;
pub mod pat;
pub mod ty;
pub mod value;
pub mod visitor;

use std::collections::HashMap;

use crate::{
    ast::{Crate, NodeId},
    impossible,
    ir::{LLVMBuilder, LLVMContext, LLVMModule, attribute::Attribute, ir_value::ConstantPtr},
    irgen::value::ValuePtrContainer,
    semantics::{
        analyzer::SemanticAnalyzer,
        resolved_ty::{ResolvedTy, ResolvedTyKind, TypeKey},
        utils::FullName,
        value::{PlaceValue, PlaceValueIndex, ValueIndex},
        visitor::Visitor,
    },
};

pub struct IRGenerator<'analyzer> {
    pub(crate) context: LLVMContext,
    pub(crate) builder: LLVMBuilder,
    pub(crate) module: LLVMModule,

    pub(crate) analyzer: &'analyzer SemanticAnalyzer,

    pub(crate) value_indexes: HashMap<ValueIndex, ValuePtrContainer>,
}

impl<'analyzer> IRGenerator<'analyzer> {
    pub fn new(analyzer: &'analyzer SemanticAnalyzer) -> Self {
        let mut context = LLVMContext::default();
        let mut builder = context.create_builder();
        let mut module = context.create_module("crate");
        let mut value_indexes = HashMap::default();

        add_preludes(&context, &mut builder, &mut module, &mut value_indexes);

        let mut generator = Self {
            context,
            builder,
            module,
            analyzer,
            value_indexes,
        };

        generator.add_struct_type();
        generator.absorb_analyzer_global_values(0);
        generator.absorb_analyzer_methods();

        generator
    }

    pub fn visit(&mut self, krate: &Crate) {
        self.visit_crate(krate, ());
    }

    pub fn print(&self) -> String {
        self.module.print()
    }

    fn absorb_analyzer_struct(&self, structs: &mut HashMap<TypeKey, ResolvedTy>) {
        self.absorb_scope_struct(0, structs);
    }

    fn absorb_scope_struct(&self, scope_id: NodeId, structs: &mut HashMap<TypeKey, ResolvedTy>) {
        let scope = self.analyzer.get_scope(scope_id);
        for key in scope.types.values() {
            let ty = self.analyzer.probe_type((*key).into()).unwrap();

            use crate::semantics::resolved_ty::ResolvedTyKind::*;
            if let Tup(_) = ty.kind {
                structs.insert(*key, ty);
            }
        }

        for child in &scope.children {
            self.absorb_scope_struct(*child, structs);
        }
    }

    fn add_struct_type(&mut self) {
        let mut map = HashMap::new();
        self.absorb_analyzer_struct(&mut map);

        for ty in map.values() {
            self.context
                .create_opaque_struct_type(&ty.names.as_ref().unwrap().0.to_string());
        }

        for ty in map.values() {
            let struct_ty = self
                .context
                .get_named_struct_type(&ty.names.as_ref().unwrap().0.to_string())
                .unwrap();
            struct_ty.set_body(
                ty.kind
                    .as_tup()
                    .unwrap()
                    .iter()
                    .map(|x| self.transform_interned_ty_faithfully(*x))
                    .collect(),
                false,
            );
        }
    }

    fn absorb_analyzer_global_values(&mut self, scope_id: NodeId) {
        let scope = self.analyzer.get_scope(scope_id);
        for (s, PlaceValue { value, .. }) in &scope.values {
            if s.0 == "exit" && scope_id == 0 {
                continue;
            }
            let is_main_function = self.analyzer.is_main_function(s, scope_id);
            let full_name = self.analyzer.get_full_name(scope_id, s.clone());

            let v = self.absorb_analyzer_global_value(value, is_main_function, full_name);
            self.value_indexes.insert(
                ValueIndex::Place(PlaceValueIndex {
                    name: s.clone(),
                    kind: crate::semantics::value::ValueIndexKind::Global { scope_id },
                }),
                v,
            );
        }

        for id in &scope.children {
            self.absorb_analyzer_global_values(*id);
        }
    }

    fn absorb_analyzer_global_value(
        &mut self,
        value: &crate::semantics::value::Value,
        is_main_function: bool,
        full_name: FullName,
    ) -> ValuePtrContainer {
        use crate::semantics::value::ValueKind::*;

        match &value.kind {
            Constant(inner) => {
                let probe = self.analyzer.probe_type(value.ty).unwrap();
                use crate::semantics::value::ConstantValue::*;
                let init: ConstantPtr = match inner {
                    ConstantInt(i) => {
                        use crate::semantics::resolved_ty::BuiltInTyKind::*;

                        match probe.kind {
                            ResolvedTyKind::BuiltIn(builtin) => match builtin {
                                Bool => self.context.get_i1(*i != 0),
                                Char => self.context.get_i8(*i as u8),
                                I32 | ISize | U32 | USize => self.context.get_i32(*i),
                                Str => impossible!(),
                            }
                            .into(),
                            ResolvedTyKind::Enum => self.context.get_i32(*i).into(),
                            _ => impossible!(),
                        }
                    }
                    ConstantString(string) => self.context.get_string(string).into(),
                    ConstantArray(..) => todo!(),
                    Unit | UnitStruct => self
                        .context
                        .get_struct(self.context.struct_type(vec![], false), vec![])
                        .into(),
                    UnEval(_) | Placeholder => impossible!(),
                };
                let ty = init.get_type().clone();
                let var_ptr = self
                    .module
                    .add_global_variable(true, init, &full_name.to_string());

                ValuePtrContainer {
                    value_ptr: var_ptr.into(),
                    kind: value::ContainerKind::Ptr(ty),
                }
            }
            Fn { .. } => {
                let mut fn_resolved_ty = self.analyzer.probe_type(value.ty).unwrap();

                if is_main_function {
                    *fn_resolved_ty.kind.as_fn_mut().unwrap().0 = self.analyzer.i32_type();
                }

                let (ret_intern, arg_interns) = fn_resolved_ty.kind.as_fn_mut().unwrap();

                let mut ret_ty = self.transform_interned_ty_faithfully(*ret_intern);
                let mut arg_tys = Vec::new();

                let i32_type = self.context.i32_type();
                let ptr_type = self.context.ptr_type();

                for arg_intern in arg_interns {
                    let arg_ty = self.transform_interned_ty_impl(
                        *arg_intern,
                        ty::TransformTypeConfig::FirstClass,
                    );
                    if let Some(s) = arg_ty.as_struct()
                        && let Some(name) = s.get_name()
                        && name == "fat_ptr"
                    {
                        arg_tys.push(ptr_type.clone().into());
                        arg_tys.push(i32_type.clone().into());
                    } else {
                        arg_tys.push(arg_ty);
                    }
                }

                let mut aggregate_type = None;
                if ret_ty.is_zero_length_type() {
                    ret_ty = self.context.void_type().into();
                } else if ret_ty.is_aggregate_type() {
                    arg_tys.insert(0, self.context.ptr_type().into());
                    aggregate_type = Some(ret_ty);
                    ret_ty = self.context.void_type().into();
                }

                let fn_ty = self.context.function_type(ret_ty.clone(), arg_tys);
                let fn_ptr = self
                    .module
                    .add_function(fn_ty.clone(), &full_name.to_string(), None);
                if let Some(aggregate_type) = aggregate_type {
                    fn_ptr
                        .as_function()
                        .add_param_attr(0, Attribute::StructReturn(aggregate_type));
                }

                ValuePtrContainer {
                    value_ptr: fn_ptr.into(),
                    kind: value::ContainerKind::Ptr(fn_ty.into()),
                }
            }
            _ => impossible!(),
        }
    }

    fn absorb_analyzer_methods(&mut self) {
        for (resolved_ty, impls) in &self.analyzer.impls {
            if resolved_ty.kind.is_trait() {
                continue;
            }
            let Some((name, _)) = &resolved_ty.names else {
                continue;
            };
            for (s, PlaceValue { value, .. }) in &impls.inherent.values {
                let v =
                    self.absorb_analyzer_global_value(value, false, name.clone().concat(s.clone()));
                self.value_indexes.insert(
                    ValueIndex::Place(PlaceValueIndex {
                        name: s.clone(),
                        kind: crate::semantics::value::ValueIndexKind::Impl {
                            ty: resolved_ty.clone(),
                            for_trait: None,
                        },
                    }),
                    v,
                );
            }
        }
    }
}

fn add_preludes(
    context: &LLVMContext,
    builder: &mut LLVMBuilder,
    module: &mut LLVMModule,
    value_indexes: &mut HashMap<ValueIndex, ValuePtrContainer>,
) {
    let string_type = context.create_opaque_struct_type("String");
    string_type.set_body(
        vec![context.ptr_type().into(), context.i32_type().into()],
        false,
    );
    let fat_ptr_type = context.create_opaque_struct_type("fat_ptr");
    fat_ptr_type.set_body(
        vec![context.ptr_type().into(), context.i32_type().into()],
        false,
    );
    let str_len_type = context.function_type(
        context.i32_type().into(),
        vec![context.ptr_type().into(), context.i32_type().into()],
    );
    let str_len_fn = module.add_function(str_len_type.clone(), "str.len", None);
    let bb = context.append_basic_block(&str_len_fn, "entry");
    builder.locate(str_len_fn.clone(), bb);
    builder.build_return(Some(
        str_len_fn
            .as_function()
            .get_nth_argument(1)
            .unwrap()
            .clone()
            .into(),
    ));
    let i32_to_string_type = context.function_type(
        context.void_type().into(),
        vec![context.ptr_type().into(), context.ptr_type().into()],
    );
    let u_to_string_fn = module.add_function(i32_to_string_type.clone(), "to_string", None);
    u_to_string_fn
        .as_function()
        .add_param_attr(0, Attribute::StructReturn(string_type.clone().into()));

    value_indexes.insert(
        ValueIndex::Place(PlaceValueIndex {
            name: "len".into(),
            kind: crate::semantics::value::ValueIndexKind::Impl {
                ty: ResolvedTy {
                    names: None,
                    kind: crate::semantics::resolved_ty::ResolvedTyKind::BuiltIn(
                        crate::semantics::resolved_ty::BuiltInTyKind::Str,
                    ),
                },
                for_trait: None,
            },
        }),
        ValuePtrContainer {
            value_ptr: str_len_fn.into(),
            kind: value::ContainerKind::Ptr(str_len_type.into()),
        },
    );

    value_indexes.insert(
        ValueIndex::Place(PlaceValueIndex {
            name: "to_string".into(),
            kind: crate::semantics::value::ValueIndexKind::Impl {
                ty: ResolvedTy {
                    names: None,
                    kind: crate::semantics::resolved_ty::ResolvedTyKind::BuiltIn(
                        crate::semantics::resolved_ty::BuiltInTyKind::U32,
                    ),
                },
                for_trait: None,
            },
        }),
        ValuePtrContainer {
            value_ptr: u_to_string_fn.clone().into(),
            kind: value::ContainerKind::Ptr(i32_to_string_type.clone().into()),
        },
    );

    value_indexes.insert(
        ValueIndex::Place(PlaceValueIndex {
            name: "to_string".into(),
            kind: crate::semantics::value::ValueIndexKind::Impl {
                ty: ResolvedTy {
                    names: None,
                    kind: crate::semantics::resolved_ty::ResolvedTyKind::BuiltIn(
                        crate::semantics::resolved_ty::BuiltInTyKind::USize,
                    ),
                },
                for_trait: None,
            },
        }),
        ValuePtrContainer {
            value_ptr: u_to_string_fn.clone().into(),
            kind: value::ContainerKind::Ptr(i32_to_string_type.clone().into()),
        },
    );
}
