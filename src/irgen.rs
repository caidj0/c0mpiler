pub mod expr;
pub mod extra;
pub mod pat;
pub mod ty;
pub mod value;
pub mod visitor;

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Crate, NodeId},
    impossible,
    ir::{
        LLVMBuilder, LLVMContext, LLVMModule,
        ir_type::{FunctionTypePtr, TypePtr},
        ir_value::{ConstantPtr, ValuePtr},
    },
    irgen::value::ValuePtrContainer,
    semantics::{
        analyzer::SemanticAnalyzer,
        resolved_ty::{ResolvedTy, TypeKey},
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

    pub(crate) functions: HashMap<String, (TypePtr, Vec<TypePtr>)>,
}

impl<'analyzer> IRGenerator<'analyzer> {
    pub fn new(analyzer: &'analyzer SemanticAnalyzer) -> Self {
        let mut context = LLVMContext::default();
        let builder = context.create_builder();
        let module = context.create_module("crate");

        let string_type = context.create_opaque_struct_type("String");
        string_type.set_body(
            vec![context.i32_type().into(), context.ptr_type().into()],
            false,
        );

        let mut generator = Self {
            context,
            builder,
            module,
            analyzer,
            value_indexes: HashMap::default(),
            functions: HashMap::default(),
        };

        generator.add_struct_type();
        generator.absorb_analyzer_global_values(0);

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
        for (_, key) in &scope.types {
            let ty = self.analyzer.probe_type((*key).into()).unwrap();

            use crate::semantics::resolved_ty::ResolvedTyKind::*;
            match ty.kind {
                Tup(_) => {
                    structs.insert(*key, ty);
                }
                _ => {}
            }
        }

        for child in &scope.children {
            self.absorb_scope_struct(*child, structs);
        }
    }

    fn add_struct_type(&mut self) {
        let mut map = HashMap::new();
        self.absorb_analyzer_struct(&mut map);

        for (_, ty) in &map {
            self.context
                .create_opaque_struct_type(&ty.names.as_ref().unwrap().0.to_string());
        }

        for (_, ty) in &map {
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
            let full_name = self.analyzer.get_full_name(scope_id, s.clone());

            use crate::semantics::value::ValueKind::*;
            let v = match &value.kind {
                Constant(inner) => {
                    let probe = self.analyzer.probe_type(value.ty).unwrap();
                    use crate::semantics::value::ConstantValue::*;
                    let init: ConstantPtr = match inner {
                        ConstantInt(i) => {
                            let builtin = probe.kind.as_built_in().unwrap();
                            use crate::semantics::resolved_ty::BuiltInTyKind::*;
                            match builtin {
                                Bool => self.context.get_i1(*i != 0),
                                Char => self.context.get_i8(*i as u8),
                                I32 | ISize | U32 | USize => self.context.get_i32(*i as u32),
                                Str => impossible!(),
                            }
                            .into()
                        }
                        ConstantString(string) => self.context.get_string(string).into(),
                        ConstantArray(..) => todo!(),
                        Unit | UnitStruct => self
                            .context
                            .get_struct(self.context.struct_type(vec![], false), vec![])
                            .into(),
                        UnEval(_) | Placeholder => impossible!(),
                    };
                    let var_ptr =
                        self.module
                            .add_global_variable(true, init.into(), &full_name.to_string());
                    ValuePtrContainer {
                        value_ptr: var_ptr.into(),
                        kind: value::ContainerKind::Raw,
                    }
                }
                Fn { .. } => {
                    let mut fn_resloved_ty = self.analyzer.probe_type(value.ty).unwrap();

                    if self.analyzer.is_main_function(s, scope_id) {
                        *fn_resloved_ty.kind.as_fn_mut().unwrap().0 = self.analyzer.i32_type();
                    }

                    let f = {
                        let (r, a) = fn_resloved_ty.kind.as_fn().unwrap();
                        (
                            self.transform_interned_ty_faithfully(*r),
                            a.iter()
                                .map(|x| self.transform_interned_ty_faithfully(*x))
                                .collect(),
                        )
                    };
                    let fn_ty = self.transform_ty_faithfully(&fn_resloved_ty);
                    let fn_ptr =
                        self.module
                            .add_function(fn_ty.into(), &full_name.to_string(), None);
                    self.functions.insert(full_name.to_string(), f);
                    ValuePtrContainer {
                        value_ptr: fn_ptr.into(),
                        kind: value::ContainerKind::Raw,
                    }
                }
                _ => impossible!(),
            };
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
}
