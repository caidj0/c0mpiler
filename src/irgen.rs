pub mod extra;
pub mod pat;
pub mod ty;
pub mod value;
pub mod visitor;
pub mod expr;

use std::collections::{HashMap, HashSet};

use crate::{
    ast::NodeId,
    impossible,
    ir::{
        LLVMBuilder, LLVMContext, LLVMModule,
        ir_type::{FunctionTypePtr, TypePtr},
        ir_value::ValuePtr,
    },
    irgen::value::ValuePtrContainer,
    semantics::{
        analyzer::SemanticAnalyzer,
        resolved_ty::{ResolvedTy, TypeKey},
        utils::FullName,
        value::{PlaceValue, ValueIndex},
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

        let mut generator = Self {
            context,
            builder,
            module,
            analyzer,
            value_indexes: HashMap::default(),
            functions: HashMap::default(),
        };

        generator.add_struct_type();
        generator.absorb_analyzer_function(0);

        generator
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

    fn absorb_analyzer_function(&mut self, scope_id: NodeId) {
        let scope = self.analyzer.get_scope(scope_id);
        for (s, PlaceValue { value, .. }) in &scope.values {
            use crate::semantics::value::ValueKind::*;
            match &value.kind {
                Constant(..) => {}
                Fn { .. } => {
                    let fn_resloved_ty = self.analyzer.probe_type(value.ty).unwrap();
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
                    let full_name = SemanticAnalyzer::get_full_name(scope_id, s.clone());
                    let _ = self
                        .module
                        .add_function(fn_ty.into(), &full_name.to_string(), None);
                    self.functions.insert(full_name.to_string(), f);
                }
                _ => impossible!(),
            }
        }

        for id in &scope.children {
            self.absorb_analyzer_function(*id);
        }
    }
}
