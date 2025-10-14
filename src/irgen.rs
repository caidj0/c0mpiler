use std::collections::HashMap;

use crate::{
    ast::NodeId,
    ir::{
        LLVMBuilder, LLVMContext, LLVMModule,
        ir_type::{FunctionTypePtr, StructTypePtr, TypePtr},
    },
    semantics::{SemanticAnalyzer, resolved_ty::ResolvedTy, utils::FullName},
};

#[allow(dead_code)]
#[derive(Debug)]
pub struct IRGenHelper {
    pub(crate) context: LLVMContext,
    pub(crate) builder: LLVMBuilder,
    pub(crate) module: LLVMModule,

    structs: HashMap<FullName, StructTypePtr>,
}

impl Default for IRGenHelper {
    fn default() -> Self {
        Self::new()
    }
}

#[allow(dead_code)]
impl IRGenHelper {
    pub fn new() -> Self {
        let mut context = LLVMContext::default();
        let builder = context.create_builder();
        let module = context.create_module("crate");

        Self {
            context,
            builder,
            module,

            structs: Default::default(),
        }
    }

    pub(crate) fn resolved_ty_to_ir_type(
        &self,
        analyzer: &SemanticAnalyzer,
        ty: &ResolvedTy,
        from: Option<NodeId>,
    ) -> TypePtr {
        match ty {
            ResolvedTy::BuiltIn(symbol, _) => match symbol.0.as_str() {
                "bool" => self.context.i1_type().into(),
                "u32" | "i32" | "usize" | "isize" => self.context.i32_type().into(),
                "char" => self.context.i8_type().into(),
                "str" => todo!(),
                "String" => todo!(),
                _ => panic!("Impossible!"),
            },
            ResolvedTy::Named(full_name) => {
                let info = analyzer.get_type_info(full_name);
                match &info.kind {
                    crate::semantics::utils::TypeKind::Placeholder => panic!("Impossible!"),
                    crate::semantics::utils::TypeKind::Struct { .. } => {
                        self.structs.get(full_name).unwrap().clone().into()
                    }
                    crate::semantics::utils::TypeKind::Enum { .. } => {
                        self.context.i32_type().into()
                    }
                    crate::semantics::utils::TypeKind::Trait { .. } => {
                        panic!("Impossible!")
                    }
                }
            }
            ResolvedTy::Ref(_, _) => self.context.ptr_type().into(),
            ResolvedTy::Array(resolved_ty, length) => self
                .context
                .array_type(
                    self.resolved_ty_to_ir_type(analyzer, resolved_ty, from),
                    *length,
                )
                .into(),
            ResolvedTy::Slice(_) => panic!("Impossible!"),
            ResolvedTy::Tup(items) => match &items[..] {
                [] => self.context.void_type().into(),
                [inner] => self.resolved_ty_to_ir_type(analyzer, inner, from),
                _ => panic!("Impossible!"),
            },
            ResolvedTy::Fn(items, resolved_ty) => self
                .context
                .function_type(
                    self.resolved_ty_to_ir_type(analyzer, resolved_ty, from),
                    items
                        .iter()
                        .map(|x| self.resolved_ty_to_ir_type(analyzer, x, from))
                        .collect(),
                )
                .into(),
            ResolvedTy::ImplicitSelf => self.resolved_ty_to_ir_type(
                analyzer,
                &analyzer.get_self_type_from(from.unwrap()).unwrap(),
                from,
            ),
            ResolvedTy::Never => self.context.void_type().into(),
        }
    }

    pub(crate) fn add_struct_declares(&mut self, analyzer: &SemanticAnalyzer, scope_id: NodeId) {
        let scope = analyzer.scopes.get(&scope_id).unwrap();
        for info in scope.types.values() {
            if let crate::semantics::utils::TypeKind::Struct { .. } = &info.kind {
                let name = analyzer.get_full_name_from(scope_id, info.name.clone());
                let ptr = self.context.create_opaque_struct_type(&name.to_string());
                self.structs.insert(name, ptr);
            }
        }
        for child in &scope.children {
            self.add_struct_declares(analyzer, *child);
        }
    }

    pub(crate) fn complete_struct_declares(&mut self, analyzer: &SemanticAnalyzer) {
        for (name, ptr) in &self.structs {
            let info = analyzer.get_type_info(name);
            ptr.set_body(
                info.kind
                    .as_struct()
                    .unwrap()
                    .values()
                    .map(|x| self.resolved_ty_to_ir_type(analyzer, x, None))
                    .filter(|x| !x.is_void())
                    .collect(),
                false,
            );
        }
    }

    pub(crate) fn add_globals(&mut self, analyzer: &SemanticAnalyzer, scope_id: NodeId) {
        let scope = analyzer.scopes.get(&scope_id).unwrap();

        for (symbol, var) in &scope.values {
            let name = analyzer.get_full_name_from(scope_id, symbol.clone());
            match &var.kind {
                crate::semantics::utils::VariableKind::Fn => {
                    let function_type =
                        self.resolved_ty_to_ir_type(analyzer, &var.ty, Some(scope_id));
                    debug_assert!(function_type.is_function());
                    self.module.add_function(
                        FunctionTypePtr(function_type),
                        &name.to_string(),
                        None,
                    );
                }
                crate::semantics::utils::VariableKind::Constant(const_eval_value) => {
                    if let Some(initializer) =
                        self.transform_constant(analyzer, scope_id, &var.ty, const_eval_value)
                    {
                        self.module
                            .add_global_variable(true, initializer, &name.to_string());
                    }
                }
                _ => {}
            }
        }
    }

    fn transform_constant(
        &self,
        analyzer: &SemanticAnalyzer,
        scope_id: usize,
        ty: &ResolvedTy,
        const_eval_value: &crate::const_eval::ConstEvalValue,
    ) -> Option<crate::ir::ir_value::ConstantPtr> {
        Some(match const_eval_value {
            crate::const_eval::ConstEvalValue::U32(num)
            | crate::const_eval::ConstEvalValue::USize(num)
            | crate::const_eval::ConstEvalValue::Integer(num) => self.context.get_i32(*num).into(),
            crate::const_eval::ConstEvalValue::I32(num)
            | crate::const_eval::ConstEvalValue::ISize(num)
            | crate::const_eval::ConstEvalValue::SignedInteger(num) => {
                self.context.get_i32(*num as u32).into()
            }
            crate::const_eval::ConstEvalValue::UnitStruct(_) => return None,
            crate::const_eval::ConstEvalValue::Bool(b) => self.context.get_i1(*b).into(),
            crate::const_eval::ConstEvalValue::Char(c) => self.context.get_i8(*c as u8).into(),
            crate::const_eval::ConstEvalValue::RefStr(s) => self.context.get_string(s).into(),
            crate::const_eval::ConstEvalValue::Array(const_eval_values) => {
                let inner_ty = ty.as_array().unwrap().0;
                let inner_ty1 = self.resolved_ty_to_ir_type(analyzer, inner_ty, Some(scope_id));
                if const_eval_values.is_empty() || inner_ty1.is_void() {
                    return None;
                }
                self.context
                    .get_array(
                        inner_ty1,
                        const_eval_values
                            .iter()
                            .map(|x| {
                                self.transform_constant(analyzer, scope_id, inner_ty, x)
                                    .unwrap()
                            })
                            .collect(),
                    )
                    .into()
            }
            crate::const_eval::ConstEvalValue::Struct(full_name, hash_map) => {
                let info = analyzer.get_type_info(full_name).kind.as_struct().unwrap();

                let struct_ty = self.structs.get(full_name)?;

                self.context
                    .get_struct(
                        struct_ty.clone(),
                        hash_map
                            .values()
                            .zip(info.values())
                            .filter_map(|(c, t)| self.transform_constant(analyzer, scope_id, t, c))
                            .collect(),
                    )
                    .into()
            }
            crate::const_eval::ConstEvalValue::Enum(full_name, symbol) => {
                let info = analyzer.get_type_info(full_name).kind.as_enum().unwrap();
                let num = info.iter().position(|x| x == symbol).unwrap();
                self.context.get_i32(num as u32).into()
            }
            _ => panic!("Impossible"),
        })
    }
}
