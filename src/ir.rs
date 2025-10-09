pub mod ir_type;
pub mod ir_value;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
    vec,
};

use crate::ir::{
    ir_type::{ArrayType, IntType, PtrType, StructType, StructTypePtr, Type, TypePtr},
    ir_value::{
        ConstantArray, ConstantArrayPtr, ConstantInt, ConstantIntPtr, ConstantPtr, ConstantString,
        ConstantStringPtr, ConstantStruct, ConstantStructPtr, FunctionPtr, Instruction,
        InstructionPtr, Value, ValueBase, ValuePtr,
    },
};

#[derive(Debug, Default)]
struct ContextPool<T>(HashSet<Rc<T>>);

impl<T> ContextPool<T>
where
    T: Eq + Hash + Clone,
{
    fn get_ty(&mut self, ty: &T) -> Rc<T> {
        if let Some(ret) = self.0.get(ty) {
            ret.clone()
        } else {
            let ret = Rc::new(ty.clone());
            self.0.insert(Rc::new(ty.clone()));
            ret
        }
    }
}

type ContextTypePool = ContextPool<Type>;

impl ContextTypePool {
    fn int_type(&mut self, bit_width: u8) -> TypePtr {
        self.get_ty(&Type::Int(IntType(bit_width)))
    }

    fn i1_type(&mut self) -> TypePtr {
        self.int_type(1)
    }

    fn i8_type(&mut self) -> TypePtr {
        self.int_type(8)
    }

    fn i32_type(&mut self) -> TypePtr {
        self.int_type(32)
    }

    fn void_type(&mut self) -> TypePtr {
        self.get_ty(&Type::Void)
    }

    fn ptr_type(&mut self) -> TypePtr {
        self.get_ty(&Type::Ptr(PtrType))
    }

    fn label_type(&mut self) -> TypePtr {
        self.get_ty(&Type::Label)
    }
}

// 存放符号表，常量等
pub struct LLVMContext {
    modules: HashMap<String, Rc<LLVMModule>>,

    ctx_impl: Rc<RefCell<LLVMContextImpl>>,
}

impl LLVMContext {
    pub fn create_builder(&self) -> LLVMBuilder {
        LLVMBuilder {
            ctx_impl: self.ctx_impl.clone(),
        }
    }

    pub fn create_module(&mut self, name: String) -> Rc<LLVMModule> {
        let module = Rc::new(LLVMModule::default());
        self.modules.insert(name, module.clone());
        module
    }

    pub fn i1_type(&self) -> TypePtr {
        self.ctx_impl.borrow_mut().i1_type()
    }

    pub fn i8_type(&self) -> TypePtr {
        self.ctx_impl.borrow_mut().i8_type()
    }

    pub fn i32_type(&self) -> TypePtr {
        self.ctx_impl.borrow_mut().i32_type()
    }

    pub fn void_type(&self) -> TypePtr {
        self.ctx_impl.borrow_mut().void_type()
    }

    pub fn ptr_type(&self) -> TypePtr {
        self.ctx_impl.borrow_mut().ptr_type()
    }

    pub fn array_type(&self, inner_type: TypePtr, length: u32) -> TypePtr {
        self.ctx_impl
            .borrow_mut()
            .get_array_type(inner_type, length)
    }

    pub fn struct_type(&self, inner_types: Vec<TypePtr>, packed: bool) -> TypePtr {
        self.ctx_impl
            .borrow_mut()
            .get_struct_type(inner_types, packed)
    }

    pub fn create_opaque_struct_type(&self, name: String) -> TypePtr {
        self.ctx_impl.borrow_mut().create_opaque_struct_type(name)
    }

    pub fn get_i1(&self, value: bool) -> ConstantIntPtr {
        self.ctx_impl.borrow_mut().get_i1(value)
    }

    pub fn get_true(&self) -> ConstantIntPtr {
        self.ctx_impl.borrow_mut().get_true()
    }

    pub fn get_false(&self) -> ConstantIntPtr {
        self.ctx_impl.borrow_mut().get_false()
    }

    pub fn get_i8(&self, value: u8) -> ConstantIntPtr {
        self.ctx_impl.borrow_mut().get_i8(value)
    }

    pub fn get_i32(&self, value: u32) -> ConstantIntPtr {
        self.ctx_impl.borrow_mut().get_i32(value)
    }

    pub fn get_array(&self, inner_ty: TypePtr, values: Vec<ConstantPtr>) -> ConstantArrayPtr {
        self.ctx_impl.borrow_mut().get_array(inner_ty, values)
    }

    pub fn get_struct(
        &self,
        struct_ty: StructTypePtr,
        values: Vec<ConstantPtr>,
    ) -> ConstantStructPtr {
        self.ctx_impl.borrow_mut().get_struct(struct_ty, values)
    }

    pub fn get_string(&self, string: String) -> ConstantStringPtr {
        self.ctx_impl.borrow_mut().get_string(string)
    }
}

pub struct LLVMContextImpl {
    ty_pool: ContextTypePool,
    named_strcut_ty: HashMap<String, TypePtr>,

    // 常量才会被缓存，指向唯一地址
    // 所以常量从 context 获取，其余从 builder 获取
    integer_pool: HashMap<(u8, u32), ConstantIntPtr>, // (位数, 大小)
    array_pool: HashMap<(TypePtr, Vec<ConstantPtr>), ConstantArrayPtr>,
    struct_pool: HashMap<Vec<ConstantPtr>, ConstantStructPtr>,
    string_pool: HashMap<String, ConstantStringPtr>,
}

impl LLVMContextImpl {
    fn int_type(&mut self, bit_width: u8) -> TypePtr {
        self.ty_pool.int_type(bit_width)
    }

    fn i1_type(&mut self) -> TypePtr {
        self.ty_pool.i1_type()
    }

    fn i8_type(&mut self) -> TypePtr {
        self.ty_pool.i8_type()
    }

    fn i32_type(&mut self) -> TypePtr {
        self.ty_pool.i32_type()
    }

    fn void_type(&mut self) -> TypePtr {
        self.ty_pool.void_type()
    }

    fn ptr_type(&mut self) -> TypePtr {
        self.ty_pool.ptr_type()
    }

    fn label_type(&mut self) -> TypePtr {
        self.ty_pool.label_type()
    }

    fn get_array_type(&mut self, inner_type: TypePtr, length: u32) -> TypePtr {
        self.ty_pool
            .get_ty(&Type::Array(ArrayType(inner_type, length)))
    }

    fn get_struct_type(&mut self, inner_types: Vec<TypePtr>, packed: bool) -> TypePtr {
        self.ty_pool.get_ty(&Type::Struct(StructType(RefCell::new(
            ir_type::StructTypeEnum::Body {
                ty: inner_types,
                packed,
            },
        ))))
    }

    fn create_opaque_struct_type(&mut self, name: String) -> TypePtr {
        let ret = Rc::new(Type::Struct(StructType(RefCell::new(
            ir_type::StructTypeEnum::Opaque,
        ))));
        self.named_strcut_ty.insert(name, ret.clone());
        ret
    }

    fn get_int(&mut self, value: u32, bit_width: u8) -> ConstantIntPtr {
        if let Some(ret) = self.integer_pool.get(&(bit_width, value)) {
            ret.clone()
        } else {
            let ret = ConstantIntPtr(ConstantPtr(ValuePtr::new(Value {
                base: ValueBase {
                    name: None,
                    ty: self.int_type(bit_width),
                },
                kind: ir_value::ValueKind::Constant(ir_value::Constant::ConstantInt(ConstantInt(
                    value,
                ))),
            })));
            self.integer_pool.insert((bit_width, value), ret.clone());
            ret
        }
    }

    fn get_i1(&mut self, value: bool) -> ConstantIntPtr {
        self.get_int(value as u32, 1)
    }

    fn get_true(&mut self) -> ConstantIntPtr {
        self.get_int(1, 1)
    }

    fn get_false(&mut self) -> ConstantIntPtr {
        self.get_int(0, 1)
    }

    fn get_i8(&mut self, value: u8) -> ConstantIntPtr {
        self.get_int(value as u32, 8)
    }

    fn get_i32(&mut self, value: u32) -> ConstantIntPtr {
        self.get_int(value, 32)
    }

    fn get_array(&mut self, inner_ty: TypePtr, values: Vec<ConstantPtr>) -> ConstantArrayPtr {
        debug_assert!(values.iter().all(|x| x.base.ty == inner_ty));

        let key = (inner_ty, values);

        if let Some(ret) = self.array_pool.get(&key) {
            ret.clone()
        } else {
            let array_ty = self.get_array_type(key.0.clone(), key.1.len() as u32);

            let ret = ConstantArrayPtr(ConstantPtr(
                Value {
                    base: ValueBase {
                        name: None,
                        ty: array_ty,
                    },
                    kind: ir_value::ValueKind::Constant(ir_value::Constant::ConstantArray(
                        ConstantArray(key.1.clone()),
                    )),
                }
                .into(),
            ));

            self.array_pool.insert(key, ret.clone());

            ret
        }
    }

    fn get_struct(
        &mut self,
        struct_ty: StructTypePtr,
        values: Vec<ConstantPtr>,
    ) -> ConstantStructPtr {
        debug_assert!(
            struct_ty
                .is_fields_type_same(&values.iter().map(|x| x.base.ty.clone()).collect::<Vec<_>>())
        );

        if let Some(ret) = self.struct_pool.get(&values) {
            ret.clone()
        } else {
            let ret = ConstantStructPtr(ConstantPtr(
                Value {
                    base: ValueBase {
                        name: None,
                        ty: struct_ty.into(),
                    },
                    kind: ir_value::ValueKind::Constant(ir_value::Constant::ConstantStruct(
                        ConstantStruct(values.clone()),
                    )),
                }
                .into(),
            ));

            self.struct_pool.insert(values, ret.clone());

            ret
        }
    }

    fn get_string(&mut self, string: String) -> ConstantStringPtr {
        if let Some(ret) = self.string_pool.get(&string) {
            ret.clone()
        } else {
            let i8_type = self.i8_type();
            let array_ty = self.get_array_type(i8_type, (string.len() + 1) as u32);

            let ret = ConstantStringPtr(ConstantPtr(
                Value {
                    base: ValueBase {
                        name: None,
                        ty: array_ty,
                    },
                    kind: ir_value::ValueKind::Constant(ir_value::Constant::ConstantString(
                        ConstantString(string.clone()),
                    )),
                }
                .into(),
            ));

            self.string_pool.insert(string, ret.clone());

            ret
        }
    }
}

pub struct LLVMBuilder {
    ctx_impl: Rc<RefCell<LLVMContextImpl>>,
}

impl LLVMBuilder {
    pub fn build_alloca(&self, ty: TypePtr, name: String) -> InstructionPtr {
        InstructionPtr(
            Value {
                base: ValueBase {
                    name: Some(name),
                    ty: ty,
                },
                kind: ir_value::ValueKind::Instruction(Instruction {
                    kind: ir_value::InstructionKind::Alloca,
                    operands: vec![],
                }),
            }
            .into(),
        )
    }

    pub fn build_conditional_branch(&self, cond: ValuePtr, iftrue: ValuePtr, ifelse: ValuePtr) -> InstructionPtr {
        debug_assert!({
            let mut ctx = self.ctx_impl.borrow_mut();
            let i1_type = ctx.i1_type();
            let label_type = ctx.label_type();

            cond.base.ty == i1_type && iftrue.base.ty == label_type && ifelse.base.ty == label_type
        });

        InstructionPtr(
            Value {
                base: ValueBase { name: None, ty: ty }, // 它的类型是什么？Store 指令是怎么插进去的？
                kind: ir_value::ValueKind::Instruction(Instruction {
                    kind: ir_value::InstructionKind::Branch,
                    operands: vec![cond],
                }),
            }
            .into(),
        )
    }
}

#[derive(Debug, Default)]
pub struct LLVMModule {
    functions: HashMap<String, FunctionPtr>,
}

impl LLVMModule {}
