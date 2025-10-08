pub mod ir_type;
pub mod ir_value;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::Crate,
    ir::ir_type::{FunctionType, IRType, IntType},
    semantics::SemanticAnalyzer,
};

struct ContextPreludes {
    int: RefCell<HashMap<u32, Rc<IntType>>>,
}

impl Default for ContextPreludes {
    fn default() -> Self {
        Self {
            int: RefCell::default(),
        }
    }
}

impl ContextPreludes {
    fn int_type(&self, bit_width: u32) -> IRType {
        IRType::Int(if let Some(ty) = self.int.borrow().get(&bit_width) {
            ty.clone()
        } else {
            let ty: Rc<IntType> = IntType(bit_width).into();
            self.int.borrow_mut().insert(bit_width, ty.clone());
            ty
        })
    }

    fn i1_type(&self) -> IRType {
        self.int_type(1)
    }

    fn i8_type(&self) -> IRType {
        self.int_type(8)
    }

    fn i32_type(&self) -> IRType {
        self.int_type(32)
    }

    fn void_type(&self) -> IRType {
        IRType::Void
    }

    fn ptr_type(&self) -> IRType {
        IRType::Ptr
    }
}

// 存放符号表，常量等
pub struct LLVMContext {
    modules: HashMap<String, LLVMModule>,
    structs: HashMap<String, Vec<IRType>>,

    preludes: ContextPreludes,
}

impl LLVMContext {
    fn int_type(&self, bit_width: u32) -> IRType {
        self.preludes.int_type(bit_width)
    }

    fn i1_type(&self) -> IRType {
        self.preludes.i1_type()
    }

    fn i8_type(&self) -> IRType {
        self.preludes.i8_type()
    }

    fn i32_type(&self) -> IRType {
        self.preludes.i32_type()
    }

    fn void_type(&self) -> IRType {
        self.preludes.void_type()
    }

    fn ptr_type(&self) -> IRType {
        self.preludes.ptr_type()
    }
}

pub struct LLVMModule {
    functions: HashMap<String, Rc<FunctionType>>,
}
