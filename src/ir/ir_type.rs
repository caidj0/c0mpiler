use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum IRType {
    Int(Rc<IntType>),
    Function(Rc<FunctionType>),
    Ptr,
    Struct(Rc<StructType>),
    Array(Rc<ArrayType>),
    Void,
}

impl IRType {
    fn function_type(self, params: Vec<Self>) -> Self {
        IRType::Function(Rc::new(FunctionType(self, params)))
    }

    fn array_type(self, length: u32) -> Self {
        IRType::Array(Rc::new(ArrayType(self, length)))
    }
}

#[derive(Debug, Clone)]
pub struct IntType(pub u32);

#[derive(Debug, Clone)]
pub struct FunctionType(pub IRType, pub Vec<IRType>);

#[derive(Debug, Clone)]
pub struct StructType(pub Vec<IRType>);

#[derive(Debug, Clone)]
pub struct ArrayType(pub IRType, pub u32);
