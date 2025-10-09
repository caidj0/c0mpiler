use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;

use enum_as_inner::EnumAsInner;

use crate::ir::ir_type::TypePtr;

pub type ValuePtr = Rc<Value>;

#[derive(Debug, PartialEq, Eq)]
pub struct Value {
    pub base: ValueBase,
    pub kind: ValueKind,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueBase {
    pub name: Option<String>,
    pub ty: TypePtr,
}

#[derive(Debug, EnumAsInner, PartialEq, Eq)]
pub enum ValueKind {
    BasicBlock(BasicBlock),
    Argument(Argument),
    Constant(Constant),
    Instruction(Instruction),
    Function(Function),
}

macro_rules! check_value_type {
    ($($cat:ident),*) => {
        impl Value{
            paste::paste!{
                $(

                pub fn [<is_ $cat:snake _type>] (&self) -> bool {
                    self.base.ty.[<is_ $cat:snake>]()
                }

                )*
            }
        }
    };
}

check_value_type!(Int, Function, Ptr, Struct, Array, Void, Label);

#[derive(Debug, EnumAsInner, PartialEq, Eq, Hash)]
pub enum Constant {
    ConstantInt(ConstantInt),
    ConstantArray(ConstantArray),
    ConstantStruct(ConstantStruct),
    ConstantString(ConstantString),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ConstantInt(pub u32);

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ConstantArray(pub Vec<ConstantPtr>);

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ConstantStruct(pub Vec<ConstantPtr>);

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ConstantString(pub String);

#[derive(Debug, PartialEq, Eq)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub operands: Vec<ValuePtr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum InstructionKind {
    Binary(BinaryOpcode), // 单目运算符都需要转化为双目运算符
    Call,
    Branch,
    Return,
    GetElementPtr { base_ty: TypePtr },
    Alloca,
    Load,
    Ret { is_void: bool },
    Store,
    Icmp(ICmpCode),
    Phi,
    Select, // TODO
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BinaryOpcode {
    Add,
    Sub,
    Mul,
    UDiv,
    SDiv,
    URem,
    SRem,

    Shl,
    LShr,
    AShr,
    And,
    Or,
    Xor,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ICmpCode {
    Eq,
    Ne,
    Ugt,
    Uge,
    Ult,
    Ule,
    Sgt,
    Sge,
    Slt,
    Sle,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    pub params: Vec<ArgumentPtr>,
    pub blocks: RefCell<HashMap<String, (BasicBlockPtr, usize)>>,
}

// 只有常量池才会
impl Hash for Function {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {
        panic!("This function shouldn't be called!");
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BasicBlock {
    pub instructions: RefCell<Vec<InstructionPtr>>,
}

impl Hash for BasicBlock {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {
        panic!("This function shouldn't be called!");
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Argument;

macro_rules! define_extension {
    ($parent:ident; $($name:ident),*) => {
        paste::paste!{
            $(
                #[derive(Debug, Clone, PartialEq, Eq)]
                pub struct [<$name Ptr>] (
                    pub(crate) [<$parent Ptr>]
                );

                impl From<[<$name Ptr>]> for [<$parent Ptr>] {
                    fn from(value: [<$name Ptr>]) -> Self {
                        value.0
                    }
                }

                impl Deref for [<$name Ptr>] {
                    type Target = [<$parent Ptr>];

                    fn deref(&self) -> &Self::Target {
                        &self.0
                    }
                }
            )*
        }
    };
}

define_extension!(Value; BasicBlock, Argument, Constant, Instruction, Function);
define_extension!(Constant; ConstantInt, ConstantArray, ConstantStruct, ConstantString);

impl Hash for ConstantPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.as_constant().unwrap().hash(state);
    }
}