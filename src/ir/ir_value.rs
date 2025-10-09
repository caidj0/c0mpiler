use std::ops::Deref;
use std::rc::Rc;

use enum_as_inner::EnumAsInner;

use crate::ir::ir_type::TypePtr;

pub type ValuePtr = Rc<Value>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Value {
    pub base: ValueBase,
    pub kind: ValueKind,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ValueBase {
    pub name: Option<String>,
    pub ty: TypePtr,
}

#[derive(Debug, EnumAsInner, PartialEq, Eq, Hash)]
pub enum ValueKind {
    BasicBlock(BasicBlock),
    Argument(Argument),
    Constant(Constant),
    Instruction(Instruction),
    Function(Function),
}

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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub operands: Vec<ValuePtr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum InstructionKind {
    Binary(), // 单目运算符都需要转化为双目运算符
    Call(),
    Branch,
    Return,
    GetElementPtr,
    Alloca,
    // TODO
}

#[derive(Debug)]
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
pub struct Function {
    pub params: Vec<ArgumentPtr>,
    pub blocks: Vec<BasicBlockPtr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct BasicBlock {
    pub instructions: Vec<InstructionPtr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Argument;

macro_rules! define_extension {
    ($parent:ident; $($name:ident),*) => {
        paste::paste!{
            $(
                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
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