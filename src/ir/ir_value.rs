use std::cell::RefCell;
use std::hash::Hash;
use std::ops::Deref;
use std::rc::{Rc, Weak};

use enum_as_inner::EnumAsInner;

use crate::ir::globalxxx::GlobalObject;
use crate::ir::ir_type::TypePtr;

pub type ValuePtr = Rc<Value>;

#[derive(Debug)]
pub struct Value {
    pub base: ValueBase,
    pub kind: ValueKind,
}

impl Value {
    pub fn new(base: ValueBase, kind: ValueKind) -> Self {
        Self { base, kind }
    }

    pub fn set_name(&self, name: String) {
        *self.base.name.borrow_mut() = Some(name);
    }

    pub fn get_name(&self) -> Option<String> {
        self.base.name.borrow().clone()
    }

    pub fn get_type(&self) -> &TypePtr {
        &self.base.ty
    }

    pub fn add_user(&self, user: &ValuePtr) {
        self.base.users.borrow_mut().push(Rc::downgrade(user));
    }

    pub fn is_used(&self) -> bool {
        !self.base.users.borrow().is_empty()
    }
}

#[derive(Debug)]
pub struct ValueBase {
    pub name: RefCell<Option<String>>,
    pub ty: TypePtr,
    pub users: RefCell<Vec<Weak<Value>>>,
}

impl ValueBase {
    pub fn new(ty: TypePtr, name: Option<&str>) -> Self {
        Self {
            name: RefCell::new(name.map(|x| x.to_string())),
            ty,
            users: vec![].into(),
        }
    }
}

#[derive(Debug, EnumAsInner)]
pub enum ValueKind {
    BasicBlock(BasicBlock),
    Argument(Argument),
    Constant(Constant),
    Instruction(Instruction),
    GlobalObject(GlobalObject),
}

macro_rules! check_value_type {
    ($($cat:ident),*) => {
        impl Value{
            paste::paste!{
                $(

                pub fn [<is_ $cat:snake _type>] (&self) -> bool {
                    self.base.ty.[<is_ $cat:snake>]()
                }

                pub fn [<get_type_as_ $cat:snake>] (&self) -> Option<& crate::ir::ir_type::[<$cat:camel Type>]> {
                    self.base.ty.[<as_ $cat:snake>]()
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
    ConstantNull(ConstantNull),
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
pub struct ConstantNull;

#[derive(Debug)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub operands: Vec<ValuePtr>,
}

impl Instruction {
    pub fn get_instruction_name(&self) -> &'static str {
        match &self.kind {
            InstructionKind::Binary(binary_opcode) => binary_opcode.get_operator_name(),
            InstructionKind::Call => "call",
            InstructionKind::Branch { .. } => "br",
            InstructionKind::GetElementPtr { .. } => "getelementptr",
            InstructionKind::Alloca { .. } => "alloca",
            InstructionKind::Load => "load",
            InstructionKind::Ret { .. } => "ret",
            InstructionKind::Store => "store",
            InstructionKind::Icmp(..) => "icmp",
            InstructionKind::Phi => "phi",
            InstructionKind::Select => "select",
            InstructionKind::PtrToInt { .. } => "ptrtoint",
            InstructionKind::Trunc => "trunc",
            InstructionKind::Zext => "zext",
            InstructionKind::Sext => "sext",
            InstructionKind::Unreachable => "unreachable",
        }
    }

    pub fn is_terminate(&self) -> bool {
        matches!(
            self.kind,
            InstructionKind::Branch { .. }
                | InstructionKind::Ret { .. }
                | InstructionKind::Unreachable
        )
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum InstructionKind {
    Binary(BinaryOpcode), // 单目运算符都需要转化为双目运算符
    Call,
    Branch { has_cond: bool },
    GetElementPtr { base_ty: TypePtr },
    Alloca { inner_ty: TypePtr },
    Load,
    Ret { is_void: bool },
    Store,
    Icmp(ICmpCode),
    Phi,
    Select,
    PtrToInt,
    Trunc,
    Zext,
    Sext,
    Unreachable,
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

impl BinaryOpcode {
    pub fn get_operator_name(&self) -> &'static str {
        match self {
            BinaryOpcode::Add => "add",
            BinaryOpcode::Sub => "sub",
            BinaryOpcode::Mul => "mul",
            BinaryOpcode::UDiv => "udiv",
            BinaryOpcode::SDiv => "sdiv",
            BinaryOpcode::URem => "urem",
            BinaryOpcode::SRem => "srem",
            BinaryOpcode::Shl => "shl",
            BinaryOpcode::LShr => "lshr",
            BinaryOpcode::AShr => "ashr",
            BinaryOpcode::And => "and",
            BinaryOpcode::Or => "or",
            BinaryOpcode::Xor => "xor",
        }
    }
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

impl ICmpCode {
    pub fn get_operator_name(&self) -> &'static str {
        match &self {
            ICmpCode::Eq => "eq",
            ICmpCode::Ne => "ne",
            ICmpCode::Ugt => "ugt",
            ICmpCode::Uge => "uge",
            ICmpCode::Ult => "ult",
            ICmpCode::Ule => "ule",
            ICmpCode::Sgt => "sgt",
            ICmpCode::Sge => "sge",
            ICmpCode::Slt => "slt",
            ICmpCode::Sle => "sle",
        }
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub instructions: RefCell<Vec<InstructionPtr>>,
}

impl Hash for BasicBlock {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {
        panic!("This function shouldn't be called!");
    }
}

#[derive(Debug)]
pub struct Argument;

#[macro_export]
macro_rules! define_extension {
    ($parent:ident; $($name:ident),*) => {
        paste::paste!{
            $(
                #[derive(Debug, Clone)]
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

define_extension!(Value; BasicBlock, Argument, Constant, Instruction, GlobalObject);
define_extension!(Constant; ConstantInt, ConstantArray, ConstantStruct, ConstantString, ConstantNull);

impl Hash for ConstantPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.as_constant().unwrap().hash(state);
    }
}

impl PartialEq for ConstantPtr {
    fn eq(&self, other: &Self) -> bool {
        self.as_constant() == other.as_constant()
    }
}

impl Eq for ConstantPtr {}

macro_rules! value_dispatch {
    ($($name:ident),*) => {
        $(
            paste::paste!{
                impl [<$name:camel Ptr>] {
                    pub fn [<as_ $name:snake>] (&self) -> & $name {
                        self.kind.[<as_ $name:snake>]().unwrap()
                    }
                }
            }
        )*
    };
}

value_dispatch!(BasicBlock, Argument, Constant, Instruction, GlobalObject);

#[macro_export]
macro_rules! into_extend {
    ($grandfather:ident; $father:ident ;$($name:ident),*) => {
        $(
            paste::paste!{
                impl From<$name> for $grandfather {
                    fn from(value: $name) -> Self {
                        Into::<$father>::into(value).into()
                    }
                }
            }
        )*
    };
}

into_extend!(ValuePtr; ConstantPtr; ConstantIntPtr, ConstantArrayPtr, ConstantStructPtr, ConstantStringPtr);
