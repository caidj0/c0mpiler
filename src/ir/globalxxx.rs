use enum_as_inner::EnumAsInner;
use std::ops::Deref;

use crate::{
    into_extend,
    ir::{
        attribute::{Attribute, AttributeDiscriminants, FunctionAttribute},
        ir_value::{GlobalObjectPtr, ValuePtr},
    },
};
use std::cell::RefCell;

use crate::{
    define_extension,
    ir::{
        ir_type::TypePtr,
        ir_value::{ArgumentPtr, BasicBlockPtr, ConstantPtr},
    },
};

// 此处的 type 都是 ptr，同时记录指向的 type
#[derive(Debug)]
pub struct GlobalObject {
    pub inner_ty: TypePtr,
    pub kind: GlobalObjectKind,
}

impl GlobalObject {
    pub fn get_inner_ty(&self) -> &TypePtr {
        &self.inner_ty
    }
}

#[derive(Debug, EnumAsInner)]
pub enum GlobalObjectKind {
    Function(Function),
    GlobalVariable(GlobalVariable),
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<ArgumentPtr>,
    pub blocks: RefCell<Vec<BasicBlockPtr>>,

    pub attr: RefCell<FunctionAttribute>,
}

impl Function {
    pub fn get_nth_argument(&self, n: usize) -> Option<&ArgumentPtr> {
        self.params.get(n)
    }

    pub fn args(&self) -> &[ArgumentPtr] {
        &self.params
    }

    pub fn add_fn_attr(&self, attr: Attribute) {
        self.attr.borrow_mut().fn_attr.set_attr(attr);
    }

    pub fn get_fn_attr(&self, attr: AttributeDiscriminants) -> Option<Attribute> {
        self.attr.borrow().fn_attr.get_attr(attr).cloned()
    }

    pub fn add_ret_attr(&self, attr: Attribute) {
        self.attr.borrow_mut().ret_attr.set_attr(attr);
    }

    pub fn get_ret_attr(&self, attr: AttributeDiscriminants) -> Option<Attribute> {
        self.attr.borrow().ret_attr.get_attr(attr).cloned()
    }

    pub fn add_param_attr(&self, index: usize, attr: Attribute) {
        self.attr
            .borrow_mut()
            .params_attr
            .get_mut(index)
            .unwrap()
            .set_attr(attr);
    }

    pub fn get_param_attr(&self, index: usize, attr: AttributeDiscriminants) -> Option<Attribute> {
        self.attr
            .borrow()
            .params_attr
            .get(index)
            .and_then(|x| x.get_attr(attr).cloned())
    }
}

#[derive(Debug)]
pub struct GlobalVariable {
    pub is_constant: bool,
    pub initializer: ConstantPtr,
}

define_extension!(GlobalObject; Function, GlobalVariable);

macro_rules! global_object_dispatch {
    ($($name:ident),*) => {
        $(
            paste::paste!{
                impl [<$name:camel Ptr>] {
                    pub fn [<as_ $name:snake>] (&self) -> & $name {
                        self.as_global_object().kind.[<as_ $name:snake>]().unwrap()
                    }
                }
            }
        )*
    };
}

global_object_dispatch!(Function, GlobalVariable);
into_extend!(ValuePtr; GlobalObjectPtr; FunctionPtr, GlobalVariablePtr);
