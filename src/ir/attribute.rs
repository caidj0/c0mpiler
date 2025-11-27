use std::iter::repeat_with;

use enum_as_inner::EnumAsInner;
use strum::{EnumCount, EnumDiscriminants};

use crate::ir::ir_type::TypePtr;

#[derive(Debug, EnumCount, EnumDiscriminants, Clone, EnumAsInner)]
pub enum Attribute {
    StructReturn(TypePtr),
}

#[derive(Debug, Default)]
pub struct AttributeList {
    pub(crate) defined: [Option<Attribute>; Attribute::COUNT],
}

impl AttributeList {
    pub fn set_attr(&mut self, attr: Attribute) {
        let index = AttributeDiscriminants::from(&attr) as usize;
        self.defined[index] = Some(attr);
    }

    pub fn get_attr(&self, attr: AttributeDiscriminants) -> Option<&Attribute> {
        self.defined[attr as usize].as_ref()
    }
}

#[derive(Debug)]
pub struct FunctionAttribute {
    pub(crate) fn_attr: AttributeList,
    pub(crate) ret_attr: AttributeList,
    pub(crate) params_attr: Vec<AttributeList>,
}

impl FunctionAttribute {
    pub fn new(param_num: usize) -> Self {
        Self {
            fn_attr: AttributeList::default(),
            ret_attr: AttributeList::default(),
            params_attr: repeat_with(AttributeList::default)
                .take(param_num)
                .collect(),
        }
    }
}
