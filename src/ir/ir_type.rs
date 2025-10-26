use std::{cell::RefCell, hash::Hash, ops::Deref, rc::Rc};

use enum_as_inner::EnumAsInner;

pub type TypePtr = Rc<Type>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum Type {
    Int(IntType),
    Function(FunctionType),
    Ptr(PtrType),
    Struct(StructType),
    Array(ArrayType),
    Void(VoidType),

    Label(LabelType), // basic block 专用
}

impl Type {
    pub fn is_aggregate_type(&self) -> bool {
        matches!(
            self,
            Type::Array(..) | Type::Function(..) | Type::Struct(..)
        )
    }

    pub fn is_zero_length_type(&self) -> bool {
        self.is_void()
            || self.as_struct().map_or(false, |x| {
                x.kind.borrow().as_body().map_or(false, |y| y.0.is_empty())
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntType(pub u8);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType(pub Rc<Type>, pub Vec<Rc<Type>>);

#[derive(Debug, Clone, Eq)]
pub struct StructType {
    pub name: RefCell<Option<String>>,
    pub kind: RefCell<StructTypeEnum>,
}

impl PartialEq for StructType {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Hash for StructType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let ptr = self.kind.borrow();
        ptr.as_body().unwrap().hash(state);
    }
}

impl StructType {
    pub fn set_body(&self, ty: Vec<TypePtr>, packed: bool) {
        (*self.kind.borrow_mut()) = StructTypeEnum::Body { ty, packed };
    }

    pub fn get_body(&self) -> Option<Vec<Rc<Type>>> {
        self.kind.borrow().as_body().map(|x| x.0).cloned()
    }

    pub fn get_name(&self) -> Option<String> {
        self.name.borrow().clone()
    }

    pub fn is_fields_type_same(&self, tys: &[TypePtr]) -> bool {
        let borrowed = self.kind.borrow();
        let Some((body, _)) = borrowed.as_body() else {
            return false;
        };

        body.as_slice() == tys
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum StructTypeEnum {
    Opaque,
    Body { ty: Vec<Rc<Type>>, packed: bool },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType(pub Rc<Type>, pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PtrType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabelType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VoidType;

macro_rules! define_extension {
    ($($name:ident),*) => {
        paste::paste!{
            $(
                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                pub struct [<$name Type Ptr>] (
                    pub(crate) TypePtr
                );

                impl Deref for [<$name Type Ptr>] {
                    type Target = [<$name Type>];

                    fn deref(&self) -> &Self::Target {
                        self.0.[<as_ $name:lower>]().unwrap()
                    }
                }

                impl From<[<$name Type Ptr>]> for TypePtr {
                    fn from(value: [<$name Type Ptr>]) -> Self {
                        value.0
                    }
                }

                impl From<TypePtr> for [<$name Type Ptr>] {
                    fn from(value: TypePtr) -> Self {
                        debug_assert!(value.[<is_ $name:lower>]());
                        Self(value)
                    }
                }
            )*
        }
    };
}

define_extension!(Int, Function, Ptr, Struct, Array, Label, Void);
