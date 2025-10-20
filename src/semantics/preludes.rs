use crate::ast::Symbol;
use crate::semantics::analyzer::SemanticAnalyzer;
use crate::semantics::resolved_ty::RefMutability;
use crate::semantics::resolved_ty::ResolvedTy;
use crate::semantics::resolved_ty::ResolvedTyKind;
use crate::semantics::resolved_ty::TypeIntern;
use crate::semantics::resolved_ty::TypeKey;
use crate::semantics::utils::FullName;
use ena::unify::InPlace;
use ena::unify::UnificationTable;

macro_rules! define_preludes {
    ($($name:ident: $e:expr),*) => {
        #[derive(Debug)]
        pub struct Preludes{
            $(
                pub(crate) $name: TypeKey,
            )*
        }

        impl Preludes {
            pub fn new(ut: &mut UnificationTable<InPlace<TypeKey>>) -> Self {
                $(
                    let $name = ut.new_key($e);
                )*
                Self{
                    $(
                        $name
                    ),*
                }
            }

            $(
                pub fn $name(&self) -> TypeKey {
                    self.$name
                }
            )*
        }

        impl SemanticAnalyzer {
            $(
                paste::paste!{
                    pub fn [<$name _type>](&self) -> TypeIntern {
                        TypeIntern::Other(self.preludes.$name)
                    }
                }
            )*

            pub fn never_type(&self) -> TypeIntern {
                TypeIntern::Never
            }
        }
    };
}

define_preludes!(
    bool: ResolvedTy::bool_type(),
    char: ResolvedTy::char_type(),
    i32: ResolvedTy::i32_type(),
    isize: ResolvedTy::isize_type(),
    u32: ResolvedTy::u32_type(),
    usize: ResolvedTy::usize_type(),
    str: ResolvedTy::str_type(),
    unit: ResolvedTy::str_type(),

    ref_str: ResolvedTy{
        names: None,
        kind: ResolvedTyKind::Ref(TypeIntern::Other(str), RefMutability::Not)
    }
);

impl SemanticAnalyzer {
    pub fn new_any_type(&mut self) -> TypeIntern {
        self.intern_type(ResolvedTy::any_type()).into()
    }

    pub fn new_any_int_type(&mut self) -> TypeIntern {
        self.intern_type(ResolvedTy::any_int_type()).into()
    }

    pub fn new_any_signed_int_type(&mut self) -> TypeIntern {
        self.intern_type(ResolvedTy::any_signed_int_type()).into()
    }

    pub fn is_string_type(ty: &ResolvedTy) -> bool {
        ty.names == Some((FullName(vec![Symbol("String".to_string())]), None))
    }
}
