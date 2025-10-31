use crate::ast::Mutability;
use crate::ast::Symbol;
use crate::semantics::analyzer::SemanticAnalyzer;
use crate::semantics::item::AssociatedInfo;
use crate::semantics::resolved_ty::RefMutability;
use crate::semantics::resolved_ty::ResolvedTy;
use crate::semantics::resolved_ty::ResolvedTyKind;
use crate::semantics::resolved_ty::TypeIntern;
use crate::semantics::resolved_ty::TypeKey;
use crate::semantics::utils::FullName;
use crate::semantics::value::MethodKind;
use crate::semantics::value::PlaceValue;
use crate::semantics::value::Value;
use crate::semantics::value::ValueKind;
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
    unit: ResolvedTy::unit_type(),

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

    pub(crate) fn add_prelude_functions(&mut self) {
        let string_ty = self
            .add_type_placeholder(0, Symbol::from("String"), None)
            .unwrap();
        let ref_str_ty = self.ref_str_type();
        let str_ty = self.str_type();
        let unit_ty = self.unit_type();
        let i32_ty = self.i32_type();
        let never_ty = self.never_type();
        let u32_ty = self.u32_type();
        let usize_ty = self.usize_type();

        let method_value = |ty, method_kind| PlaceValue {
            value: Value {
                ty,
                kind: ValueKind::Fn {
                    method_kind,
                    is_placeholder: false,
                },
            },
            mutbl: Mutability::Not,
        };

        let fn_value = |ty| method_value(ty, MethodKind::Not);

        let print_ty = self.intern_type(ResolvedTy::fn_type(unit_ty, vec![ref_str_ty]));
        self.add_scope_value(0, &Symbol::from("print"), fn_value(print_ty.into()))
            .unwrap();
        self.add_scope_value(0, &Symbol::from("println"), fn_value(print_ty.into()))
            .unwrap();

        let print_int_ty = self.intern_type(ResolvedTy::fn_type(unit_ty, vec![i32_ty]));
        self.add_scope_value(0, &Symbol::from("printInt"), fn_value(print_int_ty.into()))
            .unwrap();
        self.add_scope_value(
            0,
            &Symbol::from("printlnInt"),
            fn_value(print_int_ty.into()),
        )
        .unwrap();

        let get_string_ty = self.intern_type(ResolvedTy::fn_type(string_ty.into(), vec![]));
        self.add_scope_value(
            0,
            &Symbol::from("getString"),
            fn_value(get_string_ty.into()),
        )
        .unwrap();

        let get_int_ty = self.intern_type(ResolvedTy::fn_type(i32_ty, vec![]));
        self.add_scope_value(0, &Symbol::from("getInt"), fn_value(get_int_ty.into()))
            .unwrap();

        let exit_ty = self.intern_type(ResolvedTy::fn_type(never_ty, vec![i32_ty]));
        self.add_scope_value(0, &Symbol::from("exit"), fn_value(exit_ty.into()))
            .unwrap();

        let ref_u32_ty = self.intern_type(ResolvedTy::ref_type(u32_ty, RefMutability::Not));
        let ref_usize_ty = self.intern_type(ResolvedTy::ref_type(usize_ty, RefMutability::Not));

        // TODO: 把 any int type 转发到这两个
        let u32_to_string_ty = self.intern_type(ResolvedTy::fn_type(
            string_ty.into(),
            vec![ref_u32_ty.into()],
        ));
        let usize_to_string_ty = self.intern_type(ResolvedTy::fn_type(
            string_ty.into(),
            vec![ref_usize_ty.into()],
        ));

        self.add_impl_value(
            &AssociatedInfo {
                is_trait: false,
                ty: u32_ty.to_key(),
                for_trait: None,
            },
            &Symbol::from("to_string"),
            method_value(u32_to_string_ty.into(), MethodKind::ByRef),
        )
        .unwrap();

        self.add_impl_value(
            &AssociatedInfo {
                is_trait: false,
                ty: usize_ty.to_key(),
                for_trait: None,
            },
            &Symbol::from("to_string"),
            method_value(usize_to_string_ty.into(), MethodKind::ByRef),
        )
        .unwrap();

        let ref_string_ty =
            self.intern_type(ResolvedTy::ref_type(string_ty.into(), RefMutability::Not));
        let as_str_ty =
            self.intern_type(ResolvedTy::fn_type(ref_str_ty, vec![ref_string_ty.into()]));
        self.add_impl_value(
            &AssociatedInfo {
                is_trait: false,
                ty: string_ty,
                for_trait: None,
            },
            &Symbol::from("as_str"),
            method_value(as_str_ty.into(), MethodKind::ByRef),
        )
        .unwrap();

        let string_len_ty =
            self.intern_type(ResolvedTy::fn_type(usize_ty, vec![ref_string_ty.into()]));
        let str_len_ty = self.intern_type(ResolvedTy::fn_type(usize_ty, vec![ref_str_ty]));
        self.add_impl_value(
            &AssociatedInfo {
                is_trait: false,
                ty: string_ty,
                for_trait: None,
            },
            &Symbol::from("len"),
            method_value(string_len_ty.into(), MethodKind::ByRef),
        )
        .unwrap();
        self.add_impl_value(
            &AssociatedInfo {
                is_trait: false,
                ty: str_ty.to_key(),
                for_trait: None,
            },
            &Symbol::from("len"),
            method_value(str_len_ty.into(), MethodKind::ByRef),
        )
        .unwrap();
    }
}
