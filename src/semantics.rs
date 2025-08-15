pub mod visitor;

use std::collections::HashMap;

use crate::{
    ast::{
        BindingMode, Ident, Mutability,
        item::{ConstItem, FieldDef, FnDecl, FnItem, FnRetTy, FnSig, Param, TraitRef},
        pat::{IdentPat, Pat, PatKind},
        path::{Path, PathSegment},
        ty::{MutTy, PathTy, RefTy, TupTy, Ty, TyKind},
    },
    semantics::visitor::Visitor,
};

#[derive(Debug)]
pub enum SemanticError {}

#[derive(Debug)]
pub struct ImplInfo {
    pub trait_of: Option<TraitRef>,
    pub methods: Vec<FnItem>,
    pub consts: Vec<ConstItem>,
}

#[derive(Debug)]
pub enum TyNameSpace {
    Struct {
        fields: Vec<FieldDef>,
        impls: Vec<ImplInfo>,
    },
    Enum {
        variants: Vec<Ident>,
        impls: Vec<ImplInfo>,
    },
    Trait {
        methods: Vec<FnItem>,
        consts: Vec<ConstItem>,
    },
    BuiltIn,
}

#[derive(Debug)]
pub enum ValueNameSpace {
    Fn(FnSig),
    Const(ConstItem),
}

#[derive(Debug)]
pub struct Variable {
    pub ty: Ty,
    pub mutbl: Mutability,
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub type_names: HashMap<Ident, TyNameSpace>,
    pub value_names: HashMap<Ident, ValueNameSpace>,
    pub current_module: Path,
    pub stacks: Vec<HashMap<Ident, Variable>>,
}

impl SemanticAnalyzer {
    fn get_builtin() -> (HashMap<Ident, TyNameSpace>, HashMap<Ident, ValueNameSpace>) {
        let type_names = HashMap::from([
            (Ident::String("bool".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("i32".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("u32".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("isize".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("usize".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("char".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("str".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("String".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("Copy".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("Box".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("Option".to_owned()), TyNameSpace::BuiltIn),
            (Ident::String("Result".to_owned()), TyNameSpace::BuiltIn),
        ]);
        let value_names = HashMap::from([
            (
                Ident::String("print".to_owned()),
                ValueNameSpace::Fn(FnSig {
                    decl: Box::new(FnDecl {
                        inputs: vec![Param {
                            ty: Box::new(Ty {
                                kind: TyKind::Ref(RefTy(MutTy {
                                    ty: Box::new(Ty {
                                        kind: TyKind::Path(PathTy(
                                            None,
                                            Path {
                                                segments: vec![PathSegment {
                                                    ident: Ident::String("str".to_owned()),
                                                    args: None,
                                                }],
                                            },
                                        )),
                                    }),
                                    mutbl: Mutability::Not,
                                })),
                            }),
                            pat: Box::new(Pat {
                                kind: PatKind::Ident(IdentPat(
                                    BindingMode(crate::ast::ByRef::No, Mutability::Not),
                                    Ident::String("s".to_owned()),
                                    None,
                                )),
                            }),
                        }],
                        output: FnRetTy::Ty(Box::new(Ty {
                            kind: TyKind::Tup(TupTy(vec![])),
                        })),
                    }),
                }),
            ),
            (
                Ident::String("println".to_owned()),
                ValueNameSpace::Fn(FnSig {
                    decl: Box::new(FnDecl {
                        inputs: vec![Param {
                            ty: Box::new(Ty {
                                kind: TyKind::Ref(RefTy(MutTy {
                                    ty: Box::new(Ty {
                                        kind: TyKind::Path(PathTy(
                                            None,
                                            Path {
                                                segments: vec![PathSegment {
                                                    ident: Ident::String("str".to_owned()),
                                                    args: None,
                                                }],
                                            },
                                        )),
                                    }),
                                    mutbl: Mutability::Not,
                                })),
                            }),
                            pat: Box::new(Pat {
                                kind: PatKind::Ident(IdentPat(
                                    BindingMode(crate::ast::ByRef::No, Mutability::Not),
                                    Ident::String("s".to_owned()),
                                    None,
                                )),
                            }),
                        }],
                        output: FnRetTy::Ty(Box::new(Ty {
                            kind: TyKind::Tup(TupTy(vec![])),
                        })),
                    }),
                }),
            ),
            (
                Ident::String("printInt".to_owned()),
                ValueNameSpace::Fn(FnSig {
                    decl: Box::new(FnDecl {
                        inputs: vec![Param {
                            ty: Box::new(Ty {
                                kind: TyKind::Path(PathTy(
                                    None,
                                    Path {
                                        segments: vec![PathSegment {
                                            ident: Ident::String("i32".to_owned()),
                                            args: None,
                                        }],
                                    },
                                )),
                            }),
                            pat: Box::new(Pat {
                                kind: PatKind::Ident(IdentPat(
                                    BindingMode(crate::ast::ByRef::No, Mutability::Not),
                                    Ident::String("n".to_owned()),
                                    None,
                                )),
                            }),
                        }],
                        output: FnRetTy::Ty(Box::new(Ty {
                            kind: TyKind::Tup(TupTy(vec![])),
                        })),
                    }),
                }),
            ),
            (
                Ident::String("printlnInt".to_owned()),
                ValueNameSpace::Fn(FnSig {
                    decl: Box::new(FnDecl {
                        inputs: vec![Param {
                            ty: Box::new(Ty {
                                kind: TyKind::Path(PathTy(
                                    None,
                                    Path {
                                        segments: vec![PathSegment {
                                            ident: Ident::String("i32".to_owned()),
                                            args: None,
                                        }],
                                    },
                                )),
                            }),
                            pat: Box::new(Pat {
                                kind: PatKind::Ident(IdentPat(
                                    BindingMode(crate::ast::ByRef::No, Mutability::Not),
                                    Ident::String("n".to_owned()),
                                    None,
                                )),
                            }),
                        }],
                        output: FnRetTy::Ty(Box::new(Ty {
                            kind: TyKind::Tup(TupTy(vec![])),
                        })),
                    }),
                }),
            ),
            (
                Ident::String("getString".to_owned()),
                ValueNameSpace::Fn(FnSig {
                    decl: Box::new(FnDecl {
                        inputs: vec![],
                        output: FnRetTy::Ty(Box::new(Ty {
                            kind: TyKind::Path(PathTy(
                                None,
                                Path {
                                    segments: vec![PathSegment {
                                        ident: Ident::String("String".to_owned()),
                                        args: None,
                                    }],
                                },
                            )),
                        })),
                    }),
                }),
            ),
            (
                Ident::String("getInt".to_owned()),
                ValueNameSpace::Fn(FnSig {
                    decl: Box::new(FnDecl {
                        inputs: vec![],
                        output: FnRetTy::Ty(Box::new(Ty {
                            kind: TyKind::Path(PathTy(
                                None,
                                Path {
                                    segments: vec![PathSegment {
                                        ident: Ident::String("i32".to_owned()),
                                        args: None,
                                    }],
                                },
                            )),
                        })),
                    }),
                }),
            ),
            (
                Ident::String("exit".to_owned()),
                ValueNameSpace::Fn(FnSig {
                    decl: Box::new(FnDecl {
                        inputs: vec![Param {
                            ty: Box::new(Ty {
                                kind: TyKind::Path(PathTy(
                                    None,
                                    Path {
                                        segments: vec![PathSegment {
                                            ident: Ident::String("i32".to_owned()),
                                            args: None,
                                        }],
                                    },
                                )),
                            }),
                            pat: Box::new(Pat {
                                kind: PatKind::Ident(IdentPat(
                                    BindingMode(crate::ast::ByRef::No, Mutability::Not),
                                    Ident::String("code".to_owned()),
                                    None,
                                )),
                            }),
                        }],
                        output: FnRetTy::Ty(Box::new(Ty {
                            kind: TyKind::Tup(TupTy(vec![])),
                        })),
                    }),
                }),
            ),
        ]);

        (type_names, value_names)
    }

    pub fn new() -> Self {
        let (type_names, value_names) = Self::get_builtin();
        Self {
            type_names,
            value_names,
            current_module: Path::default(),
            stacks: Vec::default(),
        }
    }
}

impl Visitor for SemanticAnalyzer {
    fn visit_crate(&mut self, krate: &crate::ast::Crate) -> Result<(), SemanticError> {
        todo!()
    }

    fn visit_item(&mut self, item: &crate::ast::item::Item) -> Result<(), SemanticError> {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::stmt::Stmt) -> Result<(), SemanticError> {
        todo!()
    }

    fn visit_expr(&mut self, expr: &crate::ast::expr::Expr) -> Result<Ty, SemanticError> {
        todo!()
    }

    fn visit_pat(&mut self, pat: &crate::ast::pat::Pat) -> Result<Ty, SemanticError> {
        todo!()
    }
}
