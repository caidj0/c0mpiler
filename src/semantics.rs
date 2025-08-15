pub mod visitor;

use std::{collections::HashMap, iter::repeat_n, vec};

use crate::{
    ast::{
        BindingMode, Crate, Ident, Mutability,
        item::{ConstItem, FieldDef, FnDecl, FnItem, FnRetTy, FnSig, ItemKind, Param, TraitRef},
        pat::{IdentPat, Pat, PatKind},
        path::{Path, PathSegment},
        ty::{MutTy, PathTy, RefTy, TupTy, Ty, TyKind},
    },
    semantics::visitor::Visitor,
};

#[derive(Debug)]
pub enum SemanticError {
    Unimplemented,
    MultiDefined,
}

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
pub enum AnalyzeStage {
    TypeCollect,
    Done,
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub type_names: HashMap<Ident, TyNameSpace>,
    pub value_names: HashMap<Ident, ValueNameSpace>,
    pub current_module: Path,
    pub stacks: Vec<HashMap<Ident, Variable>>,
    pub stage: AnalyzeStage,
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
            type_names: HashMap::default(),
            value_names: HashMap::default(),
            current_module: Path::default(),
            stacks: Vec::default(),
            stage: AnalyzeStage::TypeCollect,
        }
    }

    pub fn parse(&mut self, krate: &Crate) -> Result<(), SemanticError> {
        self.visit_crate(krate)?;

        self.stage = AnalyzeStage::Done;

        Ok(())
    }

    fn add_type(&mut self, ident: Ident, ty: TyNameSpace) -> Result<(), SemanticError> {
        if self.type_names.get(&ident).is_some() {
            return Err(SemanticError::MultiDefined);
        }

        self.type_names.insert(ident, ty);
        Ok(())
    }

    fn add_value(&mut self, ident: Ident, v: ValueNameSpace) -> Result<(), SemanticError> {
        if self.value_names.get(&ident).is_some() {
            return Err(SemanticError::MultiDefined);
        }

        self.value_names.insert(ident, v);
        Ok(())
    }
}

impl Visitor for SemanticAnalyzer {
    fn visit_crate(&mut self, krate: &crate::ast::Crate) -> Result<(), SemanticError> {
        for x in &krate.items {
            let ident = match &x.kind {
                ItemKind::Const(const_item) => const_item.ident.clone(),
                ItemKind::Fn(fn_item) => fn_item.ident.clone(),
                ItemKind::Mod(_) => return Err(SemanticError::Unimplemented),
                ItemKind::Enum(enum_item) => enum_item.0.clone(),
                ItemKind::Struct(struct_item) => struct_item.0.clone(),
                ItemKind::Trait(trait_item) => trait_item.ident.clone(),
                ItemKind::Impl(_) => {
                    return Err(SemanticError::Unimplemented);
                }
            };
            self.current_module.segments.push(PathSegment {
                ident: ident,
                args: None,
            });
            self.visit_item(x.as_ref())?;
            self.current_module.segments.pop();
        }
        Ok(())
    }

    fn visit_item(&mut self, item: &crate::ast::item::Item) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::TypeCollect => match &item.kind {
                ItemKind::Const(const_item) => self.add_value(
                    const_item.ident.clone(),
                    ValueNameSpace::Const(ConstItem {
                        ident: const_item.ident.clone(),
                        ty: Box::new(Ty {
                            kind: TyKind::Dummy,
                        }),
                        expr: None,
                    }),
                )?,
                ItemKind::Fn(fn_item) => self.add_value(
                    fn_item.ident.clone(),
                    ValueNameSpace::Fn(FnSig {
                        decl: Box::new(FnDecl {
                            inputs: vec![],
                            output: FnRetTy::Default,
                        }),
                    }),
                )?,
                ItemKind::Mod(_) => return Err(SemanticError::Unimplemented),
                ItemKind::Enum(enum_item) => self.add_type(
                    enum_item.0.clone(),
                    TyNameSpace::Enum {
                        variants: vec![],
                        impls: vec![],
                    },
                )?,
                ItemKind::Struct(struct_item) => self.add_type(
                    struct_item.0.clone(),
                    TyNameSpace::Struct {
                        fields: vec![],
                        impls: vec![],
                    },
                )?,
                ItemKind::Trait(trait_item) => self.add_type(
                    trait_item.ident.clone(),
                    TyNameSpace::Trait {
                        methods: vec![],
                        consts: vec![],
                    },
                )?,
                ItemKind::Impl(_) => return Err(SemanticError::Unimplemented),
            },
            AnalyzeStage::Done => todo!(),
        }
        Ok(())
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
