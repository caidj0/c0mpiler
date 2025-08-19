pub mod const_eval;
pub mod visitor;

use std::{collections::HashMap, vec};

use crate::{
    ast::{
        Ident, Mutability,
        expr::Expr,
        item::{AssocItemKind, ConstItem, EnumItem, FnItem, FnRetTy},
        ty::{MutTy, RefTy, Ty, TyKind},
    },
    semantics::{const_eval::ConstEvalError, visitor::Visitor},
    tokens::TokenType,
};

#[derive(Debug)]
pub enum SemanticError {
    Unimplemented,
    UndefinedScope,
    MultiDefined,
    ConstEvalError(ConstEvalError),
    InvaildPath,
    UnknownType,
    UnknownVariable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedTy {
    BulitIn(Ident),
    Named(Vec<Ident>),
    Ref(Box<ResolvedTy>, Mutability),
    Array(Box<ResolvedTy>, u32),
    Slice(Box<ResolvedTy>),
    Tup(Vec<ResolvedTy>),
    Fn(Vec<ResolvedTy>, Box<ResolvedTy>),
    Infer,
}

#[derive(Debug, Default)]
pub struct TypeTable {
    entries: Vec<Box<ResolvedTy>>,
    map: HashMap<ResolvedTy, TypeId>,
}

impl TypeTable {
    pub fn intern(&mut self, ty: ResolvedTy) -> TypeId {
        if let Some(&id) = self.map.get(&ty) {
            return id;
        }
        let id = TypeId(self.entries.len());
        self.entries.push(Box::new(ty.clone()));
        self.map.insert(ty, id);
        id
    }

    pub fn get(&self, id: TypeId) -> &ResolvedTy {
        &self.entries[id.0]
    }
}

#[derive(Debug)]
pub struct TypeInfo {
    pub name: Ident,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub struct ImplInfo {
    pub self_ty: TypeId,
    pub for_trait: Option<Vec<Ident>>,
    pub methods: Vec<FnSig>,
}

#[derive(Debug)]
pub struct FnSig {
    pub name: Ident,
    pub params: Vec<TypeId>,
    pub ret: TypeId,
    pub is_placeholder: bool,
}

#[derive(Debug)]
pub struct Constant {
    pub name: Ident,
    pub ty: TypeId,
    pub is_placeholder: bool,
}

#[derive(Debug)]
pub enum TypeKind {
    Placeholder,
    Struct {
        fields: Vec<(Ident, TypeId)>,
    },
    Enum {
        fields: Vec<Ident>,
    },
    Trait {
        methods: Vec<FnSig>,
        constants: Vec<Constant>,
    },
}

#[derive(Debug)]
pub struct Variable {
    pub ty: TypeId,
    pub mutbl: Mutability,
}

#[derive(Debug, Default)]
pub struct Scope {
    pub ident: String,
    pub types: HashMap<Ident, TypeInfo>,
    pub values: HashMap<Ident, Variable>,
}

#[derive(Debug)]
pub enum AnalyzeStage {
    SymbolCollect,
    Definition,
    Body,
    Done,
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    layers: Vec<Scope>,
    type_table: TypeTable,
    impls: HashMap<TypeId, Vec<ImplInfo>>,
    root_scope: Scope,
    stage: AnalyzeStage,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            layers: Vec::default(),
            type_table: TypeTable::default(),
            impls: HashMap::default(),
            root_scope: Scope::default(),
            stage: AnalyzeStage::SymbolCollect,
        }
    }

    fn get_scope(&mut self) -> Result<&mut Scope, SemanticError> {
        if let Some(scope) = self.layers.last_mut() {
            Ok(scope)
        } else {
            Ok(&mut self.root_scope)
        }
    }

    fn add_type(&mut self, ident: Ident, info: TypeInfo) -> Result<(), SemanticError> {
        let s = self.get_scope()?;

        if s.types.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.types.insert(ident, info);

        Ok(())
    }

    fn get_type(&mut self, ident: &Ident) -> Result<&mut TypeInfo, SemanticError> {
        let s = self.get_scope()?;
        s.types.get_mut(ident).ok_or(SemanticError::UnknownType)
    }

    fn add_value(&mut self, ident: Ident, var: Variable) -> Result<(), SemanticError> {
        let s = self.get_scope()?;

        if s.values.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.values.insert(ident, var);

        Ok(())
    }

    fn get_value(&mut self, ident: &Ident) -> Result<&mut Variable, SemanticError> {
        let s = self.get_scope()?;
        s.values
            .get_mut(ident)
            .ok_or(SemanticError::UnknownVariable)
    }

    pub fn visit(&mut self, krate: &crate::ast::Crate) -> Result<(), SemanticError> {
        self.stage = AnalyzeStage::SymbolCollect;
        self.visit_crate(krate)?;
        self.stage = AnalyzeStage::Definition;
        self.visit_crate(krate)?;
        Ok(())
    }

    fn get_layer_name(&self) -> String {
        let mut ret = "".to_string();

        for x in &self.layers {
            ret.push_str(&x.ident);
        }

        ret
    }

    fn is_builtin_type(&self, ident: &Ident) -> bool {
        match ident {
            Ident::Empty => false,
            Ident::String(s) => {
                s == "u32" || s == "i32" || s == "char" || s == "str" || s == "String"
            }
            Ident::PathSegment(_) => false,
        }
    }

    fn resolve_ty(&self, ty: &Ty) -> Result<ResolvedTy, SemanticError> {
        match &ty.kind {
            TyKind::Slice(slice_ty) => {
                Ok(ResolvedTy::Slice(Box::new(self.resolve_ty(&slice_ty.0)?)))
            }
            TyKind::Array(array_ty) => Ok(ResolvedTy::Array(
                Box::new(self.resolve_ty(&array_ty.0)?),
                array_ty.1.value.as_ref().try_into()?,
            )),
            TyKind::Ref(RefTy(MutTy { ty: ty2, mutbl })) => {
                Ok(ResolvedTy::Ref(Box::new(self.resolve_ty(ty2)?), *mutbl))
            }
            TyKind::Tup(tup_ty) => Ok(ResolvedTy::Tup(
                tup_ty
                    .0
                    .iter()
                    .map(|x| self.resolve_ty(x))
                    .collect::<Result<Vec<_>, SemanticError>>()?,
            )),
            TyKind::Path(path_ty) => {
                if path_ty.0.is_some() {
                    return Err(SemanticError::Unimplemented);
                }

                let mut scope = None;
                let mut id: Ident = Ident::Empty;

                for (index, seg) in path_ty.1.segments.iter().enumerate() {
                    match &seg.ident {
                        Ident::Empty => return Err(SemanticError::InvaildPath),
                        Ident::String(s) => {
                            if matches!(id, Ident::Empty) {
                                id = Ident::String(s.to_string());
                            } else {
                                return Err(SemanticError::InvaildPath);
                            }
                        }
                        Ident::PathSegment(token_type) => {
                            if index != 0 {
                                return Err(SemanticError::InvaildPath);
                            } else {
                                match token_type {
                                    TokenType::LSelfType => scope = Some(&self.root_scope),
                                    TokenType::SelfType => {
                                        todo!() // TODO
                                    }
                                    TokenType::Crate => scope = Some(&self.root_scope),
                                    TokenType::Super => return Err(SemanticError::InvaildPath),
                                    _ => {
                                        panic!("The path in the ident can't be {:?}", token_type);
                                    }
                                }
                            }
                        }
                    }
                }

                // scope 要么为 None，要么为 root
                match scope {
                    Some(scope) => {
                        assert!(std::ptr::eq(scope, &self.root_scope));
                        if scope.types.contains_key(&id) {
                            Ok(ResolvedTy::Named(vec![id]))
                        } else {
                            return Err(SemanticError::UnknownType);
                        }
                    }
                    None => {
                        let mut found_index: Option<usize> = None;
                        for (index, scope) in self.layers.iter().enumerate().rev() {
                            if scope.types.contains_key(&id) {
                                found_index = Some(index);
                                break;
                            }
                        }

                        match found_index {
                            Some(idx) => {
                                let mut names: Vec<Ident> = Vec::new();
                                for i in 0..=idx {
                                    names.push(Ident::String(self.layers[i].ident.clone()));
                                }
                                names.push(id);
                                Ok(ResolvedTy::Named(names))
                            }
                            None => {
                                if self.root_scope.types.contains_key(&id) {
                                    Ok(ResolvedTy::Named(vec![id]))
                                } else {
                                    if self.is_builtin_type(&id) {
                                        Ok(ResolvedTy::BulitIn(id))
                                    } else {
                                        Err(SemanticError::UnknownType)
                                    }
                                }
                            }
                        }
                    }
                }
            }
            TyKind::TraitObject(_) => Err(SemanticError::Unimplemented),
            TyKind::ImplTrait(_) => Err(SemanticError::Unimplemented),
            TyKind::Infer(_) => Ok(ResolvedTy::Infer),
            TyKind::ImplicitSelf => todo!(),
        }
    }

    fn check_const(&self, ty: &ResolvedTy, expr: &Expr) -> Result<(), SemanticError> {
        if let ResolvedTy::BulitIn(ident) = ty {
            if let Ident::String(s) = ident {
                match s.as_str() {
                    "u32" => {
                        let _: u32 = expr.try_into()?;
                        return Ok(());
                    }
                    _ => {}
                }
            }
        };

        return Err(SemanticError::ConstEvalError(
            ConstEvalError::NotSupportedExpr,
        ));
    }
}

impl Visitor for SemanticAnalyzer {
    fn visit_crate(&mut self, krate: &crate::ast::Crate) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                for item in &krate.items {
                    self.visit_item(item)?
                }
            }
            AnalyzeStage::Definition => {
                for item in &krate.items {
                    self.visit_item(item)?
                }
            }
            AnalyzeStage::Body => todo!(),
            AnalyzeStage::Done => todo!(),
        }
        Ok(())
    }

    fn visit_item(&mut self, item: &crate::ast::item::Item) -> Result<(), SemanticError> {
        match &item.kind {
            crate::ast::item::ItemKind::Const(ConstItem { ident, ty, expr }) => match self.stage {
                AnalyzeStage::SymbolCollect => {}
                AnalyzeStage::Definition => {
                    let ty = self.resolve_ty(&ty)?;
                    if let Some(e) = expr {
                        self.check_const(&ty, e)?;
                    }
                    let tyid = self.type_table.intern(ty);
                    self.add_value(
                        ident.clone(),
                        Variable {
                            ty: tyid,
                            mutbl: Mutability::Not,
                        },
                    )?;
                }
                AnalyzeStage::Body => todo!(),
                AnalyzeStage::Done => todo!(),
            },

            crate::ast::item::ItemKind::Fn(fn_item) => match self.stage {
                AnalyzeStage::SymbolCollect => {}
                AnalyzeStage::Definition => {
                    let param_tys = fn_item
                        .sig
                        .decl
                        .inputs
                        .iter()
                        .map(|x| self.resolve_ty(&x.ty))
                        .collect::<Result<Vec<_>, SemanticError>>()?;
                    let ret_ty = match &fn_item.sig.decl.output {
                        FnRetTy::Default => ResolvedTy::Tup(Vec::new()),
                        FnRetTy::Ty(ty) => self.resolve_ty(&ty)?,
                    };
                    let tyid = self
                        .type_table
                        .intern(ResolvedTy::Fn(param_tys, Box::new(ret_ty)));
                    self.add_value(
                        fn_item.ident.clone(),
                        Variable {
                            ty: tyid,
                            mutbl: Mutability::Not,
                        },
                    )?;
                }
                AnalyzeStage::Body => todo!(),
                AnalyzeStage::Done => todo!(),
            },

            crate::ast::item::ItemKind::Mod(_) => return Err(SemanticError::Unimplemented),

            crate::ast::item::ItemKind::Enum(EnumItem(ident, _, variants)) => match self.stage {
                AnalyzeStage::SymbolCollect => self.add_type(
                    ident.clone(),
                    TypeInfo {
                        name: ident.clone(),
                        kind: TypeKind::Placeholder,
                    },
                )?,
                AnalyzeStage::Definition => {
                    let fields = variants
                        .iter()
                        .map(|x| {
                            if !matches!(x.data, crate::ast::item::VariantData::Unit)
                                || x.disr_expr.is_some()
                            {
                                Err(SemanticError::Unimplemented)
                            } else {
                                Ok(x.ident.clone())
                            }
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?;
                    self.get_type(ident)?.kind = TypeKind::Enum { fields };
                }
                AnalyzeStage::Body => todo!(),
                AnalyzeStage::Done => todo!(),
            },

            crate::ast::item::ItemKind::Struct(crate::ast::item::StructItem(
                ident,
                _,
                variant_data,
            )) => match self.stage {
                AnalyzeStage::SymbolCollect => self.add_type(
                    ident.clone(),
                    TypeInfo {
                        name: ident.clone(),
                        kind: TypeKind::Struct { fields: Vec::new() },
                    },
                )?,
                AnalyzeStage::Definition => {
                    let fields = match variant_data {
                        crate::ast::item::VariantData::Struct { fields } => fields
                            .iter()
                            .map(|x| {
                                Ok((
                                    x.ident.clone().ok_or(SemanticError::Unimplemented)?,
                                    self.type_table.intern(self.resolve_ty(&x.ty)?),
                                ))
                            })
                            .collect::<Result<Vec<_>, SemanticError>>()?,
                        crate::ast::item::VariantData::Tuple(_) => {
                            return Err(SemanticError::Unimplemented);
                        }
                        crate::ast::item::VariantData::Unit => Vec::new(),
                    };
                    self.get_type(ident)?.kind = TypeKind::Struct { fields }
                }
                AnalyzeStage::Body => todo!(),
                AnalyzeStage::Done => todo!(),
            },

            crate::ast::item::ItemKind::Trait(crate::ast::item::TraitItem {
                ident,
                generics: _,
                bounds: _,
                items,
            }) => match self.stage {
                AnalyzeStage::SymbolCollect => self.add_type(
                    ident.clone(),
                    TypeInfo {
                        name: ident.clone(),
                        kind: TypeKind::Placeholder,
                    },
                )?,
                AnalyzeStage::Definition => {
                    let mut methods = Vec::new();
                    let mut constants = Vec::new();
                    for item in items {
                        match &item.kind {
                            AssocItemKind::Const(ConstItem { ident, ty, expr }) => {
                                let resloved_ty = self.resolve_ty(&ty)?;
                                if let Some(e) = expr {
                                    self.check_const(&resloved_ty, e)?;
                                }
                                let tyid = self.type_table.intern(resloved_ty);
                                constants.push(Constant {
                                    name: ident.clone(),
                                    ty: tyid,
                                    is_placeholder: expr.is_none(),
                                })
                            }
                            AssocItemKind::Fn(FnItem {
                                ident,
                                generics: _,
                                sig,
                                body,
                            }) => {
                                let param_tys = sig
                                    .decl
                                    .inputs
                                    .iter()
                                    .map(|x| self.resolve_ty(&x.ty))
                                    .collect::<Result<Vec<_>, SemanticError>>()?;
                                let ret_ty = match &sig.decl.output {
                                    FnRetTy::Default => ResolvedTy::Tup(Vec::new()),
                                    FnRetTy::Ty(ty) => self.resolve_ty(&ty)?,
                                };
                                let params_id = param_tys
                                    .into_iter()
                                    .map(|x| self.type_table.intern(x))
                                    .collect();
                                let ret_id = self.type_table.intern(ret_ty);
                                methods.push(FnSig {
                                    name: ident.clone(),
                                    params: params_id,
                                    ret: ret_id,
                                    is_placeholder: body.is_none(),
                                });
                            }
                        }
                    }
                    self.get_type(ident)?.kind = TypeKind::Trait { methods, constants };
                }
                AnalyzeStage::Body => todo!(),
                AnalyzeStage::Done => todo!(),
            },

            crate::ast::item::ItemKind::Impl(impl_item) => todo!(),
        }
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::stmt::Stmt) -> Result<(), SemanticError> {
        todo!()
    }

    fn visit_expr(&mut self, expr: &crate::ast::expr::Expr) -> Result<ResolvedTy, SemanticError> {
        todo!()
    }

    fn visit_pat(&mut self, pat: &crate::ast::pat::Pat) -> Result<ResolvedTy, SemanticError> {
        todo!()
    }
}
