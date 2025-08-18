pub mod visitor;

use std::collections::HashMap;

use crate::{
    ast::{Ident, Mutability},
    semantics::visitor::Visitor,
};

#[derive(Debug)]
pub enum SemanticError {
    Unimplemented,
    UndefinedScope,
    MultiDefined,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedTy {
    BulitIn(Ident),
    Named(Vec<Ident>),
    Ref(Box<ResolvedTy>, Mutability),
    Array(Box<ResolvedTy>, usize),
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
    pub ret: Option<TypeId>,
}

#[derive(Debug)]
pub enum TypeKind {
    Struct {
        fileds: Vec<(Ident, TypeId)>,
        methods: Vec<FnSig>,
    },
    Enum {
        fileds: Vec<Ident>,
    },
    Trait {
        methods: Vec<FnSig>,
    },
}

#[derive(Debug)]
pub struct Variable {
    pub ty: TypeId,
    pub mutbl: Mutability,
}

#[derive(Debug, Default)]
pub struct Scope {
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
    path: Vec<Ident>,
    impls: HashMap<TypeId, Vec<ImplInfo>>,
    root_scope: Scope,
    stage: AnalyzeStage,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            layers: Vec::default(),
            type_table: TypeTable::default(),
            path: Vec::default(),
            impls: HashMap::default(),
            root_scope: Scope::default(),
            stage: AnalyzeStage::SymbolCollect,
        }
    }

    fn get_scope(&mut self, scope_id: Option<Vec<Ident>>) -> Result<&mut Scope, SemanticError> {
        match scope_id {
            // 目前只有根 module，因此 scope 只有一个，剩下的都应该定义在 layers 中
            Some(id) => {
                if id.len() != 0 {
                    return Err(SemanticError::UndefinedScope);
                }
                Ok(&mut self.root_scope)
            }
            None => self
                .layers
                .last_mut()
                .map(Ok)
                .unwrap_or(Err(SemanticError::UndefinedScope)),
        }
    }

    fn add_type(
        &mut self,
        ident: Ident,
        info: TypeInfo,
        scope_id: Option<Vec<Ident>>,
    ) -> Result<(), SemanticError> {
        let s = self.get_scope(scope_id)?;

        if s.types.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.types.insert(ident, info);

        Ok(())
    }

    fn add_value(
        &mut self,
        ident: Ident,
        var: Variable,
        scope_id: Option<Vec<Ident>>,
    ) -> Result<(), SemanticError> {
        let s = self.get_scope(scope_id)?;

        if s.values.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.values.insert(ident, var);

        Ok(())
    }

    pub fn visit(&mut self, krate: &crate::ast::Crate) -> Result<(), SemanticError> {
        self.stage = AnalyzeStage::SymbolCollect;
        self.visit_crate(krate)?;
        Ok(())
    }

    pub fn is_local_path(&self) -> bool {
        for x in &self.path {
            match x {
                Ident::String(s) => {
                    if s.starts_with("$") {
                        return true;
                    }
                }
                _ => panic!("The path of analyzer should always be String type!"),
            }
        }

        return false;
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
            AnalyzeStage::Definition => todo!(),
            AnalyzeStage::Body => todo!(),
            AnalyzeStage::Done => todo!(),
        }
        Ok(())
    }

    fn visit_item(&mut self, item: &crate::ast::item::Item) -> Result<(), SemanticError> {
        match (&self.stage, &item.kind) {
            (AnalyzeStage::SymbolCollect, crate::ast::item::ItemKind::Const(const_item)) => todo!(),
            (AnalyzeStage::SymbolCollect, crate::ast::item::ItemKind::Fn(fn_item)) => todo!(),
            (AnalyzeStage::SymbolCollect, crate::ast::item::ItemKind::Mod(mod_item)) => todo!(),
            (AnalyzeStage::SymbolCollect, crate::ast::item::ItemKind::Enum(enum_item)) => todo!(),
            (AnalyzeStage::SymbolCollect, crate::ast::item::ItemKind::Struct(struct_item)) => todo!(),
            (AnalyzeStage::SymbolCollect, crate::ast::item::ItemKind::Trait(trait_item)) => todo!(),
            (AnalyzeStage::SymbolCollect, crate::ast::item::ItemKind::Impl(impl_item)) => todo!(),
            (AnalyzeStage::Definition, crate::ast::item::ItemKind::Const(const_item)) => todo!(),
            (AnalyzeStage::Definition, crate::ast::item::ItemKind::Fn(fn_item)) => todo!(),
            (AnalyzeStage::Definition, crate::ast::item::ItemKind::Mod(mod_item)) => todo!(),
            (AnalyzeStage::Definition, crate::ast::item::ItemKind::Enum(enum_item)) => todo!(),
            (AnalyzeStage::Definition, crate::ast::item::ItemKind::Struct(struct_item)) => todo!(),
            (AnalyzeStage::Definition, crate::ast::item::ItemKind::Trait(trait_item)) => todo!(),
            (AnalyzeStage::Definition, crate::ast::item::ItemKind::Impl(impl_item)) => todo!(),
            (AnalyzeStage::Body, crate::ast::item::ItemKind::Const(const_item)) => todo!(),
            (AnalyzeStage::Body, crate::ast::item::ItemKind::Fn(fn_item)) => todo!(),
            (AnalyzeStage::Body, crate::ast::item::ItemKind::Mod(mod_item)) => todo!(),
            (AnalyzeStage::Body, crate::ast::item::ItemKind::Enum(enum_item)) => todo!(),
            (AnalyzeStage::Body, crate::ast::item::ItemKind::Struct(struct_item)) => todo!(),
            (AnalyzeStage::Body, crate::ast::item::ItemKind::Trait(trait_item)) => todo!(),
            (AnalyzeStage::Body, crate::ast::item::ItemKind::Impl(impl_item)) => todo!(),
            (AnalyzeStage::Done, crate::ast::item::ItemKind::Const(const_item)) => todo!(),
            (AnalyzeStage::Done, crate::ast::item::ItemKind::Fn(fn_item)) => todo!(),
            (AnalyzeStage::Done, crate::ast::item::ItemKind::Mod(mod_item)) => todo!(),
            (AnalyzeStage::Done, crate::ast::item::ItemKind::Enum(enum_item)) => todo!(),
            (AnalyzeStage::Done, crate::ast::item::ItemKind::Struct(struct_item)) => todo!(),
            (AnalyzeStage::Done, crate::ast::item::ItemKind::Trait(trait_item)) => todo!(),
            (AnalyzeStage::Done, crate::ast::item::ItemKind::Impl(impl_item)) => todo!(),
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
