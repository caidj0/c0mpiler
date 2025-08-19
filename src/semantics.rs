pub mod const_eval;
pub mod primitives;
pub mod visitor;

use std::{collections::HashMap, vec};

use crate::{
    ast::{
        Ident, Mutability, Symbol,
        expr::Expr,
        item::{AssocItemKind, ConstItem, EnumItem, FnItem, FnRetTy, StructItem, TraitItem},
        stmt::StmtKind,
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
    InvaildScope,
    FnWithoutBody,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedTy {
    BulitIn(Symbol),
    Named(Vec<Symbol>),
    Ref(Box<ResolvedTy>, Mutability),
    Array(Box<ResolvedTy>, u32),
    Slice(Box<ResolvedTy>),
    Tup(Vec<ResolvedTy>),
    Fn(Vec<ResolvedTy>, Box<ResolvedTy>),
    Infer,
}

impl ResolvedTy {
    pub fn unit() -> Self {
        Self::Tup(vec![])
    }
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
pub enum VariableKind {
    Decl,
    Inited,
    Const,
}

#[derive(Debug)]
pub struct Variable {
    pub ty: TypeId,
    pub mutbl: Mutability,
    pub kind: VariableKind,
}

#[derive(Debug)]
pub enum ScopeKind {
    Root,
    Fn,
    Trait(TypeId),
    Enum,
    Struct,
    Impl(TypeId),
}

#[derive(Debug)]
pub struct Scope {
    pub ident: String,
    pub kind: ScopeKind,
    pub types: HashMap<Symbol, TypeInfo>,
    pub values: HashMap<Symbol, Variable>,
}

#[derive(Debug)]
pub enum AnalyzeStage {
    SymbolCollect,
    Definition,
    Body,
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
        let mut type_table: TypeTable = TypeTable::default();
        type_table.intern(ResolvedTy::unit());

        Self {
            layers: Vec::default(),
            type_table,
            impls: HashMap::default(),
            root_scope: Scope {
                ident: String::default(),
                kind: ScopeKind::Root,
                types: HashMap::default(),
                values: HashMap::default(),
            },
            stage: AnalyzeStage::SymbolCollect,
        }
    }

    fn get_scope_mut(&mut self) -> Result<&mut Scope, SemanticError> {
        if let Some(scope) = self.layers.last_mut() {
            Ok(scope)
        } else {
            Ok(&mut self.root_scope)
        }
    }

    fn get_scope(&self) -> Result<&Scope, SemanticError> {
        if let Some(scope) = self.layers.last() {
            Ok(scope)
        } else {
            Ok(&self.root_scope)
        }
    }

    fn push_scope(&mut self, ident: Symbol, kind: ScopeKind) -> Result<(), SemanticError> {
        let name = match ident {
            Symbol::String(s) => {
                match kind {
                    ScopeKind::Root => "",
                    ScopeKind::Fn => "$Fn$",
                    ScopeKind::Trait(_) => "$Trait$",
                    ScopeKind::Enum => "$Enum$",
                    ScopeKind::Struct => "$Struct$",
                    ScopeKind::Impl(_) => "$Impl$",
                }
                .to_string()
                    + &s
            }
            _ => return Err(SemanticError::InvaildScope),
        };

        self.layers.push(Scope {
            ident: name,
            kind,
            types: HashMap::default(),
            values: HashMap::default(),
        });
        Ok(())
    }

    fn pop_scope(&mut self) -> Result<(), SemanticError> {
        if let Some(_) = self.layers.pop() {
            Err(SemanticError::InvaildScope)
        } else {
            Ok(())
        }
    }

    fn add_type(&mut self, ident: Symbol, info: TypeInfo) -> Result<(), SemanticError> {
        let s = self.get_scope_mut()?;

        if s.types.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.types.insert(ident, info);

        Ok(())
    }

    fn get_type(&mut self, ident: &Symbol) -> Result<&mut TypeInfo, SemanticError> {
        let s = self.get_scope_mut()?;
        s.types.get_mut(ident).ok_or(SemanticError::UnknownType)
    }

    fn add_value(&mut self, ident: Symbol, var: Variable) -> Result<(), SemanticError> {
        let s = self.get_scope_mut()?;

        if s.values.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.values.insert(ident, var);

        Ok(())
    }

    fn get_value(&mut self, ident: &Symbol) -> Result<&mut Variable, SemanticError> {
        let s = self.get_scope_mut()?;
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

    fn is_builtin_type(&self, ident: &Symbol) -> bool {
        match ident {
            Symbol::Empty => false,
            Symbol::String(s) => {
                s == "u32" || s == "i32" || s == "char" || s == "str" || s == "String"
            }
            Symbol::PathSegment(_) => false,
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
                let mut id = Symbol::Empty;

                for (index, seg) in path_ty.1.segments.iter().enumerate() {
                    match &seg.ident {
                        Ident {
                            symbol: Symbol::Empty,
                            span: _,
                        } => return Err(SemanticError::InvaildPath),
                        Ident {
                            symbol: Symbol::String(s),
                            span: _,
                        } => {
                            if matches!(id, Symbol::Empty) {
                                // id 应该只能被赋值一次
                                id = Symbol::String(s.to_string());
                            } else {
                                return Err(SemanticError::InvaildPath);
                            }
                        }
                        Ident {
                            symbol: Symbol::PathSegment(token_type),
                            span: _,
                        } => {
                            if index != 0 {
                                return Err(SemanticError::InvaildPath);
                            } else {
                                match token_type {
                                    TokenType::LSelfType => scope = Some(&self.root_scope),
                                    TokenType::SelfType => {
                                        if path_ty.1.segments.len() == 1
                                            && let Scope {
                                                ident: _,
                                                kind: ScopeKind::Impl(id) | ScopeKind::Trait(id),
                                                types: _,
                                                values: _,
                                            } = self.get_scope()?
                                        {
                                            return Ok(self.get_type_by_id(*id).clone());
                                        } else {
                                            return Err(SemanticError::InvaildPath);
                                        }
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
                                let mut names = Vec::new();
                                for i in 0..=idx {
                                    names.push(Symbol::String(self.layers[i].ident.clone()));
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

    fn intern_type(&mut self, ty: ResolvedTy) -> TypeId {
        self.type_table.intern(ty)
    }

    fn get_type_by_id(&self, id: TypeId) -> &ResolvedTy {
        self.type_table.get(id)
    }

    fn unit_type() -> TypeId {
        TypeId(0)
    }

    fn check_const(&self, ty: &ResolvedTy, expr: &Expr) -> Result<(), SemanticError> {
        if let ResolvedTy::BulitIn(ident) = ty {
            if let Symbol::String(s) = ident {
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
    fn visit_const_item(
        &mut self,
        ConstItem { ident, ty, expr }: &ConstItem,
    ) -> Result<(), SemanticError> {
        // const item 只需在 Definition 阶段收集和评估即可
        match self.stage {
            AnalyzeStage::SymbolCollect => {}
            AnalyzeStage::Definition => {
                let ty = self.resolve_ty(&ty)?;
                if let Some(e) = expr {
                    self.check_const(&ty, e)?;
                }
                let tyid = self.intern_type(ty);
                self.add_value(
                    ident.symbol.clone(),
                    Variable {
                        ty: tyid,
                        mutbl: Mutability::Not,
                        kind: VariableKind::Const,
                    },
                )?;
            }
            AnalyzeStage::Body => {}
        }

        Ok(())
    }

    fn visit_fn_item(
        &mut self,
        FnItem {
            ident,
            generics: _,
            sig,
            body,
        }: &FnItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {}
            AnalyzeStage::Definition => {
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
                let tyid = self.intern_type(ResolvedTy::Fn(param_tys, Box::new(ret_ty)));
                self.add_value(
                    ident.symbol.clone(),
                    Variable {
                        ty: tyid,
                        mutbl: Mutability::Not,
                        kind: VariableKind::Const,
                    },
                )?;
            }
            AnalyzeStage::Body => {
                match body {
                    Some(body) => {
                        self.push_scope(ident.symbol.clone(), ScopeKind::Fn)?;

                        for stage in [
                            AnalyzeStage::SymbolCollect,
                            AnalyzeStage::Definition,
                            AnalyzeStage::Body,
                        ] {
                            self.stage = stage;
                            for stmt in &body.stmts {
                                self.visit_stmt(stmt)?;
                            }
                        }

                        // Do something...

                        self.pop_scope()?;
                    }
                    None => {
                        let scope = self.get_scope_mut()?;
                        match scope.kind {
                            ScopeKind::Root | ScopeKind::Fn => {
                                return Err(SemanticError::FnWithoutBody);
                            }
                            ScopeKind::Trait(_) => {}
                            ScopeKind::Impl(_) => todo!(), // TODO
                            ScopeKind::Enum | ScopeKind::Struct => panic!("Impossible condition!"),
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_mod_item(&mut self, _: &crate::ast::item::ModItem) -> Result<(), SemanticError> {
        Err(SemanticError::Unimplemented)
    }

    fn visit_enum_item(
        &mut self,
        EnumItem(ident, _, variants): &EnumItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => self.add_type(
                ident.symbol.clone(),
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
                self.get_type(&ident.symbol)?.kind = TypeKind::Enum { fields };
            }
            AnalyzeStage::Body => todo!(),
        }
        Ok(())
    }

    fn visit_struct_item(
        &mut self,
        StructItem(ident, _, variant_data): &crate::ast::item::StructItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => self.add_type(
                ident.symbol.clone(),
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
                                self.intern_type(self.resolve_ty(&x.ty)?),
                            ))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?,
                    crate::ast::item::VariantData::Tuple(_) => {
                        return Err(SemanticError::Unimplemented);
                    }
                    crate::ast::item::VariantData::Unit => Vec::new(),
                };
                self.get_type(&ident.symbol)?.kind = TypeKind::Struct { fields }
            }
            AnalyzeStage::Body => todo!(),
        }
        Ok(())
    }

    fn visit_trait_item(
        &mut self,
        TraitItem {
            ident,
            generics: _,
            bounds: _,
            items,
        }: &crate::ast::item::TraitItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => self.add_type(
                ident.symbol.clone(),
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
                            let tyid = self.intern_type(resloved_ty);
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
                            let params_id =
                                param_tys.into_iter().map(|x| self.intern_type(x)).collect();
                            let ret_id = self.intern_type(ret_ty);
                            methods.push(FnSig {
                                name: ident.clone(),
                                params: params_id,
                                ret: ret_id,
                                is_placeholder: body.is_none(),
                            });
                        }
                    }
                }
                self.get_type(&ident.symbol)?.kind = TypeKind::Trait { methods, constants };
            }
            AnalyzeStage::Body => todo!(),
        }
        Ok(())
    }

    fn visit_impl_item(&mut self, _: &crate::ast::item::ImplItem) -> Result<(), SemanticError> {
        Err(SemanticError::Unimplemented)
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::stmt::Stmt) -> Result<TypeId, SemanticError> {
        match &stmt.kind {
            StmtKind::Let(local_stmt) => {
                self.visit_let_stmt(local_stmt)?;
                Ok(Self::unit_type())
            }
            StmtKind::Item(item) => {
                self.visit_item(item)?;
                Ok(Self::unit_type())
            }
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Semi(expr) => self.visit_expr(expr).map(|_| Self::unit_type()),
            StmtKind::Empty(_) => Ok(Self::unit_type()),
        }
    }

    fn visit_let_stmt(&mut self, stmt: &crate::ast::stmt::LocalStmt) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {}
            AnalyzeStage::Definition => {
                // TODO: 先做 Expr
                todo!()
            }
            AnalyzeStage::Body => {}
        }
        Ok(())
    }

    fn visit_array_expr(
        &mut self,
        expr: &crate::ast::expr::ArrayExpr,
    ) -> Result<TypeId, SemanticError> {
        let type_ids = expr
            .0
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<_>, SemanticError>>()?;

        todo!()
    }

    fn visit_const_block_expr(
        &mut self,
        expr: &crate::ast::expr::ConstBlockExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_call_expr(
        &mut self,
        expr: &crate::ast::expr::CallExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_method_call_expr(
        &mut self,
        expr: &crate::ast::expr::MethodCallExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_tup_expr(
        &mut self,
        expr: &crate::ast::expr::TupExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_binary_expr(
        &mut self,
        expr: &crate::ast::expr::BinaryExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_unary_expr(
        &mut self,
        expr: &crate::ast::expr::UnaryExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_lit_expr(
        &mut self,
        expr: &crate::ast::expr::LitExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_cast_expr(
        &mut self,
        expr: &crate::ast::expr::CastExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_let_expr(
        &mut self,
        expr: &crate::ast::expr::LetExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_if_expr(&mut self, expr: &crate::ast::expr::IfExpr) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_while_expr(
        &mut self,
        expr: &crate::ast::expr::WhileExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_for_loop_expr(
        &mut self,
        expr: &crate::ast::expr::ForLoopExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_loop_expr(
        &mut self,
        expr: &crate::ast::expr::LoopExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_match_expr(
        &mut self,
        expr: &crate::ast::expr::MatchExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_block_expr(
        &mut self,
        expr: &crate::ast::expr::BlockExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_assign_expr(
        &mut self,
        expr: &crate::ast::expr::AssignExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_assign_op_expr(
        &mut self,
        expr: &crate::ast::expr::AssignOpExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_field_expr(
        &mut self,
        expr: &crate::ast::expr::FieldExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_index_expr(
        &mut self,
        expr: &crate::ast::expr::IndexExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_range_expr(
        &mut self,
        expr: &crate::ast::expr::RangeExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_underscore_expr(
        &mut self,
        expr: &crate::ast::expr::UnderscoreExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_path_expr(
        &mut self,
        expr: &crate::ast::expr::PathExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_addr_of_expr(
        &mut self,
        expr: &crate::ast::expr::AddrOfExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_break_expr(
        &mut self,
        expr: &crate::ast::expr::BreakExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_continue_expr(
        &mut self,
        expr: &crate::ast::expr::ContinueExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_ret_expr(
        &mut self,
        expr: &crate::ast::expr::RetExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_struct_expr(
        &mut self,
        expr: &crate::ast::expr::StructExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_repeat_expr(
        &mut self,
        expr: &crate::ast::expr::RepeatExpr,
    ) -> Result<TypeId, SemanticError> {
        todo!()
    }

    fn visit_pat(&mut self, pat: &crate::ast::pat::Pat) -> Result<TypeId, SemanticError> {
        todo!()
    }
}
