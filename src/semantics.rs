pub mod const_eval;
pub mod primitives;
pub mod visitor;

use std::{
    collections::{HashMap, HashSet},
    vec,
};

use crate::{
    ast::{
        Crate, Ident, Mutability, NodeId, Symbol,
        expr::Expr,
        item::{
            AssocItemKind, ConstItem, EnumItem, FnItem, FnRetTy, ImplItem, Item, ItemKind,
            StructItem, TraitItem,
        },
        stmt::StmtKind,
        ty::{MutTy, PathTy, RefTy, Ty, TyKind},
    },
    semantics::{const_eval::ConstEvalError, visitor::Visitor},
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
    BulitIn(Symbol, Vec<ResolvedTy>),
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
    pub name: Symbol,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub struct ImplInfo {
    pub self_ty: TypeId,
    pub for_trait: Option<Vec<Symbol>>,
    pub methods: Vec<FnSig>,
}

#[derive(Debug)]
pub struct FnSig {
    pub name: Symbol,
    pub params: Vec<TypeId>,
    pub ret: TypeId,
    pub is_placeholder: bool,
}

#[derive(Debug)]
pub struct Constant {
    pub name: Symbol,
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
    Lambda,
    Root,
    Trait(TypeId),
    Impl(TypeId),
}

#[derive(Debug)]
pub struct Scope {
    pub id: NodeId,
    pub kind: ScopeKind,
    pub types: HashMap<Symbol, TypeInfo>,
    pub values: HashMap<Symbol, Variable>,
    pub children: HashSet<NodeId>,
    pub father: NodeId,
}

#[derive(Debug)]
pub enum PlaceValueExpr {
    Place,
    Value,
}

#[derive(Debug)]
pub enum AssigneeExpr {
    Yes(PlaceValueExpr),
    Not,
}

#[derive(Debug)]
pub struct ExprResult {
    pub type_id: TypeId,
    pub mutbl: Mutability,
    pub category: AssigneeExpr,
}

#[derive(Debug)]
pub enum AnalyzeStage {
    SymbolCollect,
    Definition,
    Body,
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    type_table: TypeTable,
    impls: HashMap<TypeId, Vec<ImplInfo>>,
    scopes: HashMap<NodeId, Scope>,
    current_scope: NodeId,
    stage: AnalyzeStage,
    current_ast_id: NodeId,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut type_table: TypeTable = TypeTable::default();
        type_table.intern(ResolvedTy::unit());

        Self {
            type_table,
            impls: HashMap::default(),
            scopes: HashMap::default(),
            current_scope: 0,
            stage: AnalyzeStage::SymbolCollect,
            current_ast_id: 0,
        }
    }

    fn get_scope_mut(&mut self) -> &mut Scope {
        self.scopes.get_mut(&self.current_scope).unwrap()
    }

    fn get_scope(&self) -> &Scope {
        self.scopes.get(&self.current_scope).unwrap()
    }

    fn add_scope(&mut self, id: NodeId, kind: ScopeKind) -> Result<(), SemanticError> {
        if self.scopes.contains_key(&id) || self.get_scope().children.contains(&id) {
            return Err(SemanticError::InvaildScope);
        }

        self.get_scope_mut().children.insert(id);
        self.scopes.insert(
            id,
            Scope {
                id: id,
                kind: kind,
                types: HashMap::new(),
                values: HashMap::new(),
                children: HashSet::new(),
                father: self.get_scope().id,
            },
        );

        Ok(())
    }

    fn enter_scope(&mut self, id: NodeId) -> Result<(), SemanticError> {
        if !self.get_scope().children.contains(&id) {
            return Err(SemanticError::UndefinedScope);
        }

        self.current_scope = id;

        Ok(())
    }

    fn exit_scope(&mut self) -> Result<(), SemanticError> {
        if matches!(self.get_scope().kind, ScopeKind::Root) {
            return Err(SemanticError::InvaildScope);
        }

        self.current_scope = self.get_scope().id;

        Ok(())
    }

    fn add_type(&mut self, ident: Symbol, info: TypeInfo) -> Result<TypeId, SemanticError> {
        let s = self.get_scope_mut();

        if s.types.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.types.insert(ident.clone(), info);

        let mut full_name = self.get_prefix_name();
        full_name.push(ident);
        let resolved = ResolvedTy::Named(full_name);

        Ok(self.intern_type(resolved))
    }

    fn add_value(&mut self, ident: Symbol, var: Variable) -> Result<(), SemanticError> {
        let s = self.get_scope_mut();

        if s.values.contains_key(&ident) {
            return Err(SemanticError::MultiDefined);
        }

        s.values.insert(ident, var);

        Ok(())
    }

    fn get_type_from(
        &self,
        ident: &Symbol,
        mut id: NodeId,
    ) -> Result<(NodeId, &TypeInfo), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if scope.types.contains_key(ident) {
                return Ok((id, scope.types.get(ident).unwrap()));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownType);
            } else {
                id = scope.father;
            }
        }
    }

    fn get_type_from_mut(
        &mut self,
        ident: &Symbol,
        mut id: NodeId,
    ) -> Result<(NodeId, &mut TypeInfo), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get_mut(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if scope.types.contains_key(ident) {
                // 不知道为什么，此处必须重新 get scope，否则会导致借用检查器报错
                return Ok((
                    id,
                    self.scopes
                        .get_mut(&id)
                        .unwrap()
                        .types
                        .get_mut(ident)
                        .unwrap(),
                ));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownType);
            } else {
                id = scope.father;
            }
        }
    }

    fn get_value_from(
        &self,
        ident: &Symbol,
        mut id: NodeId,
    ) -> Result<(NodeId, &Variable), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if scope.types.contains_key(ident) {
                return Ok((id, scope.values.get(ident).unwrap()));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownVariable);
            } else {
                id = scope.father;
            }
        }
    }

    fn get_value_from_mut(
        &mut self,
        ident: &Symbol,
        mut id: NodeId,
    ) -> Result<(NodeId, &mut Variable), SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get_mut(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            if scope.types.contains_key(ident) {
                return Ok((
                    id,
                    self.scopes
                        .get_mut(&id)
                        .unwrap()
                        .values
                        .get_mut(ident)
                        .unwrap(),
                ));
            }

            if matches!(scope.kind, ScopeKind::Root) {
                return Err(SemanticError::UnknownVariable);
            } else {
                id = scope.father;
            }
        }
    }

    fn get_type_mut(&mut self, ident: &Symbol) -> Result<(NodeId, &mut TypeInfo), SemanticError> {
        self.get_type_from_mut(ident, self.current_scope)
    }

    fn get_type(&self, ident: &Symbol) -> Result<(NodeId, &TypeInfo), SemanticError> {
        self.get_type_from(ident, self.current_scope)
    }

    fn get_value_mut(&mut self, ident: &Symbol) -> Result<(NodeId, &mut Variable), SemanticError> {
        self.get_value_from_mut(ident, self.current_scope)
    }

    fn get_value(&self, ident: &Symbol) -> Result<(NodeId, &Variable), SemanticError> {
        self.get_value_from(ident, self.current_scope)
    }

    pub fn visit(&mut self, krate: &crate::ast::Crate) -> Result<(), SemanticError> {
        self.stage = AnalyzeStage::SymbolCollect;
        self.visit_crate(krate)?;
        self.stage = AnalyzeStage::Definition;
        self.visit_crate(krate)?;
        Ok(())
    }

    fn get_prefix_name_from(&self, mut id: NodeId) -> Vec<Symbol> {
        let mut ret = Vec::new();
        loop {
            let scope = self.scopes.get(&id).unwrap();
            ret.push(Symbol(format!("${}", scope.id)));
            if matches!(scope.kind, ScopeKind::Root) {
                break;
            } else {
                id = scope.father;
            }
        }

        ret.reverse();
        ret
    }

    fn get_prefix_name(&self) -> Vec<Symbol> {
        self.get_prefix_name_from(self.current_scope)
    }

    fn is_builtin_type(&self, ident: &Symbol, arg_num: usize) -> bool {
        const BUILTINS: [(&str, usize); 8] = [
            ("u32", 0),
            ("i32", 0),
            ("char", 0),
            ("str", 0),
            ("String", 0),
            ("Option", 1),
            ("Box", 1),
            ("Result", 2),
        ];

        for (bs, n) in BUILTINS {
            if ident.0 == bs && arg_num == n {
                return true;
            }
        }
        false
    }

    fn get_self_type_from(&self, mut id: NodeId) -> Result<TypeId, SemanticError> {
        loop {
            let scope = if let Some(scope) = self.scopes.get(&id) {
                scope
            } else {
                return Err(SemanticError::UndefinedScope);
            };

            match scope.kind {
                ScopeKind::Root => return Err(SemanticError::UnknownType),
                ScopeKind::Trait(type_id) | ScopeKind::Impl(type_id) => return Ok(type_id),
                _ => id = scope.father,
            }
        }
    }

    fn get_self_type(&self) -> Result<TypeId, SemanticError> {
        self.get_self_type_from(self.current_scope)
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
                let PathTy(qself, path) = path_ty;
                if qself.is_some() {
                    return Err(SemanticError::Unimplemented);
                }

                if path.segments.len() > 1 {
                    return Err(SemanticError::InvaildPath);
                }

                let seg = path.segments.get(0).unwrap();
                let s = &seg.ident.symbol;
                if s.is_path_segment() {
                    if s.is_self() {
                        if seg.args.is_some() {
                            return Err(SemanticError::InvaildPath);
                        }
                        return Ok(self.get_type_by_id(self.get_self_type()?).clone());
                    }
                    return Err(SemanticError::InvaildPath);
                }
                match self.get_type(s) {
                    Ok((id, _)) => {
                        let mut full_name = self.get_prefix_name_from(id);
                        full_name.push(s.clone());

                        Ok(ResolvedTy::Named(full_name))
                    }
                    Err(err) => {
                        if self.is_builtin_type(
                            &seg.ident.symbol,
                            seg.args.as_ref().map_or(0, |x| match x.as_ref() {
                                crate::ast::generic::GenericArgs::AngleBracketed(
                                    angle_bracketed_args,
                                ) => angle_bracketed_args.args.len(),
                            }),
                        ) {
                            let mut args_v = Vec::new();
                            if let Some(args) = &seg.args {
                                match args.as_ref() {
                                    crate::ast::generic::GenericArgs::AngleBracketed(
                                        angle_bracketed_args,
                                    ) => {
                                        for x in &angle_bracketed_args.args {
                                            match x {
                                                crate::ast::generic::AngleBracketedArg::Arg(
                                                    generic_arg,
                                                ) => match generic_arg {
                                                    crate::ast::generic::GenericArg::Type(ty) => {
                                                        args_v.push(self.resolve_ty(ty)?)
                                                    }
                                                },
                                            }
                                        }
                                    }
                                }
                            }
                            Ok(ResolvedTy::BulitIn(s.clone(), args_v))
                        } else {
                            Err(err)
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

    fn unit_expr_result() -> ExprResult {
        ExprResult {
            type_id: Self::unit_type(),
            mutbl: Mutability::Not,
            category: AssigneeExpr::Not,
        }
    }

    fn check_const(&self, ty: &ResolvedTy, expr: &Expr) -> Result<(), SemanticError> {
        if let ResolvedTy::BulitIn(ident, _) = ty {
            match ident.0.as_str() {
                "u32" => {
                    let _: u32 = expr.try_into()?;
                    return Ok(());
                }
                _ => {}
            }
        };

        return Err(SemanticError::ConstEvalError(
            ConstEvalError::NotSupportedExpr,
        ));
    }
}

impl Visitor for SemanticAnalyzer {
    fn visit_crate(&mut self, krate: &Crate) -> Result<(), SemanticError> {
        if matches!(self.stage, AnalyzeStage::SymbolCollect) {
            self.scopes.insert(
                krate.id,
                Scope {
                    id: krate.id,
                    kind: ScopeKind::Root,
                    types: HashMap::default(),
                    values: HashMap::default(),
                    children: HashSet::default(),
                    father: krate.id,
                },
            );
        }
        self.current_scope = krate.id;
        self.current_ast_id = krate.id;

        for item in &krate.items {
            self.visit_item(item)?
        }
        Ok(())
    }

    fn visit_item(&mut self, item: &Item) -> Result<(), SemanticError> {
        let old_id = self.current_ast_id;
        self.current_ast_id = item.id;
        match &item.kind {
            ItemKind::Const(const_item) => self.visit_const_item(const_item)?,
            ItemKind::Fn(fn_item) => self.visit_fn_item(fn_item)?,
            ItemKind::Mod(mod_item) => self.visit_mod_item(mod_item)?,
            ItemKind::Enum(enum_item) => self.visit_enum_item(enum_item)?,
            ItemKind::Struct(struct_item) => self.visit_struct_item(struct_item)?,
            ItemKind::Trait(trait_item) => self.visit_trait_item(trait_item)?,
            ItemKind::Impl(impl_item) => self.visit_impl_item(impl_item)?,
        }
        self.current_ast_id = old_id;
        Ok(())
    }

    fn visit_associate_item(&mut self, item: &Item<AssocItemKind>) -> Result<(), SemanticError> {
        let old_id = self.current_ast_id;
        self.current_ast_id = item.id;
        match &item.kind {
            AssocItemKind::Const(const_item) => self.visit_const_item(const_item)?,
            AssocItemKind::Fn(fn_item) => self.visit_fn_item(fn_item)?,
        }
        self.current_ast_id = old_id;
        Ok(())
    }

    fn visit_const_item(
        &mut self,
        ConstItem { ident, ty, expr }: &ConstItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            // 目前还是认为 const expr 中只能有简单表达式
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
            AnalyzeStage::SymbolCollect => {
                if let Some(b) = body {
                    self.visit_block_expr(b)?;
                }
            }
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

                if let Some(b) = body {
                    self.visit_block_expr(b)?;
                }
            }
            AnalyzeStage::Body => {
                todo!()
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
            AnalyzeStage::SymbolCollect => {
                self.add_type(
                    ident.symbol.clone(),
                    TypeInfo {
                        name: ident.symbol.clone(),
                        kind: TypeKind::Placeholder,
                    },
                )?;
            }
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
                self.get_type_mut(&ident.symbol)?.1.kind = TypeKind::Enum { fields };
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
            AnalyzeStage::SymbolCollect => {
                self.add_type(
                    ident.symbol.clone(),
                    TypeInfo {
                        name: ident.symbol.clone(),
                        kind: TypeKind::Struct { fields: Vec::new() },
                    },
                )?;
            }
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
                self.get_type_mut(&ident.symbol)?.1.kind = TypeKind::Struct { fields }
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
            AnalyzeStage::SymbolCollect => {
                let id = self.add_type(
                    ident.symbol.clone(),
                    TypeInfo {
                        name: ident.symbol.clone(),
                        kind: TypeKind::Placeholder,
                    },
                )?;
                self.add_scope(self.current_ast_id, ScopeKind::Trait(id))?;
            }
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
                                name: ident.symbol.clone(),
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
                                name: ident.symbol.clone(),
                                params: params_id,
                                ret: ret_id,
                                is_placeholder: body.is_none(),
                            });
                        }
                    }
                }
                self.get_type_mut(&ident.symbol)?.1.kind = TypeKind::Trait { methods, constants };
            }
            AnalyzeStage::Body => todo!(),
        }
        self.enter_scope(self.current_ast_id)?;
        for item in items {
            self.visit_associate_item(item)?;
        }
        self.exit_scope()?;
        Ok(())
    }

    fn visit_impl_item(
        &mut self,
        ImplItem {
            generics,
            of_trait,
            self_ty,
            items,
        }: &ImplItem,
    ) -> Result<(), SemanticError> {
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                // 0 type 作为 self ty 的占位符
                self.add_scope(self.current_ast_id, ScopeKind::Impl(TypeId(0)))?;
            }
            AnalyzeStage::Definition => {
                todo!()
                // TODO 在这里检查一下实现和 Trait 是否能对应上，同时把 self type 设置好
            }
            AnalyzeStage::Body => todo!(),
        }
        self.enter_scope(self.current_ast_id)?;
        for item in items {
            self.visit_associate_item(item)?;
        }
        self.exit_scope()?;
        Err(SemanticError::Unimplemented)
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::stmt::Stmt) -> Result<ExprResult, SemanticError> {
        match &stmt.kind {
            StmtKind::Let(local_stmt) => {
                self.visit_let_stmt(local_stmt)?;
                Ok(Self::unit_expr_result())
            }
            StmtKind::Item(item) => {
                self.visit_item(item)?;
                Ok(Self::unit_expr_result())
            }
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Semi(expr) => self.visit_expr(expr).map(|_| Self::unit_expr_result()),
            StmtKind::Empty(_) => Ok(Self::unit_expr_result()),
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
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_const_block_expr(
        &mut self,
        expr: &crate::ast::expr::ConstBlockExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_call_expr(
        &mut self,
        expr: &crate::ast::expr::CallExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_method_call_expr(
        &mut self,
        expr: &crate::ast::expr::MethodCallExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_tup_expr(
        &mut self,
        expr: &crate::ast::expr::TupExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_binary_expr(
        &mut self,
        expr: &crate::ast::expr::BinaryExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_unary_expr(
        &mut self,
        expr: &crate::ast::expr::UnaryExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_lit_expr(
        &mut self,
        expr: &crate::ast::expr::LitExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_cast_expr(
        &mut self,
        expr: &crate::ast::expr::CastExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_let_expr(
        &mut self,
        expr: &crate::ast::expr::LetExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_if_expr(
        &mut self,
        expr: &crate::ast::expr::IfExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_while_expr(
        &mut self,
        expr: &crate::ast::expr::WhileExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_for_loop_expr(
        &mut self,
        expr: &crate::ast::expr::ForLoopExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_loop_expr(
        &mut self,
        expr: &crate::ast::expr::LoopExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_match_expr(
        &mut self,
        expr: &crate::ast::expr::MatchExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_block_expr(
        &mut self,
        expr: &crate::ast::expr::BlockExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_assign_expr(
        &mut self,
        expr: &crate::ast::expr::AssignExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_assign_op_expr(
        &mut self,
        expr: &crate::ast::expr::AssignOpExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_field_expr(
        &mut self,
        expr: &crate::ast::expr::FieldExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_index_expr(
        &mut self,
        expr: &crate::ast::expr::IndexExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_range_expr(
        &mut self,
        expr: &crate::ast::expr::RangeExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_underscore_expr(
        &mut self,
        expr: &crate::ast::expr::UnderscoreExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_path_expr(
        &mut self,
        expr: &crate::ast::expr::PathExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_addr_of_expr(
        &mut self,
        expr: &crate::ast::expr::AddrOfExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_break_expr(
        &mut self,
        expr: &crate::ast::expr::BreakExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_continue_expr(
        &mut self,
        expr: &crate::ast::expr::ContinueExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_ret_expr(
        &mut self,
        expr: &crate::ast::expr::RetExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_struct_expr(
        &mut self,
        expr: &crate::ast::expr::StructExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_repeat_expr(
        &mut self,
        expr: &crate::ast::expr::RepeatExpr,
    ) -> Result<ExprResult, SemanticError> {
        todo!()
    }

    fn visit_pat(&mut self, pat: &crate::ast::pat::Pat) -> Result<ExprResult, SemanticError> {
        todo!()
    }
}
