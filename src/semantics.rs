pub mod const_eval;
pub mod primitives;
pub mod visitor;

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        Crate, Ident, Mutability, NodeId, Symbol,
        expr::{BinOp, BinaryExpr, Expr, LitKind, UnOp},
        item::{
            AssocItemKind, ConstItem, EnumItem, FnItem, FnRetTy, ImplItem, Item, ItemKind,
            StructItem, TraitItem,
        },
        pat::IdentPat,
        stmt::{LocalKind, StmtKind},
        ty::{MutTy, PathTy, RefTy, Ty, TyKind},
    },
    semantics::{const_eval::ConstEvalError, visitor::Visitor},
};

macro_rules! no_assignee {
    ($id:expr) => {
        if matches!($id, ExprCategory::Only) {
            return Err(SemanticError::AssigneeOnlyExpr);
        }
    };
}

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
    UnknownSuffix,
    NoImplementation,
    UnDereferenceable,
    IncompatibleCast,
    AssigneeOnlyExpr,
    TypeMismatch,
    ConflictAssignee,
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
    Any, // for underscore
}

impl ResolvedTy {
    pub fn unit() -> Self {
        Self::Tup(Vec::new())
    }

    pub fn bool() -> Self {
        Self::BulitIn(Symbol("bool".to_string()), Vec::new())
    }

    pub fn char() -> Self {
        Self::BulitIn(Symbol("char".to_string()), Vec::new())
    }

    pub fn integer() -> Self {
        Self::BulitIn(Symbol("integer".to_string()), Vec::new())
    }

    pub fn i32() -> Self {
        Self::BulitIn(Symbol("i32".to_string()), Vec::new())
    }

    pub fn u32() -> Self {
        Self::BulitIn(Symbol("u32".to_string()), Vec::new())
    }

    pub fn str() -> Self {
        Self::BulitIn(Symbol("str".to_string()), Vec::new())
    }

    pub fn ref_str() -> Self {
        Self::Ref(Box::new(Self::str()), Mutability::Not)
    }

    pub fn string() -> Self {
        Self::BulitIn(Symbol("String".to_string()), Vec::new())
    }

    pub fn try_deref(&self) -> &Self {
        if let ResolvedTy::Ref(resolved, _) = self {
            resolved.as_ref()
        } else {
            self
        }
    }

    pub fn is_number_type(&self) -> bool {
        *self == Self::integer() || *self == Self::i32() || *self == Self::u32()
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
pub enum ExprCategory {
    Place(Mutability),
    Not, // 也就是 Value Expr
    Only,
}

#[derive(Debug)]
pub struct ExprResult {
    pub type_id: TypeId,
    pub category: ExprCategory,
}

#[derive(Debug)]
pub struct PatResult {
    pub bindings: Vec<(Symbol, TypeId, Mutability)>,
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
        type_table.intern(ResolvedTy::unit()); // Unit -> 0
        type_table.intern(ResolvedTy::Any); // Any -> 1

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

        s.values.insert(ident, var); // 允许变量遮蔽

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
        const BUILTINS: [(&str, usize); 6] = [
            ("bool", 0),
            ("u32", 0),
            ("i32", 0),
            ("char", 0),
            ("str", 0),
            ("String", 0),
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

    fn any_type() -> TypeId {
        TypeId(1)
    }

    fn unit_expr_result() -> ExprResult {
        ExprResult {
            type_id: Self::unit_type(),
            category: ExprCategory::Not,
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

    // 尝试统一传入的 type （主要是为了 integer）
    fn utilize_ty(&mut self, types: Vec<TypeId>) -> Result<TypeId, SemanticError> {
        let types: Vec<TypeId> = types
            .into_iter()
            .filter(|x| *x != Self::any_type())
            .collect();

        if types.is_empty() {
            return Ok(Self::any_type());
        }

        let mut ret_ty = self.get_type_by_id(*types.first().unwrap());
        for x in types {
            let t = self.get_type_by_id(x);
            if *ret_ty != *t {
                if *ret_ty == ResolvedTy::integer() && t.is_number_type() {
                    ret_ty = t;
                } else {
                    return Err(SemanticError::TypeMismatch);
                }
            }
        }

        Ok(self.intern_type(ret_ty.clone()))
    }

    fn utilize_category(cats: Vec<ExprCategory>) -> Result<ExprCategory, SemanticError> {
        let has_only = cats.iter().any(|x| matches!(x, ExprCategory::Only));
        let has_value = cats.iter().any(|x| matches!(x, ExprCategory::Not));
        let has_immut = cats
            .iter()
            .any(|x| matches!(x, ExprCategory::Place(Mutability::Not)));

        match (has_only, has_value, has_immut) {
            (true, true, _) | (true, false, true) => Err(SemanticError::ConflictAssignee),
            (true, false, false) => Ok(ExprCategory::Only),
            (false, true, _) => Ok(ExprCategory::Not),
            (false, false, true) => Ok(ExprCategory::Place(Mutability::Not)),
            (false, false, false) => Ok(ExprCategory::Place(Mutability::Mut)),
        }
    }
}

impl Visitor for SemanticAnalyzer {
    type DefaultRes = Result<(), SemanticError>;
    type ExprRes = Result<Option<ExprResult>, SemanticError>;
    type PatRes = Result<PatResult, SemanticError>;

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
            AnalyzeStage::Body => {}
        }
        if let Some(b) = body {
            self.visit_block_expr(b)?;
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

    fn visit_stmt(&mut self, stmt: &crate::ast::stmt::Stmt) -> Self::ExprRes {
        match &stmt.kind {
            StmtKind::Let(local_stmt) => {
                self.visit_let_stmt(local_stmt)?;
            }
            StmtKind::Item(item) => {
                self.visit_item(item)?;
            }
            StmtKind::Expr(expr) => return self.visit_expr(expr),
            StmtKind::Semi(expr) => {
                return self
                    .visit_expr(expr)
                    .map(|x| x.map(|_| Self::unit_expr_result()));
            }
            StmtKind::Empty(_) => {}
        }

        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => Ok(None),
            AnalyzeStage::Body => Ok(Some(Self::unit_expr_result())),
        }
    }

    fn visit_let_stmt(&mut self, stmt: &crate::ast::stmt::LocalStmt) -> Result<(), SemanticError> {
        let expected_ty = match &stmt.ty {
            Some(ty) => self.resolve_ty(&ty)?,
            None => return Err(SemanticError::Unimplemented),
        };

        let expected_ty_id = self.intern_type(expected_ty.clone());

        match &stmt.kind {
            LocalKind::Decl => {
                // 从下面复制上来的，因为不用检查变量是否初始化
                let pat_res = self.visit_pat(&stmt.pat, expected_ty_id)?;
                for (symbol, type_id, mutbl) in pat_res.bindings {
                    self.add_value(
                        symbol,
                        Variable {
                            ty: type_id,
                            mutbl: mutbl,
                            kind: VariableKind::Inited,
                        },
                    )?;
                }
            }
            LocalKind::Init(expr) => {
                let res = self.visit_expr(&expr)?;
                match res {
                    Some(ExprResult { type_id, category }) => {
                        no_assignee!(category);
                        let flag = if expected_ty_id == type_id {
                            true
                        } else {
                            let expr_ty = self.get_type_by_id(type_id);
                            expected_ty.is_number_type() && (*expr_ty == ResolvedTy::integer())
                        };
                        if flag {
                            let pat_res = self.visit_pat(&stmt.pat, expected_ty_id)?;
                            for (symbol, type_id, mutbl) in pat_res.bindings {
                                self.add_value(
                                    symbol,
                                    Variable {
                                        ty: type_id,
                                        mutbl: mutbl,
                                        kind: VariableKind::Inited,
                                    },
                                )?;
                            }
                        }
                    }
                    None => {}
                }
            }
        }
        Ok(())
    }

    fn visit_array_expr(&mut self, expr: &crate::ast::expr::ArrayExpr) -> Self::ExprRes {
        let expr_res = expr
            .0
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<Option<ExprResult>>, SemanticError>>()?;

        if matches!(
            self.stage,
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition
        ) {
            debug_assert!(expr_res.iter().all(|x| x.is_none()));
            return Ok(None);
        }

        let (types, cats): (Vec<_>, Vec<_>) = expr_res
            .into_iter()
            .map(|x| {
                let ExprResult { type_id, category } = x.unwrap();
                (type_id, category)
            })
            .unzip();

        let unified_type_id = self.utilize_ty(types)?;
        let unified_ty = self.get_type_by_id(unified_type_id);
        let cat = Self::utilize_category(cats)?;

        let ret_ty = ResolvedTy::Array(Box::new(unified_ty.clone()), expr.0.len() as u32);

        Ok(Some(ExprResult {
            type_id: self.intern_type(ret_ty),
            category: cat,
        }))
    }

    fn visit_const_block_expr(&mut self, expr: &crate::ast::expr::ConstBlockExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_call_expr(&mut self, expr: &crate::ast::expr::CallExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_method_call_expr(&mut self, expr: &crate::ast::expr::MethodCallExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_tup_expr(&mut self, expr: &crate::ast::expr::TupExpr) -> Self::ExprRes {
        match &expr.0[..] {
            [] => Ok(if matches!(self.stage, AnalyzeStage::Body) {
                Some(Self::unit_expr_result())
            } else {
                None
            }),
            [e] => self.visit_expr(e),
            _ => Err(SemanticError::Unimplemented),
        }
    }

    fn visit_binary_expr(
        &mut self,
        BinaryExpr(bin_op, expr1, expr2): &BinaryExpr,
    ) -> Self::ExprRes {
        let res1 = self.visit_expr(expr1)?;
        let res2 = self.visit_expr(expr2)?;

        match (res1, res2) {
            (Some(res1), Some(res2)) => {
                no_assignee!(res1.category);
                no_assignee!(res2.category);

                let ty1 = self.get_type_by_id(res1.type_id).try_deref();
                let ty2 = self.get_type_by_id(res2.type_id).try_deref();

                match bin_op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                        if !ty1.is_number_type() || !ty2.is_number_type() {
                            return Err(SemanticError::NoImplementation);
                        }
                        let ret_ty = if *ty1 == ResolvedTy::integer() {
                            ty2
                        } else if *ty2 == ResolvedTy::integer() || *ty1 == *ty2 {
                            ty1
                        } else {
                            return Err(SemanticError::NoImplementation);
                        };

                        Ok(Some(ExprResult {
                            type_id: self.intern_type(ret_ty.clone()),
                            category: ExprCategory::Not,
                        }))
                    }
                    BinOp::And | BinOp::Or => {
                        if *ty1 == ResolvedTy::bool() && *ty2 == ResolvedTy::bool() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::bool()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                    BinOp::BitXor | BinOp::BitAnd | BinOp::BitOr => {
                        if *ty1 == ResolvedTy::bool() && *ty2 == ResolvedTy::bool() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::bool()),
                                category: ExprCategory::Not,
                            }))
                        } else if ty1.is_number_type() && ty2.is_number_type() {
                            let ret_ty = if *ty1 == ResolvedTy::integer() {
                                ty2
                            } else if *ty2 == ResolvedTy::integer() || *ty1 == *ty2 {
                                ty1
                            } else {
                                return Err(SemanticError::NoImplementation);
                            };

                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ret_ty.clone()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                    BinOp::Shl | BinOp::Shr => {
                        if ty1.is_number_type() && ty2.is_number_type() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ty1.clone()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Ge | BinOp::Gt => {
                        if (matches!(ty1, ResolvedTy::BulitIn(_, _))
                            && matches!(ty2, ResolvedTy::BulitIn(_, _))
                            && *ty1 == *ty2)
                            || ((*ty1 == ResolvedTy::str() || *ty1 == ResolvedTy::string())
                                && (*ty2 == ResolvedTy::str() || *ty2 == ResolvedTy::string()))
                            || (*ty1 == ResolvedTy::integer() && ty2.is_number_type())
                            || (*ty2 == ResolvedTy::integer() && ty1.is_number_type())
                        {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::bool()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                }
            }
            (None, Some(_)) | (Some(_), None) => panic!("Impossible!"),
            (None, None) => Ok(None),
        }
    }

    fn visit_unary_expr(&mut self, expr: &crate::ast::expr::UnaryExpr) -> Self::ExprRes {
        let res = self.visit_expr(&expr.1)?;
        match res {
            Some(ExprResult { type_id, category }) => {
                no_assignee!(category);
                let ty = self.get_type_by_id(type_id);
                match expr.0 {
                    UnOp::Deref => {
                        // 在 rust 中，貌似任意 value expr 都可以取其 ref，并且再解引用后可以得到 place value
                        if let ResolvedTy::Ref(t, multb) = ty {
                            Ok(Some(ExprResult {
                                category: ExprCategory::Place(*multb),
                                type_id: self.intern_type(t.as_ref().clone()),
                            }))
                        } else {
                            Err(SemanticError::UnDereferenceable)
                        }
                    }
                    UnOp::Not => {
                        let ty = ty.try_deref();
                        if *ty == ResolvedTy::bool()
                            || *ty == ResolvedTy::integer()
                            || *ty == ResolvedTy::i32()
                            || *ty == ResolvedTy::u32()
                        {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ty.clone()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                    UnOp::Neg => {
                        let ty = ty.try_deref();
                        if *ty == ResolvedTy::i32() || *ty == ResolvedTy::integer() {
                            Ok(Some(ExprResult {
                                type_id: self.intern_type(ResolvedTy::i32()),
                                category: ExprCategory::Not,
                            }))
                        } else {
                            Err(SemanticError::NoImplementation)
                        }
                    }
                }
            }
            None => Ok(None),
        }
    }

    fn visit_lit_expr(&mut self, expr: &crate::ast::expr::LitExpr) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => Ok(None),
            AnalyzeStage::Body => Ok(Some(ExprResult {
                type_id: match expr.kind {
                    LitKind::Bool => self.intern_type(ResolvedTy::bool()),
                    LitKind::Char => self.intern_type(ResolvedTy::char()),
                    LitKind::Integer => match &expr.suffix {
                        Some(s) => {
                            if s == "u32" {
                                self.intern_type(ResolvedTy::u32())
                            } else if s == "i32" {
                                self.intern_type(ResolvedTy::i32())
                            } else {
                                return Err(SemanticError::UnknownSuffix);
                            }
                        }
                        None => self.intern_type(ResolvedTy::integer()),
                    },
                    LitKind::Str | LitKind::StrRaw(_) => {
                        if expr.suffix.is_none() {
                            self.intern_type(ResolvedTy::ref_str())
                        } else {
                            return Err(SemanticError::UnknownSuffix);
                        }
                    }
                    _ => return Err(SemanticError::Unimplemented),
                },
                category: ExprCategory::Not,
            })),
        }
    }

    fn visit_cast_expr(&mut self, expr: &crate::ast::expr::CastExpr) -> Self::ExprRes {
        let res = self.visit_expr(&expr.0)?;
        match res {
            Some(ExprResult { type_id, category }) => {
                no_assignee!(category);

                let expr_ty = self.get_type_by_id(type_id);
                let target_ty = self.resolve_ty(&expr.1)?;

                if (*expr_ty == ResolvedTy::i32()
                    || *expr_ty == ResolvedTy::u32()
                    || *expr_ty == ResolvedTy::integer()
                    || *expr_ty == ResolvedTy::char()
                    || *expr_ty == ResolvedTy::bool())
                    && (target_ty == ResolvedTy::i32() || target_ty == ResolvedTy::u32())
                {
                    Ok(Some(ExprResult {
                        type_id: self.intern_type(target_ty),
                        category: ExprCategory::Not,
                    }))
                } else {
                    Err(SemanticError::IncompatibleCast)
                }
            }
            None => Ok(None),
        }
    }

    fn visit_let_expr(&mut self, _: &crate::ast::expr::LetExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_if_expr(&mut self, expr: &crate::ast::expr::IfExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_while_expr(&mut self, expr: &crate::ast::expr::WhileExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_for_loop_expr(&mut self, _: &crate::ast::expr::ForLoopExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_loop_expr(&mut self, expr: &crate::ast::expr::LoopExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_match_expr(&mut self, _: &crate::ast::expr::MatchExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_block_expr(&mut self, expr: &crate::ast::expr::BlockExpr) -> Self::ExprRes {
        let old_ast_id = self.current_ast_id;
        self.current_ast_id = expr.id;
        let mut ret;
        match self.stage {
            AnalyzeStage::SymbolCollect => {
                self.add_scope(self.current_ast_id, ScopeKind::Lambda)?;
                ret = None
            }
            AnalyzeStage::Definition => {
                ret = None;
            }
            AnalyzeStage::Body => ret = Some(Self::unit_expr_result()),
        }

        self.enter_scope(self.current_ast_id)?;
        for stmt in &expr.stmts {
            // parser 保证只有最后一条 stmt 有可能不是 unit type
            ret = self.visit_stmt(stmt)?;
        }
        self.exit_scope()?;

        self.current_ast_id = old_ast_id;
        Ok(ret)
    }

    fn visit_assign_expr(&mut self, expr: &crate::ast::expr::AssignExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_assign_op_expr(&mut self, expr: &crate::ast::expr::AssignOpExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_field_expr(&mut self, expr: &crate::ast::expr::FieldExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_index_expr(&mut self, expr: &crate::ast::expr::IndexExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_range_expr(&mut self, _: &crate::ast::expr::RangeExpr) -> Self::ExprRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_underscore_expr(&mut self, _: &crate::ast::expr::UnderscoreExpr) -> Self::ExprRes {
        match self.stage {
            AnalyzeStage::SymbolCollect | AnalyzeStage::Definition => Ok(None),
            AnalyzeStage::Body => Ok(Some(ExprResult {
                type_id: self.intern_type(ResolvedTy::Any),
                category: ExprCategory::Only,
            })),
        }
    }

    fn visit_path_expr(&mut self, expr: &crate::ast::expr::PathExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_addr_of_expr(&mut self, expr: &crate::ast::expr::AddrOfExpr) -> Self::ExprRes {
        match self.visit_expr(&expr.1)? {
            Some(ExprResult { type_id, category }) => {
                let ty = self.get_type_by_id(type_id);
                let ret_ty = ResolvedTy::Ref(Box::new(ty.clone()), expr.0);
                Ok(Some(ExprResult {
                    type_id: self.intern_type(ret_ty),
                    category: ExprCategory::Not,
                }))
            }
            None => Ok(None),
        }
    }

    fn visit_break_expr(&mut self, expr: &crate::ast::expr::BreakExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_continue_expr(&mut self, expr: &crate::ast::expr::ContinueExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_ret_expr(&mut self, expr: &crate::ast::expr::RetExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_struct_expr(&mut self, expr: &crate::ast::expr::StructExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_repeat_expr(&mut self, expr: &crate::ast::expr::RepeatExpr) -> Self::ExprRes {
        todo!()
    }

    fn visit_wild_pat(
        &mut self,
        _pat: &crate::ast::pat::WildPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Ok(PatResult {
            bindings: Vec::new(),
        })
    }

    fn visit_ident_pat(
        &mut self,
        IdentPat(mode, ident, guarder): &IdentPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        if guarder.is_some() {
            return Err(SemanticError::Unimplemented);
        }

        let (target_ty, mutbl) = match mode.0 {
            crate::ast::ByRef::Yes(mutability) => {
                let t = ResolvedTy::Ref(
                    Box::new(self.get_type_by_id(expected_ty).clone()),
                    mutability,
                );
                (self.intern_type(t), Mutability::Not)
            }
            crate::ast::ByRef::No => (expected_ty, mode.1),
        };

        let bindings = vec![(ident.symbol.clone(), target_ty, mutbl)];

        Ok(PatResult { bindings })
    }

    fn visit_struct_pat(
        &mut self,
        _pat: &crate::ast::pat::StructPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_or_pat(
        &mut self,
        _pat: &crate::ast::pat::OrPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_path_pat(
        &mut self,
        pat: &crate::ast::pat::PathPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        todo!()
    }

    fn visit_tuple_pat(
        &mut self,
        _pat: &crate::ast::pat::TuplePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_ref_pat(
        &mut self,
        pat: &crate::ast::pat::RefPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        todo!()
    }

    fn visit_lit_pat(
        &mut self,
        pat: &crate::ast::pat::LitPat,
        expected_ty: TypeId,
    ) -> Self::PatRes {
        todo!()
    }

    fn visit_range_pat(
        &mut self,
        _pat: &crate::ast::pat::RangePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_slice_pat(
        &mut self,
        _pat: &crate::ast::pat::SlicePat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }

    fn visit_rest_pat(
        &mut self,
        _pat: &crate::ast::pat::RestPat,
        _expected_ty: TypeId,
    ) -> Self::PatRes {
        Err(SemanticError::Unimplemented)
    }
}
