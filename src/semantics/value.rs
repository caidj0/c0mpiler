use std::collections::HashSet;

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        ByRef, Mutability, NodeId, Symbol,
        expr::Expr,
        path::{Path, QSelf},
    },
    impossible, make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        impls::DerefLevel,
        pat::Binding,
        resolved_ty::{AnyTyKind, TypeIntern, TypeKey},
    },
};

// 如何区分 Value, Place, Assignee 表达式
// Value 里不需要表明可变性
#[derive(Debug)]
pub struct Value {
    pub ty: TypeIntern,
    pub kind: ValueKind,
}

#[derive(Debug, EnumAsInner)]
pub enum ValueKind {
    Anon,
    Constant(ConstantValue),
    Fn {
        is_method: bool,
        is_placeholder: bool,
    },

    Binding(ByRef),
    MethodCall {
        level: DerefLevel,
        index: PlaceValueIndex,
    },
    ExtractElement {
        index: usize,
    },
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum ConstantValue {
    ConstantInt(u32),
    ConstantString(String),
    ConstantArray(Vec<ConstantValue>),
    Unit,

    UnEval(UnEvalConstant),
    Placeholder, // Only for Trait
}

#[derive(Debug, Clone)]
pub struct UnEvalConstant(NodeId, *const Expr);

impl UnEvalConstant {
    pub fn new(scope: NodeId, expr: &Expr) -> Self {
        Self(scope, &raw const *expr)
    }

    pub fn to_ref(&self) -> (NodeId, &Expr) {
        unsafe { (self.0, &*self.1) }
    }
}

#[derive(Debug)]
pub enum ValueIndex {
    Place(PlaceValueIndex),
    Expr(NodeId),
}

impl From<PlaceValueIndex> for ValueIndex {
    fn from(value: PlaceValueIndex) -> Self {
        Self::Place(value)
    }
}

impl From<NodeId> for ValueIndex {
    fn from(value: NodeId) -> Self {
        Self::Expr(value)
    }
}

#[derive(Debug)]
pub struct PlaceValueIndex {
    name: Symbol,
    kind: ValueIndexKind,
}

#[derive(Debug, Clone)]
pub enum ValueIndexKind {
    Bindings {
        binding_id: NodeId,
    },
    Global {
        scope_id: NodeId,
    },
    Impl {
        ty: TypeKey,
        for_trait: Option<TypeKey>,
    },
}

#[derive(Debug)]
pub struct PlaceValue {
    pub(crate) value: Value,
    pub(crate) mutbl: Mutability,
}

impl SemanticAnalyzer {
    // Search 范围包括局部变量，常量，函数.
    // PathSegment 可以是 Scope 或者是 Type，实际上 Type::xxx 应该是 <Type>::xxx 的语法糖.
    // 别扭的地方在于 Value 可以在 Scope 或者 Type impl 里. 要么沿 scope 向上查，要么是 Type::xxx，先查 Type 再去 impl 里查.
    pub fn search_value_by_path(
        &mut self,
        scope_id: NodeId,
        _qself: &Option<Box<QSelf>>,
        Path { segments, span }: &Path,
    ) -> Result<PlaceValueIndex, SemanticError> {
        match &segments[..] {
            [] => impossible!(),
            [segment] => self
                .search_value(&segment.ident.symbol, scope_id)
                .ok_or(make_semantic_error!(ValueFromPathNotFound).set_span(span)),
            [s, v] => {
                let scope_result = self
                    .search_scope_or_type(&s.ident.symbol, scope_id)
                    .ok_or(make_semantic_error!(ScopeFromPathNotFound).set_span(span))?;
                match scope_result.kind {
                    crate::semantics::scope::ScopeSearchResultKind::Type(type_ptr) => self
                        .search_value_in_impl(&type_ptr.into(), &v.ident.symbol)?
                        .ok_or(make_semantic_error!(ValueFromPathNotFound).set_span(span)),
                }
            }
            _ => Err(make_semantic_error!(NoImplementation).set_span(span)),
        }
    }

    pub fn search_value_in_impl(
        &mut self,
        ty: &TypeIntern,
        symbol: &Symbol,
    ) -> Result<Option<PlaceValueIndex>, SemanticError> {
        let instance = self.probe_type_instance(*ty).unwrap();

        let impls = self.impls.get(&instance).unwrap();
        if impls.inherent.values.contains_key(symbol) {
            return Ok(Some(PlaceValueIndex {
                kind: ValueIndexKind::Impl {
                    ty: *ty.as_ref(),
                    for_trait: None,
                },
                name: symbol.clone(),
            }));
        }

        let mut ret = None;

        for (t, trait_impls) in &impls.traits {
            if trait_impls.values.contains_key(symbol) {
                if ret.is_some() {
                    return Err(make_semantic_error!(MultipleCandidates));
                }
                ret = Some(PlaceValueIndex {
                    kind: ValueIndexKind::Impl {
                        ty: *ty.as_ref(),
                        for_trait: Some(t.clone()),
                    },
                    name: symbol.clone(),
                });
            }
        }

        Ok(ret)
    }

    pub fn search_value_in_impl_recursively(
        &mut self,
        intern: &TypeIntern,
        symbol: &Symbol,
    ) -> Result<Option<(DerefLevel, PlaceValueIndex)>, SemanticError> {
        if let Some(value) = self.search_value_in_impl(intern, symbol)? {
            return Ok(Some((DerefLevel::Not, value)));
        }

        let ty = self.probe_type(*intern).unwrap();
        use super::resolved_ty::ResolvedTyKind::*;
        match &ty.kind {
            Placeholder => impossible!(),
            Any(AnyTyKind::Any) => return Err(make_semantic_error!(TypeUndetermined)),
            BuiltIn(_)
            | Tup(_)
            | Enum
            | Trait
            | Array(_, _)
            | Fn(_, _)
            | Any(_)
            | ImplicitSelf(_) => {}
            Ref(type_ptr, ref_mutability) => {
                let mut result = self.search_value_in_impl_recursively(type_ptr, symbol)?;
                result
                    .as_mut()
                    .map(|(level, _)| level.wrap((*ref_mutability).into()));
            }
        }

        Ok(None)
    }

    pub fn get_place_value_by_index(&mut self, index: &PlaceValueIndex) -> &PlaceValue {
        match &index.kind {
            ValueIndexKind::Bindings { binding_id } => self.binding_value.get(binding_id).unwrap(),
            ValueIndexKind::Global { scope_id } => {
                self.get_scope(*scope_id).values.get(&index.name).unwrap()
            }
            ValueIndexKind::Impl { ty, for_trait } => {
                let instance = self.probe_type_instance((*ty).into()).unwrap();
                let impls = self.impls.get(&instance).unwrap();
                let impl_info = if let Some(i) = for_trait {
                    impls.traits.get(i).unwrap()
                } else {
                    &impls.inherent
                };
                impl_info.values.get(&index.name).unwrap()
            }
        }
    }

    pub fn get_place_value_by_index_mut(&mut self, index: &PlaceValueIndex) -> &mut PlaceValue {
        match &index.kind {
            ValueIndexKind::Bindings { binding_id } => {
                self.binding_value.get_mut(binding_id).unwrap()
            }
            ValueIndexKind::Global { scope_id } => self
                .get_scope_mut(*scope_id)
                .values
                .get_mut(&index.name)
                .unwrap(),
            ValueIndexKind::Impl { ty, for_trait } => {
                let instance = self.probe_type_instance((*ty).into()).unwrap();
                let impls = self.impls.get_mut(&instance).unwrap();
                let impl_info = if let Some(i) = for_trait {
                    impls.traits.get_mut(i).unwrap()
                } else {
                    &mut impls.inherent
                };
                impl_info.values.get_mut(&index.name).unwrap()
            }
        }
    }

    pub fn get_value_by_index(&mut self, index: &ValueIndex) -> &Value {
        match index {
            ValueIndex::Place(place_value_index) => {
                &self.get_place_value_by_index(place_value_index).value
            }
            ValueIndex::Expr(id) => self.expr_value.get(id).unwrap(),
        }
    }

    pub fn search_value(&self, symbol: &Symbol, start_scope: NodeId) -> Option<PlaceValueIndex> {
        let mut scope_id = Some(start_scope);

        while let Some(id) = scope_id {
            if let Some(_) = self.get_scope_value(id, symbol) {
                return Some(PlaceValueIndex {
                    kind: ValueIndexKind::Global { scope_id: id },
                    name: symbol.clone(),
                });
            } else if let Some(index) = self.get_binding_index(id, symbol) {
                return Some(PlaceValueIndex {
                    kind: ValueIndexKind::Bindings { binding_id: index },
                    name: symbol.clone(),
                });
            }
            scope_id = self.get_parent_scope(id);
        }
        None
    }

    pub fn add_bindings(
        &mut self,
        bindings: Vec<Binding>,
        scope_id: NodeId,
    ) -> Result<(), SemanticError> {
        let mut set = HashSet::new();

        for Binding(symbol, value, mutbl, pat_id) in bindings {
            if !set.insert(symbol.clone()) {
                return Err(make_semantic_error!(BindingNameConflict));
            }
            if self.search_value(&symbol, scope_id).map_or(false, |x| {
                self.get_place_value_by_index(&x).value.kind.is_constant()
            }) {
                return Err(make_semantic_error!(BindingConflictWithConstant));
            }

            self.get_scope_mut(scope_id).bindings.insert(symbol, pat_id);
            self.binding_value
                .insert(pat_id, PlaceValue { value, mutbl });
        }

        Ok(())
    }

    pub fn unit_value(&self) -> Value {
        Value {
            ty: self.unit_type(),
            kind: ValueKind::Anon,
        }
    }

    pub fn never_value(&self) -> Value {
        Value {
            ty: self.never_type(),
            kind: ValueKind::Anon,
        }
    }
}
