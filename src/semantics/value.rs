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
        analyzer::SemanticAnalyzer, error::SemanticError, pat::Binding, resolved_ty::TypePtr,
    },
};

#[derive(Debug)]
pub struct Value {
    pub ty: TypePtr,
    pub mutbl: Mutability,
    pub kind: ValueKind,
}

#[derive(Debug, EnumAsInner)]
pub enum ValueKind {
    Anon,
    Constant(ConstantValue),
    Struct(Vec<ValueIndex>),
    Array(Vec<ValueIndex>),
    Fn {
        is_method: bool,
        is_placeholder: bool,
    },

    Binding(ByRef),
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
pub struct ValueIndex {
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
        ty: TypePtr,
        for_trait: Option<TypePtr>,
    },
}

impl SemanticAnalyzer {
    // Search 范围包括局部变量，常量，函数.
    // PathSegment 可以是 Scope 或者是 Type，实际上 Type::xxx 应该是 <Type>::xxx 的语法糖.
    // 别扭的地方在于 Value 可以在 Scope 或者 Type impl 里. 要么沿 scope 向上查，要么是 Type::xxx，先查 Type 再去 impl 里查.
    pub fn search_value_by_path(
        &self,
        scope_id: NodeId,
        _qself: &Option<Box<QSelf>>,
        Path { segments, span }: &Path,
    ) -> Result<ValueIndex, SemanticError> {
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
                        .search_value_in_impl(&type_ptr, &v.ident.symbol)
                        .ok_or(make_semantic_error!(ValueFromPathNotFound).set_span(span)),
                }
            }
            _ => Err(make_semantic_error!(NoImplementation).set_span(span)),
        }
    }

    pub fn search_value_in_impl(&self, ty: &TypePtr, symbol: &Symbol) -> Option<ValueIndex> {
        let impls = self.impls.get(ty).unwrap();
        if impls.inherent.values.contains_key(symbol) {
            return Some(ValueIndex {
                kind: ValueIndexKind::Impl {
                    ty: ty.clone(),
                    for_trait: None,
                },
                name: symbol.clone(),
            });
        }

        for (t, trait_impls) in &impls.traits {
            if trait_impls.values.contains_key(symbol) {
                return Some(ValueIndex {
                    kind: ValueIndexKind::Impl {
                        ty: ty.clone(),
                        for_trait: Some(t.clone()),
                    },
                    name: symbol.clone(),
                });
            }
        }

        None
    }

    pub fn get_value_by_index(&self, index: &ValueIndex) -> &Value {
        match &index.kind {
            ValueIndexKind::Bindings { binding_id } => self.binding_value.get(binding_id).unwrap(),
            ValueIndexKind::Global { scope_id } => {
                self.get_scope(*scope_id).values.get(&index.name).unwrap()
            }
            ValueIndexKind::Impl { ty, for_trait } => {
                let impls = self.impls.get(ty).unwrap();
                let impl_info = if let Some(i) = for_trait {
                    impls.traits.get(i).unwrap()
                } else {
                    &impls.inherent
                };
                impl_info.values.get(&index.name).unwrap()
            }
        }
    }

    pub fn search_value(&self, symbol: &Symbol, start_scope: NodeId) -> Option<ValueIndex> {
        let mut scope_id = Some(start_scope);

        while let Some(id) = scope_id {
            if let Some(_) = self.get_scope_value(id, symbol) {
                return Some(ValueIndex {
                    kind: ValueIndexKind::Global { scope_id: id },
                    name: symbol.clone(),
                });
            } else if let Some(index) = self.get_binding_index(id, symbol) {
                return Some(ValueIndex {
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

        for Binding(symbol, value, pat_id) in bindings {
            if !set.insert(symbol.clone()) {
                return Err(make_semantic_error!(BindingNameConflict));
            }
            if self
                .search_value(&symbol, scope_id)
                .map_or(false, |x| self.get_value_by_index(&x).kind.is_constant())
            {
                return Err(make_semantic_error!(BindingConflictWithConstant));
            }

            self.get_scope_mut(scope_id).bindings.insert(symbol, pat_id);
            self.binding_value.insert(pat_id, value);
        }

        Ok(())
    }
}
