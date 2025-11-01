use std::{collections::HashSet, rc::Rc};

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        ByRef, Mutability, NodeId, Symbol,
        expr::Expr,
        item::FnItem,
        path::{Path, QSelf},
    },
    impossible, make_semantic_error,
    semantics::{
        analyzer::SemanticAnalyzer,
        error::SemanticError,
        impls::DerefLevel,
        pat::Binding,
        resolved_ty::{ResolvedTyInstance, TypeIntern},
    },
};

// 如何区分 Value, Place, Assignee 表达式
// Value 里不需要表明可变性
#[derive(Debug, Clone)]
pub struct Value<'ast> {
    pub ty: TypeIntern,
    pub kind: ValueKind<'ast>,
}

#[derive(Debug, Clone)]
pub enum FnAstRefInfo<'ast> {
    None,
    Trait(&'ast FnItem),
    Inherent(&'ast FnItem),
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum ValueKind<'ast> {
    Anon,
    Constant(ConstantValue<'ast>),
    Fn {
        method_kind: MethodKind,
        is_placeholder: bool,

        ast_node: FnAstRefInfo<'ast>,
    },

    Binding(ByRef),
    MethodCall {
        level: DerefLevel,
        derefed_ty: TypeIntern,
        index: PlaceValueIndex,
        self_by_ref: bool,
    },
    ExtractElement {
        level: DerefLevel,
        derefed_ty: TypeIntern,
        index: Option<usize>,
    },
    Struct {
        indexes: Vec<usize>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum MethodKind {
    Not,
    ByRef,
    ByRaw,
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum ConstantValue<'ast> {
    ConstantInt(u32),
    ConstantString(String),
    ConstantArray(Vec<ConstantValue<'ast>>),
    Unit,
    UnitStruct,

    UnEval(UnEvalConstant<'ast>),
    Placeholder, // Only for Trait
}

#[derive(Debug, Clone)]
pub struct UnEvalConstant<'ast>(NodeId, &'ast Expr);

impl<'ast> UnEvalConstant<'ast> {
    pub fn new(scope: NodeId, expr: &'ast Expr) -> Self {
        Self(scope, expr)
    }

    pub fn to_ref(&self) -> (NodeId, &'ast Expr) {
        (self.0, self.1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PlaceValueIndex {
    pub(crate) name: Symbol,
    pub(crate) kind: ValueIndexKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueIndexKind {
    Bindings {
        binding_id: NodeId,
    },
    Global {
        scope_id: NodeId,
    },
    Impl {
        ty: ResolvedTyInstance,
        for_trait: Option<Rc<ResolvedTyInstance>>,
    },
}

#[derive(Debug, Clone)]
pub struct PlaceValue<'ast> {
    pub(crate) value: Value<'ast>,
    pub(crate) mutbl: Mutability,
}

impl<'ast> SemanticAnalyzer<'ast> {
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
        let impls = self.get_impls_by_instance_mut(&instance);
        if impls.inherent.values.contains_key(symbol) {
            return Ok(Some(PlaceValueIndex {
                kind: ValueIndexKind::Impl {
                    ty: instance.clone(),
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
                        ty: instance.clone(),
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
    ) -> Result<Option<(DerefLevel, TypeIntern, PlaceValueIndex)>, SemanticError> {
        self.auto_deref(*intern, |analyzer, intern| {
            analyzer.search_value_in_impl(&intern, symbol)
        })
    }

    pub fn get_place_value_by_index(&self, index: &PlaceValueIndex) -> &PlaceValue<'ast> {
        match &index.kind {
            ValueIndexKind::Bindings { binding_id } => self.binding_value.get(binding_id).unwrap(),
            ValueIndexKind::Global { scope_id } => {
                self.get_scope(*scope_id).values.get(&index.name).unwrap()
            }
            ValueIndexKind::Impl { ty, for_trait } => {
                let impls = self.get_impls_by_instance(ty).unwrap();
                let impl_info = if let Some(i) = for_trait {
                    impls.traits.get(i).unwrap()
                } else {
                    &impls.inherent
                };
                impl_info.values.get(&index.name).unwrap()
            }
        }
    }

    pub fn get_place_value_by_index_mut(
        &mut self,
        index: &PlaceValueIndex,
    ) -> &mut PlaceValue<'ast> {
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
                let impls = self.get_impls_by_instance_mut(ty);
                let impl_info = if let Some(i) = for_trait {
                    impls.traits.get_mut(i).unwrap()
                } else {
                    &mut impls.inherent
                };
                impl_info.values.get_mut(&index.name).unwrap()
            }
        }
    }

    pub fn get_value_by_index(&self, index: &ValueIndex) -> &Value<'ast> {
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
            if self.get_scope_value(id, symbol).is_some() {
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
        bindings: Vec<Binding<'ast>>,
        scope_id: NodeId,
    ) -> Result<(), SemanticError> {
        let mut set = HashSet::new();

        for Binding(symbol, value, mutbl, pat_id) in bindings {
            self.check_sized(value.ty)?;

            if !set.insert(symbol.clone()) {
                return Err(make_semantic_error!(BindingNameConflict));
            }
            if self
                .search_value(&symbol, scope_id)
                .is_some_and(|x| self.get_place_value_by_index(&x).value.kind.is_constant())
            {
                return Err(make_semantic_error!(BindingConflictWithConstant));
            }

            self.get_scope_mut(scope_id).bindings.insert(symbol, pat_id);
            self.binding_value
                .insert(pat_id, PlaceValue { value, mutbl });
        }

        Ok(())
    }

    pub fn unit_value(&self) -> Value<'ast> {
        Value {
            ty: self.unit_type(),
            kind: ValueKind::Anon,
        }
    }

    pub fn never_value(&self) -> Value<'ast> {
        Value {
            ty: self.never_type(),
            kind: ValueKind::Anon,
        }
    }
}
