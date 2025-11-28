use std::fmt::Display;

use crate::{ast::Span, semantics::type_solver::TypeSolveError};

#[derive(Debug)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Option<Span>,
    #[cfg(debug_assertions)]
    pub file: &'static str,
    #[cfg(debug_assertions)]
    pub line: u32,
}

impl SemanticError {
    pub fn set_span(mut self, span: &Span) -> Self {
        if self.span.is_none() {
            self.span = Some(*span);
        }

        self
    }
}

#[derive(Debug)]
pub enum SemanticErrorKind {
    TypeDefineConflict,
    ValueDefineConflict,
    NoImplementation,
    UnknownType,
    MultipleDefinedField,
    UnknownSelfType,
    ConstEvalNoImplementation,
    TypeError(TypeSolveError),
    UnknownSuffix,
    Overflow,
    IndexOutOfBound,
    ValueFromPathNotFound,
    ScopeFromPathNotFound,
    NotConstantValue,
    NoBinaryOperation,
    NoUnaryOperation,
    NoAssignOperation,
    ConstantWithoutBody,
    FunctionWithoutBody,
    SelfInNotAssociateItem,
    NotCompleteImpl,
    UnknownAssociateItem,
    AssociateItemMismatch,
    BindingNameConflict,
    BindingConflictWithConstant,
    Immutable,
    AssigneeKindMismatch,
    ArgumentNumberMismatch,
    UnkonwnMethod,
    MultipleCandidates,
    TypeUndetermined,
    AssigneeExprNotAllowed,
    CannotMoveOutOfReference,
    IllegalLeftExpression,
    NotStructType,
    UnknownField,
    NotInCycleScope,
    NotInLoopScope,
    NotInFnScope,
    MissingField,
    NotSupportCast,
    NotFunctionType,
    NotMethod,
    NotArrayType,
    NotReferenceType,
    MainFnWithoutExit,
    NotInMainFunction,
    MainFunctionDoubleExit,
    MainFunctionWithWrongType,
    ExprAfterExit,
    NotSizedType,
    CyclicEvaluation,

    GeneralError,
}

#[macro_export]
macro_rules! make_semantic_error {
    ($kind:ident $($tail:tt)*) => {
        $crate::semantics::error::SemanticError {
            kind: $crate::semantics::error::SemanticErrorKind::$kind $($tail)*,
            span: None,
            #[cfg(debug_assertions)]
            file: file!(),
            #[cfg(debug_assertions)]
            line: line!(),
        }
    };
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(debug_assertions)]
        {
            writeln!(
                f,
                "{:?} at {}:{}, from {}:{}",
                self.kind,
                self.span.unwrap().begin.line + 1,
                self.span.unwrap().begin.col,
                self.file,
                self.line
            )
        }

        #[cfg(not(debug_assertions))]
        {
            writeln!(
                f,
                "{:?} at {}:{}",
                self.kind,
                self.span.unwrap().begin.line + 1,
                self.span.unwrap().begin.col
            )
        }
    }
}
