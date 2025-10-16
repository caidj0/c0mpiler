use std::fmt::Display;

use crate::{ast::expr::BinOp, semantics::resolved_ty::TypePtr};

#[derive(Debug)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    #[cfg(debug_assertions)]
    pub file: &'static str,
    #[cfg(debug_assertions)]
    pub line: u32,
}

#[derive(Debug)]
pub enum SemanticErrorKind {
    Unimplemented,
    UndefinedScope,
    MultiDefined,
    ConstEvalError(ConstEvalErrorKind),
    InvalidPath,
    UnknownType,
    UnknownVariable,
    UnknownField,
    InvalidScope,
    FnWithoutBody,
    UnknownSuffix,
    NoImplementation,
    UnDereferenceable,
    IncompatibleCast,
    AssigneeOnlyExpr,
    TypeMismatch,
    PatMismatch,
    ConflictAssignee,
    NonFunctionCall,
    NonMethodCall,
    MismatchArgNum,
    ImmutableVar,
    NoLoopScope,
    BreakWithValue,
    NoFnScope,
    NotStructType,
    MultiSpecifiedField,
    NonProvidedField,
    NonAssigneeExpr,
    NonPlaceExpr,
    ConstantWithoutBody,
    MultiImplemented,
    NotTrait,
    NotTraitMember,
    IncompatibleFn,
    NotAllTraitItemsImplemented,
    MultipleApplicable,
    LocalVarOutOfFn,
    MultiBinding,
    ShadowedConstantByBinding,
    NoReturnFunction,
    MissingField,
    NoBinaryOperation(BinOp, TypePtr, TypePtr),
    SelfInNoAssocFn,
    NotMainFunction,
    ExprAfterExit,
    MainFunctionNotExited,
    NotSizedType,
    MainFunctionWithNonUnit,
}

#[macro_export]
macro_rules! make_semantic_error {
    ($kind:ident $($tail:tt)*) => {
        $crate::semantics::error::SemanticError {
            kind: $crate::semantics::error::SemanticErrorKind::$kind $($tail)*,
            #[cfg(debug_assertions)]
            file: file!(),
            #[cfg(debug_assertions)]
            line: line!(),
        }
    };
}

impl From<ConstEvalError> for SemanticError {
    fn from(error: ConstEvalError) -> Self {
        Self {
            kind: match error.kind {
                ConstEvalErrorKind::Semantic(kind) => *kind,
                _ => SemanticErrorKind::ConstEvalError(error.kind),
            },
            #[cfg(debug_assertions)]
            file: error.file,
            #[cfg(debug_assertions)]
            line: error.line,
        }
    }
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(debug_assertions)]
        {
            writeln!(f, "{:?} at {}:{}", self.kind, self.file, self.line)
        }

        #[cfg(not(debug_assertions))]
        {
            writeln!(f, "{:?}", self.kind)
        }
    }
}

#[derive(Debug)]
pub struct ConstEvalError {
    pub kind: ConstEvalErrorKind,
    #[cfg(debug_assertions)]
    pub file: &'static str,
    #[cfg(debug_assertions)]
    pub line: u32,
}

#[derive(Debug)]
pub enum ConstEvalErrorKind {
    NotSupportedExpr,
    IncorrectSuffix,
    Overflow,
    InvalidDigit,
    TypeMisMatch,
    Semantic(Box<SemanticErrorKind>),
    NotAStruct,
    NotStructField,
    NotSupportedCast,
    OutOfBound,
    NonConstVariable,
    NotStructType,
    NotSupportedBinary,
}

#[macro_export]
macro_rules! make_const_eval_error {
    ($kind:ident $($tail:tt)*) => {
        $crate::semantics::error::ConstEvalError {
            kind: $crate::semantics::error::ConstEvalErrorKind::$kind $($tail)*,
            #[cfg(debug_assertions)]
            file: file!(),
            #[cfg(debug_assertions)]
            line: line!(),
        }
    };
}

impl From<SemanticError> for ConstEvalError {
    fn from(error: SemanticError) -> Self {
        Self {
            kind: match error.kind {
                SemanticErrorKind::ConstEvalError(kind) => kind,
                _ => ConstEvalErrorKind::Semantic(Box::new(error.kind)),
            },
            #[cfg(debug_assertions)]
            file: error.file,
            #[cfg(debug_assertions)]
            line: error.line,
        }
    }
}

impl Display for ConstEvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(debug_assertions)]
        {
            writeln!(f, "{:?} at {}:{}", self.kind, self.file, self.line)
        }

        #[cfg(not(debug_assertions))]
        {
            writeln!(f, "{:?}", self.kind)
        }
    }
}
