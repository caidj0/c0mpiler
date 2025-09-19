use crate::{ast::expr::BinOp, const_eval::ConstEvalError, semantics::resolved_ty::ResolvedTy};

#[derive(Debug)]
pub enum SemanticError {
    Unimplemented,
    UndefinedScope,
    MultiDefined,
    ConstEvalError(ConstEvalError),
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
    NoBinaryOperation(BinOp, ResolvedTy, ResolvedTy),
    SelfInNoAssocFn,
    NotMainFunction,
    ExprAfterExit,
    MainFunctionNotExited,
    NotSizedType,
}

impl From<ConstEvalError> for SemanticError {
    fn from(value: ConstEvalError) -> Self {
        match value {
            ConstEvalError::Semantic(err) => *err,
            _ => Self::ConstEvalError(value),
        }
    }
}
