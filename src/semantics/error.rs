use crate::const_eval::ConstEvalError;

#[derive(Debug)]
pub enum SemanticError {
    Unimplemented,
    UndefinedScope,
    MultiDefined,
    ConstEvalError(ConstEvalError),
    InvaildPath,
    UnknownType,
    UnknownVariable,
    UnknownField,
    InvaildScope,
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
}

impl From<ConstEvalError> for SemanticError {
    fn from(value: ConstEvalError) -> Self {
        match value {
            ConstEvalError::Semantic(err) => *err,
            _ => Self::ConstEvalError(value),
        }
    }
}
