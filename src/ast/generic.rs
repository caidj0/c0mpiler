use crate::ast::{ASTResult, Visitable};

#[derive(Debug)]
pub enum GenericArgs {
    // AngleBracketed(AngleBracketedArgs),
    // Parenthesized(ParenthesizedArgs),
    // ParenthesizedElided(Span),
}

impl Visitable for Option<GenericArgs> {
    fn eat(_iter: &mut crate::lexer::TokenIter) -> super::ASTResult<Self> {
        // TODO
        Ok(None)
    }
}

#[derive(Debug, Default)]
pub struct Generics {
    // pub params: ThinVec<GenericParam>,
    // pub where_clause: WhereClause,
}

impl Visitable for Generics {
    fn eat(_iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        // TODO
        Ok(Generics::default())
    }
}
