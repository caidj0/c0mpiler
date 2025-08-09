use crate::ast::Visitable;

#[derive(Debug)]
pub enum GenericArgs {
    // AngleBracketed(AngleBracketedArgs),
    // Parenthesized(ParenthesizedArgs),
    // ParenthesizedElided(Span),
}

impl Visitable for GenericArgs {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        // TODO
        None
    }
}

#[derive(Debug)]
pub struct Generics {
    // pub params: ThinVec<GenericParam>,
    // pub where_clause: WhereClause,
}

impl Visitable for Generics {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        // TODO
        None
    }
}

impl Default for Generics {
    fn default() -> Self {
        Self {}
    }
}
