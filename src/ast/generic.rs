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