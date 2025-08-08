pub mod expr;

use crate::lexer::TokenIter;

pub trait Visitable: Sized {
    #[allow(unused_variables)]
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        unimplemented!()
    }
}
