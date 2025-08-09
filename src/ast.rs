pub mod expr;
pub mod generic;
pub mod item;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod ty;

use crate::{
    lexer::{Token, TokenIter},
    tokens::TokenType,
};

pub trait Visitable: Sized {
    #[allow(unused_variables)]
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        unimplemented!()
    }
}

#[macro_export]
macro_rules! match_keyword {
    ($iter:ident, $e:expr) => {
        if $iter.peek()?.token_type == $e {
            $iter.next();
        } else {
            return None;
        }
    };
}

#[derive(Debug)]
pub struct BindingMode(pub Mutability); // 没有实现引用

impl Visitable for BindingMode {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();
        match_keyword!(using_iter, TokenType::Mut);

        iter.update(using_iter);
        Some(BindingMode(Mutability::Mut))
    }
}

impl Default for BindingMode {
    fn default() -> Self {
        Self(Mutability::Not)
    }
}

#[derive(Debug)]
pub enum Mutability {
    Not,
    Mut,
}

pub type Ident = String;

impl<'a> TryInto<String> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<String, Self::Error> {
        if self.token_type == TokenType::Id {
            Ok(self.lexeme.to_owned())
        } else {
            Err(())
        }
    }
}
