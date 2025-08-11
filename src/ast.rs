pub mod expr;
pub mod generic;
pub mod item;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod ty;

use crate::{
    ast::item::Item,
    lexer::{Token, TokenIter},
    tokens::TokenType,
};

pub trait Visitable: Sized {
    #[allow(unused_variables, unused_mut)]
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        // Do something...

        iter.update(using_iter);
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

#[derive(Debug)]
pub enum Ident {
    Empty,
    String(String),
    PathSegment(TokenType),
}

impl Default for Ident {
    fn default() -> Self {
        Ident::Empty
    }
}

impl<'a> TryInto<Ident> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<Ident, Self::Error> {
        match self.token_type {
            TokenType::Id => Ok(Ident::String(self.lexeme.to_owned())),
            TokenType::LSelfType | TokenType::SelfType | TokenType::Crate | TokenType::Super => {
                Ok(Ident::PathSegment(self.token_type.clone()))
            }
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Crate {
    pub items: Vec<Box<Item>>,
}

impl Visitable for Crate {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let mut items = Vec::new();
        while let Some(item) = Item::eat(&mut using_iter) {
            items.push(Box::new(item));
        }

        iter.update(using_iter);
        Some(Self { items })
    }
}
