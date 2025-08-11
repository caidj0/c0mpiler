pub mod expr;
pub mod generic;
pub mod item;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod ty;

use crate::{
    ast::item::Item,
    lexer::{Token, TokenIter, TokenPosition},
    tokens::TokenType,
};

pub trait Visitable: Sized {
    #[allow(unused_variables, unused_mut)]
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        // Do something...

        iter.update(using_iter);
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct ASTError {
    pub kind: ASTErrorKind,
    pub pos: TokenPosition,
}

impl ASTError {
    pub fn select(self, right: ASTError) -> ASTError {
        if (matches!(self.kind, ASTErrorKind::Empty) || self.pos < right.pos) && !matches!(right.kind, ASTErrorKind::Empty) {
            right
        } else {
            self
        }
    }
}

impl Default for ASTError {
    fn default() -> Self {
        Self { kind: ASTErrorKind::Empty, pos: TokenPosition{ line: 0, col: 0 }}
    }
}

#[derive(Debug)]
pub enum ASTErrorKind {
    Empty,
    EOF,
    MisMatch { expected: String, actual: String },
    LiteralError,
}

pub type ASTResult<T> = Result<T, ASTError>;

#[macro_export]
macro_rules! match_keyword {
    ($iter:ident, $e:expr) => {{
        let token = $iter.peek()?;

        if token.token_type == $e {
            $iter.advance();
        } else {
            return Err(crate::ast::ASTError {
                kind: crate::ast::ASTErrorKind::MisMatch {
                    expected: stringify!($e).to_owned(),
                    actual: format!("{:?}", token),
                },
                pos: token.pos.clone(),
            });
        }
    }};
}

#[macro_export]
macro_rules! match_prefix {
    ($iter:ident, $e:expr) => {{
        let token = $iter.peek()?;

        if token.token_type == $e {
            $iter.advance();
        } else {
            return Ok(None);
        }
    }};
}

#[macro_export]
macro_rules! skip_keyword {
    ($iter:ident, $e:expr) => {
        if $iter.peek()?.token_type == $e {
            $iter.advance();
        }
    };
}

#[macro_export]
macro_rules! loop_until {
    ($iter:ident, $y:expr, $b:block) => {
        while $iter.peek()?.token_type != $y $b;
        $iter.advance();
    };
}

#[macro_export]
macro_rules! kind_check {
    ($iter:ident, $enum_name:ident, $suffix:ident, ($($member:ident),*)) => {
        {
            let mut kind = Err(crate::ast::ASTError::default());
        
            paste::paste!{
                $(
                    kind = kind.or_else(|err| {
                        [<$member $suffix>]::eat($iter).map($enum_name::$member).or_else(|err2| Err(err.select(err2)))
                    });
                )*
            }

            kind
        }
        
    };
}

#[derive(Debug)]
pub struct BindingMode(pub Mutability); // 没有实现引用

impl Visitable for BindingMode {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();
        match_keyword!(using_iter, TokenType::Mut);

        iter.update(using_iter);
        Ok(BindingMode(Mutability::Mut))
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
    type Error = ASTError;

    fn try_into(self) -> Result<Ident, Self::Error> {
        match self.token_type {
            TokenType::Id => Ok(Ident::String(self.lexeme.to_owned())),
            TokenType::LSelfType | TokenType::SelfType | TokenType::Crate | TokenType::Super => {
                Ok(Ident::PathSegment(self.token_type.clone()))
            }
            _ => Err(ASTError {
                kind: ASTErrorKind::MisMatch {
                    expected: r#"Identifier, "self", "Self", "crate" or "super""#.to_owned(),
                    actual: format!("{:?}", self),
                },
                pos: self.pos.clone(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct Crate {
    pub items: Vec<Box<Item>>,
}

impl Visitable for Crate {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let mut items = Vec::new();

        while using_iter.peek().is_ok() {
            items.push(Box::new(Item::eat(&mut using_iter)?));
        }

        iter.update(using_iter);
        Ok(Self { items })
    }
}
