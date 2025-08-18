pub mod expr;
pub mod generic;
pub mod item;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod ty;

use crate::{
    ast::{
        expr::{Expr, PathExpr},
        item::Item,
        path::{Path, PathSegment},
    },
    lexer::{Token, TokenIter, TokenPosition},
    tokens::TokenType,
};

pub trait Eatable: Sized {
    #[allow(unused_variables, unused_mut)]
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        // Do something...

        iter.update(using_iter);
        unimplemented!()
    }
}

pub trait OptionEatable: Sized {
    #[allow(unused_variables, unused_mut)]
    fn try_eat(iter: &mut TokenIter) -> ASTResult<Option<Self>> {
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
        if (matches!(self.kind, ASTErrorKind::Empty) || self.pos < right.pos)
            && !matches!(right.kind, ASTErrorKind::Empty)
        {
            right
        } else {
            self
        }
    }
}

impl Default for ASTError {
    fn default() -> Self {
        Self {
            kind: ASTErrorKind::Empty,
            pos: TokenPosition { line: 0, col: 0 },
        }
    }
}

#[derive(Debug)]
pub enum ASTErrorKind {
    Empty,
    EOF,
    MisMatch { expected: String, actual: String },
    LiteralError,
    MisMatchPat,
}

pub type ASTResult<T> = Result<T, ASTError>;

#[macro_export]
macro_rules! match_keyword {
    ($iter:ident, $e:expr) => {{
        let token = $iter.peek()?;

        if token.token_type == $e {
            $iter.advance();
        } else {
            return Err($crate::ast::ASTError {
                kind: $crate::ast::ASTErrorKind::MisMatch {
                    expected: stringify!($e).to_owned(),
                    actual: format!("{:?}", token),
                },
                pos: token.pos.clone(),
            });
        }
    }};
}

#[macro_export]
macro_rules! peek_keyword {
    ($iter:ident, $e:expr) => {{
        let token = $iter.peek()?;

        if token.token_type != $e {
            return Err($crate::ast::ASTError {
                kind: $crate::ast::ASTErrorKind::MisMatch {
                    expected: stringify!($e).to_owned(),
                    actual: format!("{:?}", token),
                },
                pos: token.pos.clone(),
            });
        }
    }};
}

#[macro_export]
macro_rules! is_keyword {
    ($iter:ident, $e:expr) => {{
        if $iter.peek()?.token_type == $e {
            $iter.advance();
            true
        } else {
            false
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
macro_rules! skip_keyword_or_break {
    ($iter:ident, $e:expr, $fi:expr) => {
        let token = $iter.peek()?;
        if token.token_type == $e {
            $iter.advance();
        } else if token.token_type == $fi {
            break;
        } else {
            return Err($crate::ast::ASTError {
                kind: $crate::ast::ASTErrorKind::MisMatch {
                    expected: stringify!($e $fi).to_owned(),
                    actual: format!("{:?}", token),
                },
                pos: token.pos.clone(),
            });
        }
    };
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
macro_rules! loop_while {
    ($iter:ident, $y:expr, $b:block) => {
        while $iter.peek()?.token_type == $y {
            $iter.advance();
            $b
        }
    };
}

#[macro_export]
macro_rules! kind_check {
    ($iter:ident, $enum_name:ident, $suffix:ident, ($($member:ident),*)) => {
        {
            let mut kind = Err($crate::ast::ASTError::default());

            paste::paste!{
                $(
                    kind = kind.or_else(|err| {
                        [<$member $suffix>]::eat($iter).map($enum_name::$member).map_err(|err2| err.select(err2))
                    });
                )*
            }

            kind
        }

    };
}

#[derive(Debug)]
pub struct BindingMode(pub ByRef, pub Mutability);

impl Eatable for BindingMode {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let r = ByRef::eat(&mut using_iter)?;
        let m = if matches!(r, ByRef::No) {
            Mutability::eat(&mut using_iter)?
        } else {
            Mutability::Not
        };

        iter.update(using_iter);
        Ok(Self(r, m))
    }
}

#[derive(Debug)]
pub enum ByRef {
    Yes(Mutability),
    No,
}

impl Eatable for ByRef {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        if iter.peek()?.token_type == TokenType::Ref {
            iter.advance();
            Ok(Self::Yes(Mutability::eat(iter)?))
        } else {
            Ok(Self::No)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Mutability {
    Not,
    Mut,
}

impl Eatable for Mutability {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        if iter.peek()?.token_type == TokenType::Mut {
            iter.advance();
            Ok(Mutability::Mut)
        } else {
            Ok(Mutability::Not)
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Ident {
    #[default]
    Empty,
    String(String),
    PathSegment(TokenType),
}

impl From<Ident> for Path {
    fn from(val: Ident) -> Self {
        Path {
            segments: vec![PathSegment {
                ident: val,
                args: None,
            }],
        }
    }
}

impl From<Ident> for Expr {
    fn from(val: Ident) -> Self {
        Expr {
            kind: expr::ExprKind::Path(PathExpr(None, val.into())),
        }
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
                    actual: format!("{self:?}"),
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

impl Eatable for Crate {
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
