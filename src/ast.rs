pub mod expr;
pub mod generic;
pub mod item;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod ty;

use crate::{
    ast::{
        item::Item,
        path::{Path, PathSegment},
    },
    lexer::{Token, TokenIter, TokenPosition},
    tokens::TokenType,
};

pub type NodeId = usize;

#[derive(Debug, Clone)]
pub struct Span {
    pub begin: TokenPosition,
    pub end: TokenPosition,
}

pub trait Eatable: Sized {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();
        let ret = Self::eat_impl(&mut using_iter);
        if ret.is_ok() {
            iter.update(using_iter);
        }
        ret
    }

    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self>;
}

pub trait OptionEatable: Sized {
    fn try_eat(iter: &mut TokenIter) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();
        let ret = Self::try_eat_impl(&mut using_iter);

        match &ret {
            Ok(Some(_)) => iter.update(using_iter),
            _ => {}
        }
        ret
    }

    fn try_eat_impl(iter: &mut TokenIter) -> ASTResult<Option<Self>>;
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
    MissingSemi,
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
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let r = ByRef::eat(iter)?;
        let m = if matches!(r, ByRef::No) {
            Mutability::eat(iter)?
        } else {
            Mutability::Not
        };
        Ok(Self(r, m))
    }
}

#[derive(Debug)]
pub enum ByRef {
    Yes(Mutability),
    No,
}

impl Eatable for ByRef {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        if iter.peek()?.token_type == TokenType::Ref {
            iter.advance();
            Ok(Self::Yes(Mutability::eat(iter)?))
        } else {
            Ok(Self::No)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Not,
    Mut,
}

impl Eatable for Mutability {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        if iter.peek()?.token_type == TokenType::Mut {
            iter.advance();
            Ok(Mutability::Mut)
        } else {
            Ok(Mutability::Not)
        }
    }
}

impl Mutability {
    pub fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Mutability::Mut, Mutability::Mut) => Mutability::Mut,
            _ => Mutability::Not,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Symbol(pub String);

impl Symbol {
    pub fn is_path_segment(&self) -> bool {
        self.0 == "self" || self.0 == "Self"
    }

    pub fn is_self(&self) -> bool {
        self.0 == "self"
    }

    pub fn is_big_self(&self) -> bool {
        self.0 == "Self"
    }

    pub fn self_symbol() -> Self {
        Self("self".to_owned())
    }

    pub fn big_self_symbol() -> Self {
        Self("Self".to_owned())
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: Span,
}

impl From<Ident> for Path {
    fn from(val: Ident) -> Self {
        Path {
            span: val.span.clone(),
            segments: vec![PathSegment {
                ident: val,
                args: None,
            }],
        }
    }
}

impl<'a> TryInto<Ident> for &Token<'a> {
    type Error = ASTError;

    fn try_into(self) -> Result<Ident, Self::Error> {
        match self.token_type {
            TokenType::Id => Ok(Ident {
                symbol: Symbol(self.lexeme.to_owned()),
                span: Span {
                    begin: self.pos.clone(),
                    end: TokenPosition {
                        line: self.pos.line,
                        col: self.pos.col + self.lexeme.len(),
                    },
                },
            }),
            TokenType::LSelfType | TokenType::SelfType => Ok(Ident {
                symbol: Symbol(self.token_type.to_keyword().unwrap().to_string()),
                span: Span {
                    begin: self.pos.clone(),
                    end: TokenPosition {
                        line: self.pos.line,
                        col: self.pos.col + self.lexeme.len(),
                    },
                },
            }),
            _ => Err(ASTError {
                kind: ASTErrorKind::MisMatch {
                    expected: r#"Identifier, "self" or "Self""#.to_owned(),
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
    pub id: NodeId,
}

impl Eatable for Crate {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut items = Vec::new();

        while iter.peek().is_ok() {
            items.push(Box::new(Item::eat(iter)?));
        }
        Ok(Self {
            items,
            id: iter.assign_id(),
        })
    }
}
