use crate::{
    ast::{Visitable, pat::Pat},
    lexer::{Token, TokenIter},
    match_keyword,
    tokens::TokenType,
    utils::string::{parse_number_literal, parse_quoted_content},
};

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Array(ArrayExpr),
    // ConstBlock(AnonConst),
    // Call(P<Expr>, ThinVec<P<Expr>>),
    // MethodCall(Box<MethodCall>),
    // Tup(ThinVec<P<Expr>>),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Lit(LitExpr),
    // Cast(P<Expr>, P<Ty>),
    Let(LetExpr),
    // If(P<Expr>, P<Block>, Option<P<Expr>>),
    // While(P<Expr>, P<Block>, Option<Label>),
    // ForLoop {
    //     pat: P<Pat>,
    //     iter: P<Expr>,
    //     body: P<Block>,
    //     label: Option<Label>,
    //     kind: ForLoopKind,
    // },
    // Loop(P<Block>, Option<Label>, Span),
    // Match(P<Expr>, ThinVec<Arm>, MatchKind),
    // Block(P<Block>, Option<Label>),
    // Assign(P<Expr>, P<Expr>, Span),
    // AssignOp(AssignOp, P<Expr>, P<Expr>),
    // Field(P<Expr>, Ident),
    // Index(P<Expr>, P<Expr>, Span),
    // Range(Option<P<Expr>>, Option<P<Expr>>, RangeLimits),
    // Underscore,
    // Path(Option<P<QSelf>>, Path),
    // Break(Option<Label>, Option<P<Expr>>),
    // Continue(Option<Label>),
    // Ret(Option<P<Expr>>),
    // Struct(P<StructExpr>),
    // Repeat(P<Expr>, AnonConst),
    // Paren(P<Expr>),
    // Become(P<Expr>),
}

impl Visitable for Expr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        Self::eat_with_priority(iter, 0)
    }
}

impl Expr {
    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> Option<Self> {
        let mut kind: Option<ExprKind> = None;
        kind = kind.or_else(|| ArrayExpr::eat(iter).map(ExprKind::Array));
        kind = kind.or_else(|| UnaryExpr::eat(iter).map(ExprKind::Unary));
        kind = kind.or_else(|| LitExpr::eat(iter).map(ExprKind::Lit));
        kind = kind.or_else(|| LetExpr::eat(iter).map(ExprKind::Let));

        kind = kind.map(|expr1| {
            if let Some(helper) = BinaryHelper::eat_with_priority(iter, min_priority) {
                ExprKind::Binary(BinaryExpr(
                    helper.0,
                    Box::new(Expr { kind: expr1 }),
                    helper.1,
                ))
            } else {
                expr1
            }
        });

        Some(Expr { kind: kind? })
    }
}

#[derive(Debug)]
pub struct LitExpr {
    pub kind: LitKind,
    pub symbol: String,
    pub suffix: Option<String>,
}

#[derive(Debug)]
pub enum LitKind {
    Bool,
    Byte,
    Char,
    Integer,
    Float,
    Str,
    StrRaw(u8),
    ByteStr,
    ByteStrRaw(u8),
    CStr,
    CStrRaw(u8),
}

impl Visitable for LitExpr {
    fn eat(iter: &mut TokenIter) -> Option<LitExpr> {
        let mut using_iter = iter.clone();

        let ret = using_iter.next()?.try_into().ok()?;

        iter.update(using_iter);
        Some(ret)
    }
}

impl<'a> TryInto<LitExpr> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<LitExpr, Self::Error> {
        match self.token_type {
            TokenType::True | TokenType::False => Ok(LitExpr {
                kind: LitKind::Bool,
                symbol: self.lexeme.to_owned(),
                suffix: None,
            }),

            TokenType::Byte => {
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\'').ok_or(())?;
                Ok(LitExpr {
                    kind: LitKind::Byte,
                    symbol,
                    suffix,
                })
            }

            TokenType::Character => {
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\'').ok_or(())?;
                Ok(LitExpr {
                    kind: LitKind::Char,
                    symbol,
                    suffix,
                })
            }

            TokenType::Integer => {
                let (symbol, suffix) = parse_number_literal(self.lexeme);
                Ok(LitExpr {
                    kind: LitKind::Integer,
                    symbol,
                    suffix,
                })
            }

            TokenType::Float => {
                let suffix_pos = self
                    .lexeme
                    .find(|c: char| !(c.is_ascii_digit() || c == '.' || c == '_'));

                let symbol =
                    suffix_pos.map_or(self.lexeme.to_owned(), |pos| self.lexeme[..pos].to_owned());
                let suffix = suffix_pos.map(|pos| self.lexeme[pos..].to_owned());

                Ok(LitExpr {
                    kind: LitKind::Float,
                    symbol,
                    suffix,
                })
            }

            TokenType::String => {
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\"').ok_or(())?;
                Ok(LitExpr {
                    kind: LitKind::Str,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawString => {
                let (sharps, other) = self.lexeme.split_once('\"').ok_or(())?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or(())?;
                let sharp_num = sharps.len() - 1;
                let suffix = &suffix_with_sharps[sharp_num..];
                let suffix = if suffix.is_empty() {
                    None
                } else {
                    Some(suffix.to_owned())
                };

                Ok(LitExpr {
                    kind: LitKind::StrRaw(sharp_num as u8),
                    symbol: symbol.to_owned(),
                    suffix,
                })
            }

            TokenType::ByteString => {
                let content = &self.lexeme[1..];
                let (symbol, suffix) = parse_quoted_content(content, '\"').ok_or(())?;
                Ok(LitExpr {
                    kind: LitKind::ByteStr,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawByteString => {
                let without_b = &self.lexeme[1..];
                let (sharps, other) = without_b.split_once('\"').ok_or(())?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or(())?;
                let sharp_num = sharps.len() - 1;
                let suffix = &suffix_with_sharps[sharp_num..];
                let suffix = if suffix.is_empty() {
                    None
                } else {
                    Some(suffix.to_owned())
                };

                Ok(LitExpr {
                    kind: LitKind::ByteStrRaw(sharp_num as u8),
                    symbol: symbol.to_owned(),
                    suffix,
                })
            }

            TokenType::CString => {
                let content = &self.lexeme[1..];
                let (symbol, suffix) = parse_quoted_content(content, '\"').ok_or(())?;
                Ok(LitExpr {
                    kind: LitKind::CStr,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawCString => {
                let without_c = &self.lexeme[1..];
                let (sharps, other) = without_c.split_once('\"').ok_or(())?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or(())?;
                let sharp_num = sharps.len() - 1;
                let suffix = &suffix_with_sharps[sharp_num..];
                let suffix = if suffix.is_empty() {
                    None
                } else {
                    Some(suffix.to_owned())
                };

                Ok(LitExpr {
                    kind: LitKind::CStrRaw(sharp_num as u8),
                    symbol: symbol.to_owned(),
                    suffix,
                })
            }

            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpr(pub UnOp, pub Box<Expr>);

impl Visitable for UnaryExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let unop = using_iter.next()?.try_into().ok()?;
        let expr = Expr::eat_with_priority(&mut using_iter, 1000000)?;

        iter.update(using_iter);
        Some(Self(unop, Box::new(expr)))
    }
}

#[derive(Debug)]
pub enum UnOp {
    Deref,
    Not,
    Neg,
}

impl<'a> TryInto<UnOp> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<UnOp, Self::Error> {
        match self.token_type {
            TokenType::Star => Ok(UnOp::Deref),
            TokenType::Not => Ok(UnOp::Not),
            TokenType::Minus => Ok(UnOp::Neg),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpr(pub BinOp, pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct BinaryHelper(pub BinOp, pub Box<Expr>);

impl BinaryHelper {
    fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> Option<Self> {
        let mut using_iter = iter.clone();

        let op: BinOp = using_iter.next()?.try_into().ok()?;

        if op.get_priority() < min_priority {
            return None;
        }

        let expr2 = Expr::eat_with_priority(&mut using_iter, op.get_priority() + 1)?;

        iter.update(using_iter);
        Some(BinaryHelper(op, Box::new(expr2)))
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

impl BinOp {
    pub fn get_priority(&self) -> usize {
        match self {
            Self::Or => 10,
            Self::And => 20,
            Self::Eq | Self::Ne | Self::Lt | Self::Gt | Self::Le | Self::Ge => 30,
            Self::BitOr => 40,
            Self::BitXor => 50,
            Self::BitAnd => 60,
            Self::Shl | Self::Shr => 70,
            Self::Add | Self::Sub => 80,
            Self::Mul | Self::Div | Self::Rem => 90,
        }
    }
}

impl<'a> TryInto<BinOp> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<BinOp, Self::Error> {
        match self.token_type {
            TokenType::Plus => Ok(BinOp::Add),
            TokenType::Minus => Ok(BinOp::Sub),
            TokenType::Star => Ok(BinOp::Mul),
            TokenType::Slash => Ok(BinOp::Div),
            TokenType::Percent => Ok(BinOp::Rem),
            TokenType::AndAnd => Ok(BinOp::And),
            TokenType::OrOr => Ok(BinOp::Or),
            TokenType::Caret => Ok(BinOp::BitXor),
            TokenType::And => Ok(BinOp::BitAnd),
            TokenType::Or => Ok(BinOp::BitOr),
            TokenType::Shl => Ok(BinOp::Shl),
            TokenType::Shr => Ok(BinOp::Shr),
            TokenType::EqEq => Ok(BinOp::Eq),
            TokenType::Lt => Ok(BinOp::Lt),
            TokenType::Le => Ok(BinOp::Le),
            TokenType::Ne => Ok(BinOp::Ne),
            TokenType::Ge => Ok(BinOp::Ge),
            TokenType::Gt => Ok(BinOp::Gt),
            _ => Err(()),
        }
    }
}

// 这个 LetExpr 仅供 if 和 while 使用
#[derive(Debug)]
pub struct LetExpr(pub Box<Pat>, pub Box<Expr>);

impl Visitable for LetExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Let);

        let pat = Pat::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::Eq);

        let expr = Expr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self(Box::new(pat), Box::new(expr)))
    }
}

#[derive(Debug)]
pub struct ArrayExpr(pub Vec<Box<Expr>>);

impl Visitable for ArrayExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();
        match_keyword!(using_iter, TokenType::OpenSqu);

        let mut exprs = Vec::new();

        while let Some(expr) = Expr::eat(&mut using_iter) {
            exprs.push(Box::new(expr));

            if using_iter.peek()?.token_type == TokenType::Comma {
                using_iter.next();
            } else {
                break;
            }
        }

        match_keyword!(using_iter, TokenType::CloseSqu);

        iter.update(using_iter);
        Some(Self(exprs))
    }
}
