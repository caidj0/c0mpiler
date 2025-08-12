use crate::{
    ast::{
        ASTError, ASTResult, Ident, Mutability, Visitable,
        pat::Pat,
        path::{Path, PathSegment, QSelf},
        stmt::Stmt,
        ty::Ty,
    },
    kind_check,
    lexer::{Token, TokenIter},
    loop_until, match_keyword, skip_keyword,
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
    ConstBlock(ConstBlockExpr),
    Call(CallExpr),
    MethodCall(MethodCallExpr),
    Tup(TupExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Lit(LitExpr),
    Cast(CastExpr),
    Let(LetExpr),
    If(IfExpr),
    While(WhileExpr),
    ForLoop(ForLoopExpr),
    Loop(LoopExpr),
    Match(MatchExpr),
    Block(BlockExpr),
    Assign(AssignExpr),
    AssignOp(AssignOpExpr),
    Field(FieldExpr),
    Index(IndexExpr),
    Range(RangeExpr),
    Underscore(UnderscoreExpr),
    Path(PathExpr),
    AddrOf(AddrOfExpr),
    Break(BreakExpr),
    Continue(ContinueExpr),
    Ret(RetExpr),
    Struct(StructExpr),
    Repeat(RepeatExpr),
}

impl Visitable for Expr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        Self::eat_with_priority(iter, 0)
    }
}

impl Expr {
    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Self> {
        let mut kind = kind_check!(
            iter,
            ExprKind,
            Expr,
            (
                Struct, Path, Array, Repeat, Unary, Lit, Let, Block, If, Match, While, ForLoop,
                Loop, Break, Continue, AddrOf, Tup, ConstBlock, Underscore, Ret
            )
        );

        kind = kind.or_else(|err| match RangeHelper::eat_with_priority(iter, 0) {
            Ok(Some(helper)) => Ok(ExprKind::Range(RangeExpr(None, helper.0, helper.1))),
            Ok(None) => Err(err),
            Err(err2) => Err(err.select(err2)),
        });

        let kind = if let Ok(mut expr1) = kind {
            loop {
                if let Ok(helper) = MethodCallHelper::eat(iter) {
                    expr1 = ExprKind::MethodCall(MethodCallExpr {
                        seg: helper.seg,
                        receiver: Box::new(Expr { kind: expr1 }),
                        args: helper.args,
                    })
                } else if let Ok(helper) = FieldHelper::eat(iter) {
                    expr1 = ExprKind::Field(FieldExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else if let Some(helper) = Option::<CallHelper>::eat(iter)? {
                    expr1 = ExprKind::Call(CallExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else if let Some(helper) = Option::<IndexHelper>::eat(iter)? {
                    expr1 = ExprKind::Index(IndexExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else if let Some(helper) = CastHelper::eat_with_priority(iter, min_priority)? {
                    expr1 = ExprKind::Cast(CastExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else if let Some(helper) = BinaryHelper::eat_with_priority(iter, min_priority)? {
                    expr1 = ExprKind::Binary(BinaryExpr(
                        helper.0,
                        Box::new(Expr { kind: expr1 }),
                        helper.1,
                    ))
                } else if let Some(helper) = RangeHelper::eat_with_priority(iter, min_priority)? {
                    expr1 = ExprKind::Range(RangeExpr(
                        Some(Box::new(Expr { kind: expr1 })),
                        helper.0,
                        helper.1,
                    ))
                } else if let Some(helper) = Option::<AssignHelper>::eat(iter)? {
                    expr1 = ExprKind::Assign(AssignExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else if let Some(helper) = Option::<AssignOpHelper>::eat(iter)? {
                    expr1 = ExprKind::AssignOp(AssignOpExpr(
                        helper.0,
                        Box::new(Expr { kind: expr1 }),
                        helper.1,
                    ))
                } else {
                    break Ok(expr1);
                };
            }
        } else {
            kind
        };

        Ok(Expr { kind: kind? })
    }

    pub fn is_block(&self) -> bool {
        matches!(
            self.kind,
            ExprKind::Block(_) | ExprKind::If(_) | ExprKind::Match(_)
        )
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
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let ret = using_iter.next()?.try_into()?;

        iter.update(using_iter);
        Ok(ret)
    }
}

impl<'a> TryInto<LitExpr> for &Token<'a> {
    type Error = ASTError;

    fn try_into(self) -> Result<LitExpr, Self::Error> {
        let err = || ASTError {
            kind: crate::ast::ASTErrorKind::LiteralError,
            pos: self.pos.clone(),
        };

        match self.token_type {
            TokenType::True | TokenType::False => Ok(LitExpr {
                kind: LitKind::Bool,
                symbol: self.lexeme.to_owned(),
                suffix: None,
            }),

            TokenType::Byte => {
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\'').ok_or_else(err)?;
                Ok(LitExpr {
                    kind: LitKind::Byte,
                    symbol,
                    suffix,
                })
            }

            TokenType::Character => {
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\'').ok_or_else(err)?;
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
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\"').ok_or_else(err)?;
                Ok(LitExpr {
                    kind: LitKind::Str,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawString => {
                let (sharps, other) = self.lexeme.split_once('\"').ok_or_else(err)?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or_else(err)?;
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
                let (symbol, suffix) = parse_quoted_content(content, '\"').ok_or_else(err)?;
                Ok(LitExpr {
                    kind: LitKind::ByteStr,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawByteString => {
                let without_b = &self.lexeme[1..];
                let (sharps, other) = without_b.split_once('\"').ok_or_else(err)?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or_else(err)?;
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
                let (symbol, suffix) = parse_quoted_content(content, '\"').ok_or_else(err)?;
                Ok(LitExpr {
                    kind: LitKind::CStr,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawCString => {
                let without_c = &self.lexeme[1..];
                let (sharps, other) = without_c.split_once('\"').ok_or_else(err)?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or_else(err)?;
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

            _ => Err(ASTError {
                kind: super::ASTErrorKind::MisMatch {
                    expected: "Literal Kind".to_owned(),
                    actual: format!("{:?}", self.token_type.clone()),
                },
                pos: self.pos.clone(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpr(pub UnOp, pub Box<Expr>);

impl Visitable for UnaryExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let unop = using_iter.next()?.try_into()?;
        let expr = Expr::eat_with_priority(&mut using_iter, 1000000)?;

        iter.update(using_iter);
        Ok(Self(unop, Box::new(expr)))
    }
}

#[derive(Debug)]
pub enum UnOp {
    Deref,
    Not,
    Neg,
}

impl<'a> TryInto<UnOp> for &Token<'a> {
    type Error = ASTError;

    fn try_into(self) -> Result<UnOp, Self::Error> {
        match self.token_type {
            TokenType::Star => Ok(UnOp::Deref),
            TokenType::Not => Ok(UnOp::Not),
            TokenType::Minus => Ok(UnOp::Neg),
            _ => Err(ASTError {
                kind: super::ASTErrorKind::MisMatch {
                    expected: "Unary Operation".to_owned(),
                    actual: format!("{:?}", self.token_type.clone()),
                },
                pos: self.pos.clone(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpr(pub BinOp, pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct BinaryHelper(pub BinOp, pub Box<Expr>);

impl BinaryHelper {
    fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        let op_result: Result<BinOp, _> = using_iter.next()?.try_into();
        let op: BinOp = if let Ok(ok) = op_result {
            if ok.get_priority() < min_priority {
                return Ok(None);
            }
            ok
        } else {
            return Ok(None);
        };

        let expr2 = Expr::eat_with_priority(&mut using_iter, op.get_priority() + 1)?;

        iter.update(using_iter);
        Ok(Some(BinaryHelper(op, Box::new(expr2))))
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
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Let);

        let pat = Pat::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::Eq);

        let expr = Expr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(Box::new(pat), Box::new(expr)))
    }
}

#[derive(Debug)]
pub struct ArrayExpr(pub Vec<Box<Expr>>);

impl Visitable for ArrayExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();
        match_keyword!(using_iter, TokenType::OpenSqu);

        let mut exprs = Vec::new();

        loop_until!(using_iter, TokenType::CloseSqu, {
            exprs.push(Box::new(Expr::eat(&mut using_iter)?));
            skip_keyword!(using_iter, TokenType::Comma);
        });

        iter.update(using_iter);
        Ok(Self(exprs))
    }
}

// 要求里面的 Expr 是编译期计算的
#[derive(Debug)]
pub struct AnonConst {
    pub value: Box<Expr>,
}

impl Visitable for AnonConst {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        Ok(Self {
            value: Box::new(Expr::eat(iter)?),
        })
    }
}

#[derive(Debug)]
pub struct RepeatExpr(pub Box<Expr>, pub AnonConst);

impl Visitable for RepeatExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenSqu);

        let expr = Expr::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::Semi);

        let anon = AnonConst::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::CloseSqu);

        iter.update(using_iter);
        Ok(Self(Box::new(expr), anon))
    }
}

#[derive(Debug)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl Visitable for BlockExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenCurly);

        let mut stmts = Vec::new();

        loop_until!(using_iter, TokenType::CloseCurly, {
            stmts.push(Stmt::eat(&mut using_iter)?);
        });

        iter.update(using_iter);
        Ok(Self { stmts })
    }
}

impl Visitable for Option<BlockExpr> {
    fn eat(iter: &mut TokenIter) -> super::ASTResult<Self> {
        if iter.peek()?.token_type == TokenType::OpenCurly {
            BlockExpr::eat(iter).map(Some)
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub struct IndexExpr(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct IndexHelper(pub Box<Expr>);

impl Visitable for Option<IndexHelper> {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        if using_iter.next()?.token_type != TokenType::OpenSqu {
            return Ok(None);
        }

        let expr = Expr::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::CloseSqu);

        iter.update(using_iter);
        Ok(Some(IndexHelper(Box::new(expr))))
    }
}

#[derive(Debug)]
pub struct PathExpr(pub Option<Box<QSelf>>, pub Path);

impl Visitable for PathExpr {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let qself = Option::<QSelf>::eat(&mut using_iter)?;
        let path = Path::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(qself.map(Box::new), path))
    }
}

// Rust 的赋值语句没有返回值，貌似不用关心赋值语句的结合性。
#[derive(Debug)]
pub struct AssignExpr(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct AssignHelper(pub Box<Expr>);

impl Visitable for Option<AssignHelper> {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        if using_iter.next()?.token_type != TokenType::Eq {
            return Ok(None);
        }
        let expr2 = Expr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Some(AssignHelper(Box::new(expr2))))
    }
}

#[derive(Debug)]
pub struct CallExpr(pub Box<Expr>, pub Vec<Box<Expr>>);

#[derive(Debug)]
pub struct TupExpr(pub Vec<Box<Expr>>);
pub type CallHelper = TupExpr;

impl Visitable for Option<CallHelper> {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        if using_iter.next()?.token_type != TokenType::OpenPar {
            return Ok(None);
        }

        let mut exprs = Vec::new();

        loop_until!(using_iter, TokenType::ClosePar, {
            exprs.push(Box::new(Expr::eat(&mut using_iter)?));

            skip_keyword!(using_iter, TokenType::Comma);
        });

        iter.update(using_iter);
        Ok(Some(TupExpr(exprs)))
    }
}

impl Visitable for TupExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenPar);

        let mut exprs = Vec::new();

        loop_until!(using_iter, TokenType::ClosePar, {
            exprs.push(Box::new(Expr::eat(&mut using_iter)?));

            skip_keyword!(using_iter, TokenType::Comma);
        });

        iter.update(using_iter);
        Ok(Self(exprs))
    }
}

#[derive(Debug)]
pub struct FieldExpr(pub Box<Expr>, pub Ident);

#[derive(Debug)]
pub struct FieldHelper(pub Ident);

impl Visitable for FieldHelper {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Dot);

        let ident = using_iter.next()?.try_into()?;

        iter.update(using_iter);
        Ok(Self(ident))
    }
}

#[derive(Debug)]
pub struct MethodCallExpr {
    pub seg: PathSegment,
    pub receiver: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}

#[derive(Debug)]
pub struct MethodCallHelper {
    pub seg: PathSegment,
    pub args: Vec<Box<Expr>>,
}

impl Visitable for MethodCallHelper {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Dot);

        let seg = PathSegment::eat(&mut using_iter)?;
        let call_helper = Option::<CallHelper>::eat(&mut using_iter)?.ok_or(ASTError {
            kind: crate::ast::ASTErrorKind::MisMatch {
                expected: "(".to_string(),
                actual: format!("{:?}", using_iter.peek()?.token_type.clone()),
            },
            pos: using_iter.peek()?.pos.clone(),
        })?;

        iter.update(using_iter);
        Ok(Self {
            seg,
            args: call_helper.0,
        })
    }
}

#[derive(Debug)]
pub struct IfExpr(pub Box<Expr>, pub Box<BlockExpr>, pub Option<Box<Expr>>);

impl Visitable for IfExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::If);

        let expr = Expr::eat(&mut using_iter)?;
        let block = BlockExpr::eat(&mut using_iter)?;

        let else_expr = if using_iter.peek()?.token_type == TokenType::Else {
            using_iter.advance();

            let t = using_iter.peek()?;

            let else_expr = Expr::eat(&mut using_iter)?;

            match else_expr.kind {
                ExprKind::Block(_) | ExprKind::If(_) => Some(Box::new(else_expr)),
                _ => {
                    return Err(ASTError {
                        kind: crate::ast::ASTErrorKind::MisMatch {
                            expected: "Block or If".to_owned(),
                            actual: format!("{t:?}"),
                        },
                        pos: t.pos.clone(),
                    });
                }
            }
        } else {
            None
        };

        iter.update(using_iter);
        Ok(Self(Box::new(expr), Box::new(block), else_expr))
    }
}

#[derive(Debug)]
pub struct MatchExpr(pub Box<Expr>, pub Vec<Arm>);

impl Visitable for MatchExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Match);
        let expr = Expr::eat(&mut using_iter)?;
        match_keyword!(using_iter, TokenType::OpenCurly);

        let mut arms = Vec::new();

        loop_until!(using_iter, TokenType::CloseCurly, {
            let arm = Arm::eat(&mut using_iter)?;
            let is_block = arm.body.is_block();
            arms.push(arm);

            if using_iter.peek()?.token_type == TokenType::Comma {
                using_iter.advance();
            } else if !is_block {
                let t = using_iter.peek()?;
                if t.token_type != TokenType::CloseCurly {
                    return Err(ASTError {
                        kind: crate::ast::ASTErrorKind::MisMatch {
                            expected: "Comma".to_owned(),
                            actual: format!("{:?}", t.token_type.clone()),
                        },
                        pos: t.pos.clone(),
                    });
                }
            }
        });

        iter.update(using_iter);
        Ok(Self(Box::new(expr), arms))
    }
}

#[derive(Debug)]
pub struct Arm {
    pub pat: Box<Pat>,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
}

impl Visitable for Arm {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let pat = Pat::eat(&mut using_iter)?;
        let guard = if using_iter.peek()?.token_type == TokenType::If {
            using_iter.advance();
            Some(Expr::eat(&mut using_iter)?)
        } else {
            None
        };
        match_keyword!(using_iter, TokenType::FatArrow);
        let body = Expr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self {
            pat: Box::new(pat),
            guard: guard.map(Box::new),
            body: Box::new(body),
        })
    }
}

#[derive(Debug)]
pub struct ForLoopExpr {
    pub pat: Box<Pat>,
    pub iter: Box<Expr>,
    pub body: Box<BlockExpr>,
}

impl Visitable for ForLoopExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::For);
        let pat = Pat::eat(&mut using_iter)?;
        match_keyword!(using_iter, TokenType::In);
        let loop_iter = Expr::eat(&mut using_iter)?;
        let block = BlockExpr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self {
            pat: Box::new(pat),
            iter: Box::new(loop_iter),
            body: Box::new(block),
        })
    }
}

#[derive(Debug)]
pub struct LoopExpr(pub Box<BlockExpr>);

impl Visitable for LoopExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Loop);
        let block = BlockExpr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(Box::new(block)))
    }
}

#[derive(Debug)]
pub struct WhileExpr(pub Box<Expr>, pub Box<BlockExpr>);

impl Visitable for WhileExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::While);
        let expr = Expr::eat(&mut using_iter)?;
        let block = BlockExpr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(Box::new(expr), Box::new(block)))
    }
}

#[derive(Debug)]
pub struct RangeExpr(
    pub Option<Box<Expr>>,
    pub Option<Box<Expr>>,
    pub RangeLimits,
);

#[derive(Debug)]
pub struct RangeHelper(pub Option<Box<Expr>>, pub RangeLimits);

impl RangeHelper {
    const RANGE_PRIORITY: usize = 5;

    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        if Self::RANGE_PRIORITY < min_priority {
            return Ok(None);
        }

        let limits = match using_iter.next()?.token_type {
            TokenType::DotDot => RangeLimits::HalfOpen,
            TokenType::DotDotEq => RangeLimits::Closed,
            _ => return Ok(None),
        };

        let expr2 = Expr::eat_with_priority(&mut using_iter, Self::RANGE_PRIORITY + 1).ok();

        iter.update(using_iter);
        Ok(Some(RangeHelper(expr2.map(Box::new), limits)))
    }
}

#[derive(Debug)]
pub enum RangeLimits {
    HalfOpen,
    Closed,
}

#[derive(Debug)]
pub struct AssignOpExpr(pub AssignOp, pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct AssignOpHelper(pub AssignOp, pub Box<Expr>);

impl Visitable for Option<AssignOpHelper> {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let assign_op_result: Result<AssignOp, _> = using_iter.next()?.try_into();
        let assign_op = match assign_op_result {
            Ok(o) => o,
            Err(_) => return Ok(None),
        };
        let expr2 = Expr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Some(AssignOpHelper(assign_op, Box::new(expr2))))
    }
}

#[derive(Debug)]
pub enum AssignOp {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitXorAssign,
    BitAndAssign,
    BitOrAssign,
    ShlAssign,
    ShrAssign,
}

impl<'a> TryInto<AssignOp> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<AssignOp, Self::Error> {
        match self.token_type {
            TokenType::PlusEq => Ok(AssignOp::AddAssign),
            TokenType::MinusEq => Ok(AssignOp::SubAssign),
            TokenType::StarEq => Ok(AssignOp::MulAssign),
            TokenType::SlashEq => Ok(AssignOp::DivAssign),
            TokenType::PercentEq => Ok(AssignOp::RemAssign),
            TokenType::CaretEq => Ok(AssignOp::BitXorAssign),
            TokenType::AndEq => Ok(AssignOp::BitAndAssign),
            TokenType::OrEq => Ok(AssignOp::BitOrAssign),
            TokenType::ShlEq => Ok(AssignOp::ShlAssign),
            TokenType::ShrEq => Ok(AssignOp::ShrAssign),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct StructExpr {
    pub qself: Option<Box<QSelf>>,
    pub path: Path,
    pub fields: Vec<ExprField>,
    pub rest: StructRest,
}

impl Visitable for StructExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let qself = Option::<QSelf>::eat(&mut using_iter)?;
        let path = Path::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::OpenCurly);

        let mut fields = Vec::new();
        let mut rest = StructRest::None;

        loop_until!(using_iter, TokenType::CloseCurly, {
            match using_iter.peek()?.token_type {
                TokenType::DotDot => {
                    using_iter.advance();
                    let t = using_iter.peek()?;
                    rest = if t.token_type != TokenType::Comma
                        && t.token_type != TokenType::CloseCurly
                    {
                        StructRest::Base(Box::new(Expr::eat(&mut using_iter)?))
                    } else {
                        StructRest::Rest
                    };
                }
                _ => {
                    fields.push(ExprField::eat(&mut using_iter)?);
                }
            }

            skip_keyword!(using_iter, TokenType::Comma);
        });

        iter.update(using_iter);
        Ok(Self {
            qself: qself.map(Box::new),
            path,
            fields,
            rest,
        })
    }
}

#[derive(Debug)]
pub struct ExprField {
    pub ident: Ident,
    pub expr: Box<Expr>,
    pub is_shorthand: bool,
}

impl Visitable for ExprField {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let ident: Ident = using_iter.next()?.try_into()?;

        let (expr, is_shorthand) = if using_iter.peek()?.token_type == TokenType::Colon {
            using_iter.advance();
            (Box::new(Expr::eat(&mut using_iter)?), false)
        } else {
            (Box::new(ident.clone().into()), true)
        };

        iter.update(using_iter);
        Ok(Self {
            ident,
            expr,
            is_shorthand,
        })
    }
}

#[derive(Debug)]
pub enum StructRest {
    Base(Box<Expr>),
    Rest,
    None,
}

#[derive(Debug)]
pub struct BreakExpr(pub Option<Box<Expr>>);

impl Visitable for BreakExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Break);

        let expr = Expr::eat(&mut using_iter).ok();

        iter.update(using_iter);
        Ok(Self(expr.map(Box::new)))
    }
}

#[derive(Debug)]
pub struct ContinueExpr;

impl Visitable for ContinueExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Continue);

        iter.update(using_iter);
        Ok(Self)
    }
}

#[derive(Debug)]
pub struct AddrOfExpr(pub Mutability, pub Box<Expr>);

impl Visitable for AddrOfExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::And);
        let mutability = if using_iter.peek()?.token_type == TokenType::Mut {
            Mutability::Mut
        } else {
            Mutability::Not
        };
        let expr = Expr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(mutability, Box::new(expr)))
    }
}

#[derive(Debug)]
pub struct CastExpr(pub Box<Expr>, pub Box<Ty>);

pub struct CastHelper(pub Box<Ty>);

impl CastHelper {
    const CAST_PRIOTIRY: usize = 500;

    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        if using_iter.peek()?.token_type != TokenType::As || Self::CAST_PRIOTIRY < min_priority {
            return Ok(None);
        }
        using_iter.advance();

        let ty = Ty::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Some(CastHelper(Box::new(ty))))
    }
}

#[derive(Debug)]
pub struct ConstBlockExpr(pub AnonConst);

impl Visitable for ConstBlockExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Const);
        let block = BlockExpr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(AnonConst {
            value: Box::new(Expr {
                kind: ExprKind::Block(block),
            }),
        }))
    }
}

#[derive(Debug)]
pub struct UnderscoreExpr;

impl Visitable for UnderscoreExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Underscor);
        Ok(Self)
    }
}

#[derive(Debug)]
pub struct RetExpr(pub Option<Box<Expr>>);

impl Visitable for RetExpr {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Return);
        // 暂时这样做
        let expr = Expr::eat(&mut using_iter).ok();

        iter.update(using_iter);
        Ok(Self(expr.map(Box::new)))
    }
}
