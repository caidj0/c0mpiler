use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        ASTResult, Eatable, Ident, Mutability, NodeId, OptionEatable, Span, SyntaxError,
        pat::Pat,
        path::{Path, PathSegment, QSelf},
        stmt::{Stmt, StmtKind},
        ty::Ty,
    },
    is_keyword, kind_check,
    lexer::{Token, TokenIter},
    loop_until, match_keyword, match_prefix, peek_keyword, skip_keyword_or_break,
    tokens::TokenType,
    utils::string::{parse_number_literal, parse_quoted_content},
};

macro_rules! define_priority {
    ($($name:ident $num:literal),*) => {
        paste::paste! {
            $(
                const [<$name:upper _PRIORITY>]: usize = $num;
            )*
        }
    };
}

define_priority! {
    assign 1,
    range 5,
    or 10, and 20, compare 30, bitor 40, bitxor 50,
    bitand 60, shlshr 70, addsub 80, muldivrem 90,
    cast 500,
    unary 1000000
} // BlockExpression - Expression 处有歧义，暂时认为 block 后不能跟优先级在 index 之下的运算符

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Debug, EnumAsInner)]
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

impl ExprKind {
    pub fn is_expr_with_block(&self) -> bool {
        matches!(
            self,
            Self::Block(_)
                | Self::ConstBlock(_)
                | Self::Loop(_)
                | Self::ForLoop(_)
                | Self::While(_)
                | Self::If(_)
                | Self::Match(_)
        )
    }
}

pub struct ExprEatConfig {
    pub min_priority: usize,
    pub has_struct: bool,
    pub is_stmt_environment: bool,
}

impl Eatable for Expr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        Self::eat_with_priority(iter, 0)
    }
}

impl Expr {
    pub fn eat_by_stmt(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();
        let result = Expr::eat_with_config(
            &mut using_iter,
            ExprEatConfig {
                min_priority: 0,
                has_struct: true,
                is_stmt_environment: true,
            },
        )?;
        iter.update(using_iter);
        Ok(result)
    }

    pub fn eat_without_struct(iter: &mut TokenIter) -> ASTResult<Self> {
        Self::eat_with_config(
            iter,
            ExprEatConfig {
                min_priority: 0,
                has_struct: false,
                is_stmt_environment: false,
            },
        )
    }

    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Self> {
        Self::eat_with_config(
            iter,
            ExprEatConfig {
                min_priority,
                has_struct: true,
                is_stmt_environment: false,
            },
        )
    }

    // 这个 eat 失败时不会自动恢复 iter 位置，要小心使用
    pub fn eat_with_config(
        iter: &mut TokenIter,
        ExprEatConfig {
            min_priority,
            has_struct,
            is_stmt_environment,
        }: ExprEatConfig,
    ) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let mut kind = if has_struct {
            kind_check!(iter, ExprKind, Expr, (Struct))
        } else {
            Err(SyntaxError::default())
        };
        kind = kind.or_else(|err| {
            kind_check!(
                iter,
                ExprKind,
                Expr,
                (
                    Path, Array, Repeat, Unary, Lit, Let, Block, If, Match, While, ForLoop, Loop,
                    Break, Continue, AddrOf, Tup, ConstBlock, Underscore, Ret
                )
            )
            .map_err(|err2| err.select(err2))
        });

        let block_stmt = if let Ok(x) = &kind
            && is_stmt_environment
            && x.is_expr_with_block()
        {
            true
        } else {
            false
        };

        let kind = if let Ok(mut expr1) = kind {
            loop {
                let warp_expr = |e: ExprKind, i: &mut TokenIter| {
                    Box::new(Expr {
                        kind: e,
                        span: Span {
                            begin,
                            end: i.get_pos(),
                        },
                        id: i.assign_id(),
                    })
                };
                if let Ok(helper) = MethodCallHelper::eat(iter) {
                    expr1 = ExprKind::MethodCall(MethodCallExpr {
                        seg: helper.seg,
                        receiver: warp_expr(expr1, iter),
                        args: helper.args,
                        span: helper.span,
                    })
                } else if let Ok(helper) = FieldHelper::eat(iter) {
                    expr1 = ExprKind::Field(FieldExpr(warp_expr(expr1, iter), helper.0))
                } else if !block_stmt && let Some(helper) = CallHelper::try_eat(iter)? {
                    expr1 = ExprKind::Call(CallExpr(warp_expr(expr1, iter), helper.0))
                } else if !block_stmt && let Some(helper) = IndexHelper::try_eat(iter)? {
                    expr1 = ExprKind::Index(IndexExpr(warp_expr(expr1, iter), helper.0))
                } else if !block_stmt
                    && let Some(helper) = CastHelper::eat_with_priority(iter, min_priority)?
                {
                    expr1 = ExprKind::Cast(CastExpr(warp_expr(expr1, iter), helper.0))
                } else if !block_stmt
                    && let Some(helper) =
                        BinaryHelper::eat_with_priority_and_struct(iter, min_priority, has_struct)?
                {
                    expr1 = ExprKind::Binary(BinaryExpr(helper.0, warp_expr(expr1, iter), helper.1))
                } else if !block_stmt
                    && let Some(helper) = RangeHelper::eat_with_priority(iter, min_priority)?
                {
                    expr1 =
                        ExprKind::Range(RangeExpr(Some(warp_expr(expr1, iter)), helper.0, helper.1))
                } else if !block_stmt
                    && let Some(helper) = AssignHelper::eat_with_priority(iter, min_priority)?
                {
                    expr1 = ExprKind::Assign(AssignExpr(warp_expr(expr1, iter), helper.0))
                } else if !block_stmt
                    && let Some(helper) = AssignOpHelper::eat_with_priority(iter, min_priority)?
                {
                    expr1 =
                        ExprKind::AssignOp(AssignOpExpr(helper.0, warp_expr(expr1, iter), helper.1))
                } else {
                    break Ok(expr1);
                };
            }
        } else {
            kind
        };

        Ok(Expr {
            kind: kind?,
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }

    pub fn is_block(&self) -> bool {
        self.kind.is_expr_with_block()
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

impl Eatable for LitExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let ret = iter.next()?.try_into()?;
        Ok(ret)
    }
}

impl<'a> TryInto<LitExpr> for &Token<'a> {
    type Error = SyntaxError;

    fn try_into(self) -> Result<LitExpr, Self::Error> {
        let err = || SyntaxError {
            kind: crate::ast::SyntaxErrorKind::LiteralError,
            pos: self.pos,
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
                if symbol.len() != 1 {
                    return Err(SyntaxError {
                        kind: crate::ast::SyntaxErrorKind::LiteralError,
                        pos: self.pos,
                    });
                }
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

            _ => Err(SyntaxError {
                kind: super::SyntaxErrorKind::MisMatch {
                    expected: "Literal Kind".to_owned(),
                    actual: format!("{:?}", self.token_type.clone()),
                },
                pos: self.pos,
            }),
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpr(pub UnOp, pub Box<Expr>);

impl Eatable for UnaryExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        // 除了 . 运算符，unary 是优先级最高的
        let unop = iter.next()?.try_into()?;
        let expr = Expr::eat_with_priority(iter, UNARY_PRIORITY + 1)?;
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
    type Error = SyntaxError;

    fn try_into(self) -> Result<UnOp, Self::Error> {
        match self.token_type {
            TokenType::Star => Ok(UnOp::Deref),
            TokenType::Not => Ok(UnOp::Not),
            TokenType::Minus => Ok(UnOp::Neg),
            _ => Err(SyntaxError {
                kind: super::SyntaxErrorKind::MisMatch {
                    expected: "Unary Operation".to_owned(),
                    actual: format!("{:?}", self.token_type.clone()),
                },
                pos: self.pos,
            }),
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpr(pub BinOp, pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct BinaryHelper(pub BinOp, pub Box<Expr>);

impl BinaryHelper {
    fn eat_with_priority_and_struct(
        iter: &mut TokenIter,
        min_priority: usize,
        has_struct: bool,
    ) -> ASTResult<Option<Self>> {
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

        let expr2 = Expr::eat_with_config(
            &mut using_iter,
            ExprEatConfig {
                min_priority: op.get_priority() + 1,
                has_struct,
                is_stmt_environment: false,
            },
        )?;

        iter.update(using_iter);
        Ok(Some(BinaryHelper(op, Box::new(expr2))))
    }
}

#[derive(Debug, Clone, Copy, EnumAsInner)]
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
            Self::Or => OR_PRIORITY,
            Self::And => AND_PRIORITY,
            Self::Eq | Self::Ne | Self::Lt | Self::Gt | Self::Le | Self::Ge => COMPARE_PRIORITY,
            Self::BitOr => BITOR_PRIORITY,
            Self::BitXor => BITXOR_PRIORITY,
            Self::BitAnd => BITAND_PRIORITY,
            Self::Shl | Self::Shr => SHLSHR_PRIORITY,
            Self::Add | Self::Sub => ADDSUB_PRIORITY,
            Self::Mul | Self::Div | Self::Rem => MULDIVREM_PRIORITY,
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

impl Eatable for LetExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Let);

        let pat = Pat::eat(iter)?;

        match_keyword!(iter, TokenType::Eq);

        let expr = Expr::eat_without_struct(iter)?;
        Ok(Self(Box::new(pat), Box::new(expr)))
    }
}

#[derive(Debug)]
pub struct ArrayExpr(pub Vec<Box<Expr>>);

impl Eatable for ArrayExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenSqu);

        let mut exprs = Vec::new();

        loop_until!(iter, TokenType::CloseSqu, {
            exprs.push(Box::new(Expr::eat(iter)?));
            skip_keyword_or_break!(iter, TokenType::Comma, TokenType::CloseSqu);
        });

        Ok(Self(exprs))
    }
}

// 要求里面的 Expr 是编译期计算的
#[derive(Debug)]
pub struct AnonConst {
    pub value: Box<Expr>,
    pub id: NodeId,
}

impl Eatable for AnonConst {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        Ok(Self {
            value: Box::new(Expr::eat(iter)?),
            id: iter.assign_id(),
        })
    }
}

#[derive(Debug)]
pub struct RepeatExpr(pub Box<Expr>, pub AnonConst);

impl Eatable for RepeatExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenSqu);

        let expr = Expr::eat(iter)?;

        match_keyword!(iter, TokenType::Semi);

        let anon = AnonConst::eat(iter)?;

        match_keyword!(iter, TokenType::CloseSqu);

        Ok(Self(Box::new(expr), anon))
    }
}

#[derive(Debug)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
    pub id: NodeId,
    pub span: Span,
}

impl Eatable for BlockExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();
        match_keyword!(iter, TokenType::OpenCurly);

        let mut stmts = Vec::new();

        loop_until!(iter, TokenType::CloseCurly, {
            stmts.push(Stmt::eat(iter)?);
        });

        let mut no_item_stmt_iter = stmts.iter().rev().filter(|x| {
            !matches!(
                x,
                Stmt {
                    kind: StmtKind::Item(_),
                    id: _,
                    span: _
                }
            )
        });

        no_item_stmt_iter.next();
        for stmt in no_item_stmt_iter {
            if let StmtKind::Expr(e) = &stmt.kind
                && !e.is_block()
            {
                return Err(SyntaxError {
                    kind: crate::ast::SyntaxErrorKind::MissingSemi,
                    pos: e.span.end,
                });
            }
        }

        Ok(Self {
            stmts,
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

impl OptionEatable for BlockExpr {
    fn try_eat_impl(iter: &mut TokenIter) -> ASTResult<Option<Self>> {
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

impl OptionEatable for IndexHelper {
    fn try_eat_impl(iter: &mut TokenIter) -> ASTResult<Option<Self>> {
        match_prefix!(iter, TokenType::OpenSqu);

        let expr = Expr::eat(iter)?;

        match_keyword!(iter, TokenType::CloseSqu);

        Ok(Some(IndexHelper(Box::new(expr))))
    }
}

#[derive(Debug)]
pub struct PathExpr(pub Option<Box<QSelf>>, pub Path);

impl Eatable for PathExpr {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let qself = QSelf::try_eat(iter)?;
        let path = Path::eat(iter)?;

        Ok(Self(qself.map(Box::new), path))
    }
}

// Rust 的赋值语句没有返回值，貌似不用关心赋值语句的结合性。
#[derive(Debug)]
pub struct AssignExpr(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct AssignHelper(pub Box<Expr>);

impl AssignHelper {
    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        if using_iter.peek()?.token_type != TokenType::Eq || ASSIGN_PRIORITY < min_priority {
            return Ok(None);
        }
        using_iter.advance();

        let expr2 = Expr::eat_with_priority(&mut using_iter, ASSIGN_PRIORITY + 1)?;

        iter.update(using_iter);
        Ok(Some(AssignHelper(Box::new(expr2))))
    }
}

#[derive(Debug)]
pub struct CallExpr(pub Box<Expr>, pub Vec<Box<Expr>>);

#[derive(Debug)]
pub struct TupExpr(pub Vec<Box<Expr>>, pub bool);
pub type CallHelper = TupExpr;

impl OptionEatable for CallHelper {
    fn try_eat_impl(iter: &mut TokenIter) -> ASTResult<Option<Self>> {
        match_prefix!(iter, TokenType::OpenPar);

        let mut exprs = Vec::new();

        let mut force = false;

        loop_until!(iter, TokenType::ClosePar, {
            exprs.push(Box::new(Expr::eat(iter)?));

            let token = iter.peek()?;
            if token.token_type == (TokenType::Comma) {
                force = true;
                iter.advance();
            } else if token.token_type == (TokenType::ClosePar) {
                break;
            } else {
                return Err(crate::make_syntax_error!(
                    token,
                    MisMatch {
                        expected: stringify!((TokenType::Comma)(TokenType::ClosePar)).to_owned(),
                        actual: format!("{:?}", token),
                    }
                ));
            };
        });

        Ok(Some(TupExpr(exprs, force)))
    }
}

impl Eatable for TupExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenPar);

        let mut exprs = Vec::new();

        let mut force = false;

        loop_until!(iter, TokenType::ClosePar, {
            exprs.push(Box::new(Expr::eat(iter)?));

            let token = iter.peek()?;
            if token.token_type == (TokenType::Comma) {
                force = true;
                iter.advance();
            } else if token.token_type == (TokenType::ClosePar) {
                break;
            } else {
                return Err(crate::make_syntax_error!(
                    token,
                    MisMatch {
                        expected: stringify!((TokenType::Comma)(TokenType::ClosePar)).to_owned(),
                        actual: format!("{:?}", token),
                    }
                ));
            };
        });

        Ok(Self(exprs, force))
    }
}

#[derive(Debug)]
pub struct FieldExpr(pub Box<Expr>, pub Ident);

#[derive(Debug)]
pub struct FieldHelper(pub Ident);

impl Eatable for FieldHelper {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Dot);

        let ident = iter.next()?.try_into()?;

        Ok(Self(ident))
    }
}

#[derive(Debug)]
pub struct MethodCallExpr {
    pub seg: PathSegment,
    pub receiver: Box<Expr>,
    pub args: Vec<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MethodCallHelper {
    pub seg: PathSegment,
    pub args: Vec<Box<Expr>>,
    pub span: Span,
}

impl Eatable for MethodCallHelper {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Dot);

        let begin = iter.get_pos();

        let seg = PathSegment::eat(iter)?;
        let call_helper = CallHelper::try_eat(iter)?.ok_or(SyntaxError {
            kind: crate::ast::SyntaxErrorKind::MisMatch {
                expected: "(".to_string(),
                actual: format!("{:?}", iter.peek()?.token_type.clone()),
            },
            pos: iter.peek()?.pos,
        })?;

        Ok(Self {
            seg,
            args: call_helper.0,
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

#[derive(Debug)]
pub struct IfExpr(pub Box<Expr>, pub Box<BlockExpr>, pub Option<Box<Expr>>);

impl Eatable for IfExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::If);
        match_keyword!(iter, TokenType::OpenPar);

        let expr = Expr::eat_without_struct(iter)?;

        match_keyword!(iter, TokenType::ClosePar);

        let block = BlockExpr::eat(iter)?;

        let else_expr = if is_keyword!(iter, TokenType::Else) {
            let t = iter.peek()?;
            let begin = t.pos;

            Some(Box::new(Expr {
                kind: match t.token_type {
                    TokenType::If => ExprKind::If(IfExpr::eat(iter)?),
                    TokenType::OpenCurly => ExprKind::Block(BlockExpr::eat(iter)?),
                    _ => {
                        return Err(SyntaxError {
                            kind: crate::ast::SyntaxErrorKind::MisMatch {
                                expected: "Block or If".to_owned(),
                                actual: format!("{t:?}"),
                            },
                            pos: t.pos,
                        });
                    }
                },
                span: Span {
                    begin,
                    end: iter.get_pos(),
                },
                id: iter.assign_id(),
            }))
        } else {
            None
        };

        Ok(Self(Box::new(expr), Box::new(block), else_expr))
    }
}

#[derive(Debug)]
pub struct MatchExpr(pub Box<Expr>, pub Vec<Arm>);

impl Eatable for MatchExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Match);
        let expr = Expr::eat_without_struct(iter)?;
        match_keyword!(iter, TokenType::OpenCurly);

        let mut arms = Vec::new();

        loop_until!(iter, TokenType::CloseCurly, {
            let arm = Arm::eat(iter)?;
            let is_block = arm.body.is_block();
            arms.push(arm);

            if iter.peek()?.token_type == TokenType::Comma {
                iter.advance();
            } else if !is_block {
                let t = iter.peek()?;
                if t.token_type != TokenType::CloseCurly {
                    return Err(SyntaxError {
                        kind: crate::ast::SyntaxErrorKind::MisMatch {
                            expected: "Comma".to_owned(),
                            actual: format!("{:?}", t.token_type.clone()),
                        },
                        pos: t.pos,
                    });
                }
            }
        });

        Ok(Self(Box::new(expr), arms))
    }
}

#[derive(Debug)]
pub struct Arm {
    pub pat: Box<Pat>,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
    pub span: Span,
    pub id: NodeId,
}

impl Eatable for Arm {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let pat = Pat::eat(iter)?;
        let guard = if iter.peek()?.token_type == TokenType::If {
            iter.advance();
            Some(Expr::eat(iter)?)
        } else {
            None
        };
        match_keyword!(iter, TokenType::FatArrow);
        let body = Expr::eat(iter)?;

        Ok(Self {
            pat: Box::new(pat),
            guard: guard.map(Box::new),
            body: Box::new(body),
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

#[derive(Debug)]
pub struct ForLoopExpr {
    pub pat: Box<Pat>,
    pub iter: Box<Expr>,
    pub body: Box<BlockExpr>,
}

impl Eatable for ForLoopExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::For);
        let pat = Pat::eat(iter)?;
        match_keyword!(iter, TokenType::In);
        let loop_iter = Expr::eat(iter)?;
        let block = BlockExpr::eat(iter)?;

        Ok(Self {
            pat: Box::new(pat),
            iter: Box::new(loop_iter),
            body: Box::new(block),
        })
    }
}

#[derive(Debug)]
pub struct LoopExpr(pub Box<BlockExpr>);

impl Eatable for LoopExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Loop);
        let block = BlockExpr::eat(iter)?;

        Ok(Self(Box::new(block)))
    }
}

#[derive(Debug)]
pub struct WhileExpr(pub Box<Expr>, pub Box<BlockExpr>);

impl Eatable for WhileExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::While);
        match_keyword!(iter, TokenType::OpenPar);
        let expr = Expr::eat(iter)?;
        match_keyword!(iter, TokenType::ClosePar);
        let block = BlockExpr::eat(iter)?;

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
    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        if RANGE_PRIORITY < min_priority {
            return Ok(None);
        }

        let limits = match using_iter.next()?.token_type {
            TokenType::DotDot => RangeLimits::HalfOpen,
            TokenType::DotDotEq => RangeLimits::Closed,
            _ => return Ok(None),
        };

        let mut using_iter2 = using_iter.clone();
        let expr2 = Expr::eat_with_priority(&mut using_iter2, RANGE_PRIORITY + 1).ok();
        if expr2.is_some() {
            using_iter.update(using_iter2);
        }

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

impl AssignOpHelper {
    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        if ASSIGN_PRIORITY < min_priority {
            return Ok(None);
        }

        let assign_op_result: Result<AssignOp, _> = using_iter.next()?.try_into();
        let assign_op = match assign_op_result {
            Ok(o) => o,
            Err(_) => return Ok(None),
        };

        let expr2 = Expr::eat_with_priority(&mut using_iter, ASSIGN_PRIORITY + 1)?;

        iter.update(using_iter);
        Ok(Some(AssignOpHelper(assign_op, Box::new(expr2))))
    }
}

#[derive(Debug, EnumAsInner)]
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

impl Eatable for StructExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let qself = QSelf::try_eat(iter)?;
        let path = Path::eat(iter)?;

        match_keyword!(iter, TokenType::OpenCurly);

        let mut fields = Vec::new();
        let mut rest = StructRest::None;

        loop_until!(iter, TokenType::CloseCurly, {
            match iter.peek()?.token_type {
                TokenType::DotDot => {
                    iter.advance();
                    let t = iter.peek()?;
                    rest = if t.token_type != TokenType::Comma
                        && t.token_type != TokenType::CloseCurly
                    {
                        StructRest::Base(Box::new(Expr::eat(iter)?))
                    } else {
                        StructRest::Rest
                    };
                    peek_keyword!(iter, TokenType::CloseCurly);
                    break;
                }
                _ => {
                    fields.push(ExprField::eat(iter)?);
                }
            }

            skip_keyword_or_break!(iter, TokenType::Comma, TokenType::CloseCurly);
        });

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
    pub id: NodeId,
    pub span: Span,
}

impl Eatable for ExprField {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let ident: Ident = iter.next()?.try_into()?;

        let (expr, is_shorthand) = if iter.peek()?.token_type == TokenType::Colon {
            iter.advance();
            (Box::new(Expr::eat(iter)?), false)
        } else {
            let expr = Expr {
                kind: ExprKind::Path(PathExpr(None, ident.clone().into())),
                span: ident.span,
                id: iter.assign_id(),
            };
            (Box::new(expr), true)
        };

        Ok(Self {
            ident,
            expr,
            is_shorthand,
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
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

impl Eatable for BreakExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Break);

        let expr = Expr::eat(iter).ok();

        Ok(Self(expr.map(Box::new)))
    }
}

#[derive(Debug)]
pub struct ContinueExpr;

impl Eatable for ContinueExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Continue);

        Ok(Self)
    }
}

#[derive(Debug)]
pub struct AddrOfExpr(pub Mutability, pub Box<Expr>);

impl Eatable for AddrOfExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let double_ref = if is_keyword!(iter, TokenType::AndAnd) {
            true
        } else {
            match_keyword!(iter, TokenType::And);
            false
        };

        let mutability = if is_keyword!(iter, TokenType::Mut) {
            Mutability::Mut
        } else {
            Mutability::Not
        };
        let expr = Expr::eat_with_priority(iter, UNARY_PRIORITY + 1)?;

        let mut ret = Self(mutability, Box::new(expr));

        if double_ref {
            ret = Self(
                Mutability::Not,
                Box::new(Expr {
                    kind: ExprKind::AddrOf(ret),
                    span: Span {
                        begin,
                        end: iter.get_pos(),
                    },
                    id: iter.assign_id(),
                }),
            );
        }

        Ok(ret)
    }
}

#[derive(Debug)]
pub struct CastExpr(pub Box<Expr>, pub Box<Ty>);

pub struct CastHelper(pub Box<Ty>);

impl CastHelper {
    pub fn eat_with_priority(iter: &mut TokenIter, min_priority: usize) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        if using_iter.peek()?.token_type != TokenType::As || CAST_PRIORITY < min_priority {
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

impl Eatable for ConstBlockExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Const);

        let begin = iter.get_pos();
        let block = BlockExpr::eat(iter)?;

        Ok(Self(AnonConst {
            value: Box::new(Expr {
                kind: ExprKind::Block(block),
                span: Span {
                    begin,
                    end: iter.get_pos(),
                },
                id: iter.assign_id(),
            }),
            id: iter.assign_id(),
        }))
    }
}

#[derive(Debug)]
pub struct UnderscoreExpr;

impl Eatable for UnderscoreExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Underscor);
        Ok(Self)
    }
}

#[derive(Debug)]
pub struct RetExpr(pub Option<Box<Expr>>);

impl Eatable for RetExpr {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Return);
        // 暂时这样做
        let expr = Expr::eat(iter).ok();

        Ok(Self(expr.map(Box::new)))
    }
}
