use crate::{
    ast::{
        Ident, Visitable,
        pat::Pat,
        path::{Path, PathSegment, QSelf},
        stmt::Stmt,
    },
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
    Call(CallExpr),
    MethodCall(MethodCallExpr),
    // Tup(ThinVec<P<Expr>>),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Lit(LitExpr),
    // Cast(P<Expr>, P<Ty>),
    Let(LetExpr),
    If(IfExpr),
    While(WhileExpr),
    ForLoop(ForLoopExpr),
    Loop(LoopExpr),
    Match(MatchExpr),
    Block(BlockExpr), // TODO: 先忽略 label
    Assign(AssignExpr),
    // AssignOp(AssignOp, P<Expr>, P<Expr>),
    Field(FieldExpr),
    Index(IndexExpr),
    Range(RangeExpr),
    // Underscore,
    Path(PathExpr),
    // Break(Option<Label>, Option<P<Expr>>),
    // Continue(Option<Label>),
    // Ret(Option<P<Expr>>),
    // Struct(P<StructExpr>),
    Repeat(RepeatExpr),
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
        kind = kind.or_else(|| PathExpr::eat(iter).map(ExprKind::Path));
        kind = kind.or_else(|| ArrayExpr::eat(iter).map(ExprKind::Array));
        kind = kind.or_else(|| RepeatExpr::eat(iter).map(ExprKind::Repeat));
        kind = kind.or_else(|| UnaryExpr::eat(iter).map(ExprKind::Unary));
        kind = kind.or_else(|| LitExpr::eat(iter).map(ExprKind::Lit));
        kind = kind.or_else(|| LetExpr::eat(iter).map(ExprKind::Let));
        kind = kind.or_else(|| BlockExpr::eat(iter).map(ExprKind::Block));
        kind = kind.or_else(|| IfExpr::eat(iter).map(ExprKind::If));
        kind = kind.or_else(|| MatchExpr::eat(iter).map(ExprKind::Match));
        kind = kind.or_else(|| WhileExpr::eat(iter).map(ExprKind::While));
        kind = kind.or_else(|| ForLoopExpr::eat(iter).map(ExprKind::ForLoop));
        kind = kind.or_else(|| LoopExpr::eat(iter).map(ExprKind::Loop));

        kind = kind
            .or_else(|| RangeHelper::eat(iter).map(|x| ExprKind::Range(RangeExpr(None, x.0, x.1))));

        kind = kind.map(|mut expr1| {
            loop {
                if let Some(helper) = MethodCallHelper::eat(iter) {
                    expr1 = ExprKind::MethodCall(MethodCallExpr {
                        seg: helper.seg,
                        receiver: Box::new(Expr { kind: expr1 }),
                        args: helper.args,
                    })
                } else if let Some(helper) = FieldHelper::eat(iter) {
                    expr1 = ExprKind::Field(FieldExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else if let Some(helper) = CallHelper::eat(iter) {
                    expr1 = ExprKind::Call(CallExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else if let Some(helper) = IndexHelper::eat(iter) {
                    expr1 = ExprKind::Index(IndexExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else if let Some(helper) = BinaryHelper::eat_with_priority(iter, min_priority) {
                    expr1 = ExprKind::Binary(BinaryExpr(
                        helper.0,
                        Box::new(Expr { kind: expr1 }),
                        helper.1,
                    ))
                } else if let Some(helper) = RangeHelper::eat(iter) {
                    expr1 = ExprKind::Range(RangeExpr(
                        Some(Box::new(Expr { kind: expr1 })),
                        helper.0,
                        helper.1,
                    ))
                } else if let Some(helper) = AssignHelper::eat(iter) {
                    expr1 = ExprKind::Assign(AssignExpr(Box::new(Expr { kind: expr1 }), helper.0))
                } else {
                    break expr1;
                };
            }
        });

        Some(Expr { kind: kind? })
    }

    pub fn is_block(&self) -> bool {
        match self.kind {
            ExprKind::Block(_) | ExprKind::If(_) | ExprKind::Match(_) => true,
            _ => false,
        }
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

// 要求里面的 Expr 是编译期计算的
#[derive(Debug)]
pub struct AnonConst {
    pub value: Box<Expr>,
}

impl Visitable for AnonConst {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        Some(Self {
            value: Box::new(Expr::eat(iter)?),
        })
    }
}

#[derive(Debug)]
pub struct RepeatExpr(pub Box<Expr>, pub AnonConst);

impl Visitable for RepeatExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenSqu);

        let expr = Expr::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::Semi);

        let anon = AnonConst::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::CloseSqu);

        iter.update(using_iter);
        Some(Self(Box::new(expr), anon))
    }
}

#[derive(Debug)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

impl Visitable for BlockExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenCurly);

        let mut stmts = Vec::new();

        while let Some(stmt) = Stmt::eat(&mut using_iter) {
            stmts.push(stmt);
        }

        match_keyword!(using_iter, TokenType::CloseCurly);

        iter.update(using_iter);
        Some(Self { stmts })
    }
}

#[derive(Debug)]
pub struct IndexExpr(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct IndexHelper(pub Box<Expr>);

impl Visitable for IndexHelper {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenSqu);

        let expr = Expr::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::CloseSqu);

        iter.update(using_iter);
        Some(Self(Box::new(expr)))
    }
}

#[derive(Debug)]
pub struct PathExpr(pub Option<Box<QSelf>>, pub Path);

impl Visitable for PathExpr {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let qself = QSelf::eat(&mut using_iter);
        let path = Path::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self(qself.map(Box::new), path))
    }
}

// Rust 的赋值语句没有返回值，貌似不用关心赋值语句的结合性。
#[derive(Debug)]
pub struct AssignExpr(pub Box<Expr>, pub Box<Expr>);

#[derive(Debug)]
pub struct AssignHelper(pub Box<Expr>);

impl Visitable for AssignHelper {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Eq);

        let expr = Expr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self(Box::new(expr)))
    }
}

#[derive(Debug)]
pub struct CallExpr(pub Box<Expr>, pub Vec<Box<Expr>>);

#[derive(Debug)]
pub struct CallHelper(pub Vec<Box<Expr>>);

impl Visitable for CallHelper {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenPar);

        let mut exprs = Vec::new();

        while let Some(expr) = Expr::eat(&mut using_iter) {
            exprs.push(Box::new(expr));

            if using_iter.peek()?.token_type == TokenType::Comma {
                using_iter.next();
            } else {
                break;
            }
        }

        match_keyword!(using_iter, TokenType::ClosePar);

        iter.update(using_iter);
        Some(Self(exprs))
    }
}

#[derive(Debug)]
pub struct FieldExpr(pub Box<Expr>, pub Ident);

#[derive(Debug)]
pub struct FieldHelper(pub Ident);

impl Visitable for FieldHelper {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Dot);

        let ident = using_iter.next()?.try_into().ok()?;

        iter.update(using_iter);
        Some(Self(ident))
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
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Dot);

        let seg = PathSegment::eat(&mut using_iter)?;
        let call_helper = CallHelper::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self {
            seg,
            args: call_helper.0,
        })
    }
}

#[derive(Debug)]
pub struct IfExpr(pub Box<Expr>, pub Box<BlockExpr>, pub Option<Box<Expr>>);

impl Visitable for IfExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::If);

        let expr = Expr::eat(&mut using_iter)?;
        let block = BlockExpr::eat(&mut using_iter)?;

        let else_expr = if using_iter.peek()?.token_type == TokenType::Else {
            let mut using_iter2 = using_iter.clone();

            using_iter2.next();
            if let Some(else_expr) = Expr::eat(&mut using_iter2) {
                match else_expr.kind {
                    ExprKind::Block(_) | ExprKind::If(_) => {
                        using_iter.update(using_iter2);
                        Some(Box::new(else_expr))
                    }
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        };

        iter.update(using_iter);
        Some(Self(Box::new(expr), Box::new(block), else_expr))
    }
}

#[derive(Debug)]
pub struct MatchExpr(pub Box<Expr>, pub Vec<Arm>);

impl Visitable for MatchExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Match);
        let expr = Expr::eat(&mut using_iter)?;
        match_keyword!(using_iter, TokenType::OpenCurly);

        let mut arms = Vec::new();
        while let Some(arm) = Arm::eat(&mut using_iter) {
            let is_block = arm.body.is_block();
            arms.push(arm);

            if using_iter.peek()?.token_type == TokenType::Comma {
                using_iter.next();
            } else {
                if !is_block {
                    break;
                }
            }
        }

        match_keyword!(using_iter, TokenType::CloseCurly);

        iter.update(using_iter);
        Some(Self(Box::new(expr), arms))
    }
}

#[derive(Debug)]
pub struct Arm {
    pub pat: Box<Pat>,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
}

impl Visitable for Arm {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let pat = Pat::eat(&mut using_iter)?;
        let guard = if using_iter.peek()?.token_type == TokenType::If {
            using_iter.next();
            Some(Expr::eat(&mut using_iter)?)
        } else {
            None
        };
        match_keyword!(using_iter, TokenType::FatArrow);
        let body = Expr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self {
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
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::For);
        let pat = Pat::eat(&mut using_iter)?;
        match_keyword!(using_iter, TokenType::In);
        let loop_iter = Expr::eat(&mut using_iter)?;
        let block = BlockExpr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self {
            pat: Box::new(pat),
            iter: Box::new(loop_iter),
            body: Box::new(block),
        })
    }
}

#[derive(Debug)]
pub struct LoopExpr(pub Box<BlockExpr>);

impl Visitable for LoopExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Loop);
        let block = BlockExpr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self(Box::new(block)))
    }
}

#[derive(Debug)]
pub struct WhileExpr(pub Box<Expr>, pub Box<BlockExpr>);

impl Visitable for WhileExpr {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::While);
        let expr = Expr::eat(&mut using_iter)?;
        let block = BlockExpr::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self(Box::new(expr), Box::new(block)))
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

impl Visitable for RangeHelper {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let limits = match using_iter.next()?.token_type {
            TokenType::DotDot => RangeLimits::HalfOpen,
            TokenType::DotDotEq => RangeLimits::Closed,
            _ => return None,
        };

        let expr2 = Expr::eat(&mut using_iter);

        iter.update(using_iter);
        Some(Self(expr2.map(Box::new), limits))
    }
}

#[derive(Debug)]
pub enum RangeLimits {
    HalfOpen,
    Closed,
}
