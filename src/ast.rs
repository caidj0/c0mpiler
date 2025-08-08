use crate::lexer::TokenIter;

pub trait Visitable: Sized {
    #[allow(unused_variables)]
    fn visit(iter: &mut TokenIter) -> Option<Self> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    // Array(ThinVec<P<Expr>>),
    // ConstBlock(AnonConst),
    // Call(P<Expr>, ThinVec<P<Expr>>),
    // MethodCall(Box<MethodCall>),
    // Tup(ThinVec<P<Expr>>),
    // Binary(BinOp, P<Expr>, P<Expr>),
    Unary(Unary),
    Lit(Lit),
    // Cast(P<Expr>, P<Ty>),
    // Let(P<Pat>, P<Expr>, Span, Recovered),
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
    fn visit(iter: &mut TokenIter) -> Option<Self> {
        let mut kind: Option<ExprKind> = None;
        kind = kind.or(Unary::visit(iter).map(|x| ExprKind::Unary(x)));
        kind = kind.or(Lit::visit(iter).map(|x| ExprKind::Lit(x)));

        Some(Expr { kind: kind? })
    }
}

#[derive(Debug)]
pub struct Lit {
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

impl Visitable for Lit {
    fn visit(iter: &mut TokenIter) -> Option<Lit> {
        let mut using_iter = iter.clone();

        let ret = using_iter.next()?.try_into().ok()?;

        iter.update(using_iter);
        Some(ret)
    }
}

#[derive(Debug)]
pub struct Unary(pub UnOp, pub Box<Expr>);

impl Visitable for Unary {
    fn visit(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let unop = using_iter.next()?.try_into().ok()?;
        let expr = Expr::visit(&mut using_iter)?;

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
