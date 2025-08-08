use crate::{
    ast::{
        Visitable,
        expr::{Expr, LetExpr},
        pat::Pat,
        ty::Ty,
    },
    match_keyword,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Let(Box<LocalStmt>),
    // Item(P<Item>),
    // Expr(P<Expr>),
    // Semi(P<Expr>),
    // Empty,
    // MacCall(P<MacCallStmt>),
}

impl Visitable for Stmt {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut kind = None;
        kind = kind.or_else(|| LocalStmt::eat(iter).map(|x| StmtKind::Let(Box::new(x))));

        Some(Self { kind: kind? })
    }
}

#[derive(Debug)]
pub struct LocalStmt {
    pub pat: Box<Pat>,
    pub ty: Option<Box<Ty>>,
    pub kind: LocalKind,
}

impl Visitable for LocalStmt {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Let);
        let pat = Pat::eat(&mut using_iter)?;

        let ty = if using_iter.peek()?.token_type == TokenType::Colon {
            using_iter.next();
            Some(Box::new(Ty::eat(&mut using_iter)?))
        } else {
            None
        };

        let kind = if using_iter.peek()?.token_type == TokenType::Eq {
            using_iter.next();
            LocalKind::Init(Box::new(Expr::eat(&mut using_iter)?))
        } else {
            LocalKind::Decl
        };

        match_keyword!(using_iter, TokenType::Semi);

        iter.update(using_iter);
        Some(Self {
            pat: Box::new(pat),
            ty,
            kind,
        })
    }
}

#[derive(Debug)]
pub enum LocalKind {
    Decl,
    Init(Box<Expr>),
    // InitElse(Box<Expr>, Box<Block>),
}
