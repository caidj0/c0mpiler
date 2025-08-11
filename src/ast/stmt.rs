use crate::{
    ast::{Visitable, expr::Expr, item::Item, pat::Pat, ty::Ty},
    match_keyword,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Let(LocalStmt),
    Item(Box<Item>),
    Expr(Box<Expr>),
    Semi(Box<Expr>),
    Empty(EmptyStmt),
    // MacCall(P<MacCallStmt>),
}

impl Visitable for Stmt {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut kind = None;
        kind = kind.or_else(|| LocalStmt::eat(iter).map(StmtKind::Let));
        kind = kind.or_else(|| Item::eat(iter).map(Box::new).map(StmtKind::Item));

        kind = kind.or_else(|| {
            let expr = Box::new(Expr::eat(iter)?);

            if expr.is_block() && iter.peek()?.token_type == TokenType::Semi {
                Some(StmtKind::Semi(expr))
            } else {
                Some(StmtKind::Expr(expr))
            }
        });

        kind = kind.or_else(|| EmptyStmt::eat(iter).map(StmtKind::Empty));

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

#[derive(Debug)]
pub struct EmptyStmt;

impl Visitable for EmptyStmt {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        match_keyword!(iter, TokenType::Semi);
        Some(Self)
    }
}
