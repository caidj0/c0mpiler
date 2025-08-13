use crate::{
    ast::{ASTResult, Eatable, expr::Expr, item::Item, pat::Pat, ty::Ty},
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

impl Eatable for Stmt {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut kind = Err(crate::ast::ASTError::default());
        kind = kind.or_else(|err| {
            LocalStmt::eat(iter)
                .map(StmtKind::Let)
                .map_err(|err2| err.select(err2))
        });
        kind = kind.or_else(|err| {
            Item::eat(iter)
                .map(Box::new)
                .map(StmtKind::Item)
                .map_err(|err2| err.select(err2))
        });

        kind = kind.or_else(|err| {
            let expr_result = Expr::eat(iter);

            match expr_result {
                Ok(expr) => {
                    if expr.is_block() && iter.peek()?.token_type == TokenType::Semi {
                        Ok(StmtKind::Semi(Box::new(expr)))
                    } else {
                        Ok(StmtKind::Expr(Box::new(expr)))
                    }
                }
                Err(err2) => Err(err.select(err2)),
            }
        });

        kind = kind.or_else(|err| {
            EmptyStmt::eat(iter)
                .map(StmtKind::Empty)
                .map_err(|err2| err.select(err2))
        });

        Ok(Self { kind: kind? })
    }
}

#[derive(Debug)]
pub struct LocalStmt {
    pub pat: Box<Pat>,
    pub ty: Option<Box<Ty>>,
    pub kind: LocalKind,
}

impl Eatable for LocalStmt {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Let);
        let pat = Pat::eat_no_alt(&mut using_iter)?;

        let ty = if using_iter.peek()?.token_type == TokenType::Colon {
            using_iter.advance();
            Some(Box::new(Ty::eat(&mut using_iter)?))
        } else {
            None
        };

        let kind = if using_iter.peek()?.token_type == TokenType::Eq {
            using_iter.advance();
            LocalKind::Init(Box::new(Expr::eat(&mut using_iter)?))
        } else {
            LocalKind::Decl
        };

        match_keyword!(using_iter, TokenType::Semi);

        iter.update(using_iter);
        Ok(Self {
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

impl Eatable for EmptyStmt {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Semi);
        Ok(Self)
    }
}
