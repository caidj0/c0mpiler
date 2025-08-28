use crate::{
    ast::{ASTResult, Eatable, NodeId, Span, expr::Expr, item::Item, pat::Pat, ty::Ty},
    is_keyword, match_keyword,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub id: NodeId,
    pub span: Span,
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
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

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
                    if is_keyword!(iter, TokenType::Semi) {
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

        Ok(Self {
            kind: kind?,
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

#[derive(Debug)]
pub struct LocalStmt {
    pub pat: Box<Pat>,
    pub ty: Option<Box<Ty>>,
    pub kind: LocalKind,
    pub id: NodeId,
    pub span: Span,
}

impl Eatable for LocalStmt {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        match_keyword!(iter, TokenType::Let);
        let pat = Pat::eat_no_alt(iter)?;

        let ty = if iter.peek()?.token_type == TokenType::Colon {
            iter.advance();
            Some(Box::new(Ty::eat(iter)?))
        } else {
            None
        };

        let kind = if iter.peek()?.token_type == TokenType::Eq {
            iter.advance();
            LocalKind::Init(Box::new(Expr::eat(iter)?))
        } else {
            LocalKind::Decl
        };

        match_keyword!(iter, TokenType::Semi);

        Ok(Self {
            pat: Box::new(pat),
            ty,
            kind,
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
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
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Semi);
        Ok(Self)
    }
}
