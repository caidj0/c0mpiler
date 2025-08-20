use crate::{
    ast::{
        ASTError, ASTResult, BindingMode, Eatable, Ident, Mutability, NodeId, OptionEatable, Span,
        Symbol,
        expr::{Expr, ExprKind, LitExpr, PathExpr, UnaryExpr},
        path::{Path, PathSegment, QSelf},
    },
    is_keyword, kind_check,
    lexer::{Token, TokenIter},
    loop_until, loop_while, match_keyword, peek_keyword, skip_keyword, skip_keyword_or_break,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Pat {
    pub kind: PatKind,
    pub id: NodeId,
    pub span: Span,
}

impl Pat {
    pub fn is_self(&self) -> bool {
        match &self.kind {
            PatKind::Path(PathPat(_, Path { segments, span: _ })) => {
                if let [
                    PathSegment {
                        ident: Ident { symbol, span: _ },
                        args: _,
                    },
                ] = &segments[..]
                {
                    symbol.is_self()
                } else {
                    false
                }
            }
            PatKind::Ref(ref_pat) => ref_pat.0.is_self(),
            PatKind::Ident(IdentPat(_, Ident { symbol, span: _ }, _)) => symbol.is_self(),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum PatKind {
    // Missing,
    Wild(WildPat),
    Ident(IdentPat),
    Struct(StructPat),
    // TupleStruct(Option<P<QSelf>>, Path, ThinVec<P<Pat>>),
    Or(OrPat),
    Path(PathPat),
    Tuple(TuplePat),
    // Box(P<Pat>),
    // Deref(P<Pat>),
    Ref(RefPat),
    Lit(LitPat),
    // Expr(P<Expr>),
    Range(RangePat),
    Slice(SlicePat),
    Rest(RestPat),
    // Never,
    // Guard(P<Pat>, P<Expr>),
    // Paren(P<Pat>),
    // MacCall(P<MacCall>),
    // Err(ErrorGuaranteed),
}

impl Eatable for Pat {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();
        skip_keyword!(iter, TokenType::Or);

        let mut pats = vec![Box::new(Pat::eat_no_alt(iter)?)];

        loop_while!(iter, TokenType::Or, {
            pats.push(Box::new(Pat::eat_no_alt(iter)?));
        });

        if pats.len() == 1 {
            Ok(*pats.pop().unwrap())
        } else {
            Ok(Self {
                kind: PatKind::Or(OrPat(pats)),
                id: iter.assign_id(),
                span: Span {
                    begin,
                    end: iter.get_pos(),
                },
            })
        }
    }
}

impl Pat {
    pub fn eat_no_alt(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();
        let kind = kind_check!(
            iter,
            PatKind,
            Pat,
            (
                Struct, Range, Path, Ident, Ref, Tuple, Slice, Lit, Rest, Wild
            )
        );

        Ok(Self {
            kind: kind?,
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }

    pub fn eat_no_range(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();
        let kind = kind_check!(
            iter,
            PatKind,
            Pat,
            (Struct, Path, Ident, Ref, Tuple, Slice, Lit, Rest, Wild)
        );

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
pub struct IdentPat(pub BindingMode, pub Ident, pub Option<Box<Pat>>);

impl Eatable for IdentPat {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let bindmod = BindingMode::eat(iter)?;
        let ident = iter.next()?.try_into()?;
        let pat = if iter.peek()?.token_type == TokenType::At {
            Some(Pat::eat_no_alt(iter)?)
        } else {
            None
        };

        Ok(Self(bindmod, ident, pat.map(Box::new)))
    }
}

#[derive(Debug)]
pub struct PathPat(pub Option<Box<QSelf>>, pub Path);

impl Eatable for PathPat {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let qself = QSelf::try_eat(iter)?;
        let path = Path::eat(iter)?;

        Ok(Self(qself.map(Box::new), path))
    }
}

#[derive(Debug)]
pub struct RefPat(pub Box<Pat>, pub Mutability);

impl Eatable for RefPat {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::And);
        let m = Mutability::eat(iter)?;
        let pat = Pat::eat_no_range(iter)?;

        Ok(Self(Box::new(pat), m))
    }
}

#[derive(Debug)]
pub struct StructPat(
    pub Option<Box<QSelf>>,
    pub Path,
    pub Vec<PatField>,
    pub PatFieldsRest,
);

impl Eatable for StructPat {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let qself = QSelf::try_eat(iter)?;
        let path = Path::eat(iter)?;

        match_keyword!(iter, TokenType::OpenCurly);

        let mut fields = Vec::new();
        let mut rest = PatFieldsRest::None;

        loop_until!(iter, TokenType::CloseCurly, {
            match iter.peek()?.token_type {
                TokenType::DotDot => {
                    iter.advance();
                    rest = PatFieldsRest::Rest;
                    peek_keyword!(iter, TokenType::CloseCurly);
                    break;
                }
                _ => {
                    fields.push(PatField::eat(iter)?);
                }
            }

            skip_keyword_or_break!(iter, TokenType::Comma, TokenType::CloseCurly);
        });

        Ok(Self(qself.map(Box::new), path, fields, rest))
    }
}

#[derive(Debug)]
pub struct PatField {
    pub ident: Ident,
    pub pat: Box<Pat>,
    pub is_shorthand: bool,
    pub id: NodeId,
    pub span: Span,
}

impl Eatable for PatField {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let pat = Pat::eat(iter)?;

        let err = ASTError {
            kind: crate::ast::ASTErrorKind::MisMatchPat,
            pos: iter.peek()?.pos.clone(),
        };

        let (ident, pat, is_shorthand) = if is_keyword!(iter, TokenType::Colon) {
            (
                match pat.kind {
                    PatKind::Path(PathPat(
                        None,
                        Path {
                            ref segments,
                            span: _,
                        },
                    )) => match &segments[..] {
                        [PathSegment { ident, args: None }] => ident.clone(),
                        _ => return Err(err),
                    },
                    _ => return Err(err),
                },
                Pat::eat(iter)?,
                false,
            )
        } else {
            (
                match pat.kind {
                    PatKind::Ident(IdentPat(_, ref ident, _)) => ident.clone(),
                    PatKind::Path(PathPat(
                        None,
                        Path {
                            ref segments,
                            span: _,
                        },
                    )) => match &segments[..] {
                        [PathSegment { ident, args: None }] => ident.clone(),
                        _ => return Err(err),
                    },
                    _ => {
                        return Err(err);
                    }
                },
                pat,
                true,
            )
        };

        Ok(Self {
            ident,
            pat: Box::new(pat),
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
pub enum PatFieldsRest {
    Rest,
    // Recovered(ErrorGuaranteed),
    None,
}

#[derive(Debug)]
pub struct TuplePat(pub Vec<Box<Pat>>);

impl Eatable for TuplePat {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenPar);

        let mut pats = Vec::new();
        loop_until!(iter, TokenType::ClosePar, {
            pats.push(Box::new(Pat::eat(iter)?));
            skip_keyword_or_break!(iter, TokenType::Comma, TokenType::ClosePar);
        });

        Ok(Self(pats))
    }
}

#[derive(Debug)]
pub struct SlicePat(pub Vec<Box<Pat>>);

impl Eatable for SlicePat {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenSqu);

        let mut pats = Vec::new();
        loop_until!(iter, TokenType::CloseSqu, {
            pats.push(Box::new(Pat::eat(iter)?));
            skip_keyword_or_break!(iter, TokenType::Comma, TokenType::CloseSqu);
        });

        Ok(Self(pats))
    }
}

#[derive(Debug)]
pub struct RangePat(pub Option<Box<Expr>>, pub Option<Box<Expr>>, pub RangeEnd);

impl Eatable for RangePat {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let get_expr = |iter: &mut TokenIter<'_>| {
            if let Ok(o) = LitPat::eat(iter) {
                return Some(o.0);
            }
            let begin = iter.get_pos();
            if let Ok(path) = PathPat::eat(iter) {
                return Some(Box::new(Expr {
                    kind: ExprKind::Path(PathExpr(path.0, path.1)),
                    span: Span {
                        begin,
                        end: iter.get_pos(),
                    },
                    id: iter.assign_id(),
                }));
            }
            None
        };

        let lit1 = get_expr(iter);

        let end = match iter.next()? {
            Token {
                token_type: TokenType::DotDot,
                ..
            } => RangeEnd::Excluded,
            Token {
                token_type: TokenType::DotDotEq,
                ..
            } => RangeEnd::Included,
            t => {
                return Err(ASTError {
                    kind: crate::ast::ASTErrorKind::MisMatch {
                        expected: "Comma".to_owned(),
                        actual: format!("{:?}", t.token_type.clone()),
                    },
                    pos: t.pos.clone(),
                });
            }
        };

        let lit2 = get_expr(iter);

        Ok(Self(lit1, lit2, end))
    }
}

#[derive(Debug)]
pub enum RangeEnd {
    Included,
    Excluded,
}

#[derive(Debug)]
pub struct RestPat;

impl Eatable for RestPat {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::DotDot);

        Ok(Self)
    }
}

#[derive(Debug)]
pub struct LitPat(pub Box<Expr>);

impl Eatable for LitPat {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let begin_neg = iter.get_pos();
        let is_negative = is_keyword!(iter, TokenType::Minus);

        let begin = iter.get_pos();
        let literal = Box::new(Expr {
            kind: ExprKind::Lit(LitExpr::eat(iter)?),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
            id: iter.assign_id(),
        });

        if is_negative {
            Ok(Self(Box::new(Expr {
                kind: ExprKind::Unary(UnaryExpr(crate::ast::expr::UnOp::Neg, literal)),
                span: Span {
                    begin: begin_neg,
                    end: iter.get_pos(),
                },
                id: iter.assign_id(),
            })))
        } else {
            Ok(Self(literal))
        }
    }
}

#[derive(Debug)]
pub struct WildPat;

impl Eatable for WildPat {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Underscor);

        std::unimplemented!()
    }
}

#[derive(Debug)]
pub struct OrPat(pub Vec<Box<Pat>>);
