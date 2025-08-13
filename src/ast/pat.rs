use crate::{
    ast::{
        ASTError, ASTResult, BindingMode, Eatable, Ident, Mutability, OptionEatable,
        expr::{Expr, ExprKind, LitExpr, PathExpr, UnaryExpr},
        path::{Path, PathSegment, QSelf},
    },
    is_keyword, kind_check,
    lexer::{Token, TokenIter},
    loop_until, match_keyword, peek_keyword, skip_keyword_or_break,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Pat {
    pub kind: PatKind,
}

impl Pat {
    pub fn is_self(&self) -> bool {
        match &self.kind {
            PatKind::Path(PathPat(_, Path { segments })) => matches!(
                &segments[..],
                [PathSegment {
                    ident: Ident::PathSegment(TokenType::LSelfType),
                    args: _
                }]
            ),
            PatKind::Ref(ref_pat) => ref_pat.0.is_self(),
            PatKind::Ident(IdentPat(_, Ident::PathSegment(TokenType::LSelfType), _)) => true,
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
    // Or(ThinVec<P<Pat>>),
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
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let kind = kind_check!(
            iter,
            PatKind,
            Pat,
            (
                Struct, Range, Path, Ident, Ref, Tuple, Slice, Lit, Rest, Wild
            )
        );

        Ok(Self { kind: kind? })
    }
}

#[derive(Debug)]
pub struct IdentPat(pub BindingMode, pub Ident, pub Option<Box<Pat>>);

impl Eatable for IdentPat {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let bindmod = BindingMode::eat(&mut using_iter)?;
        let ident = using_iter.next()?.try_into()?;
        let pat = if using_iter.peek()?.token_type == TokenType::At {
            Some(Pat::eat(&mut using_iter)?)
        } else {
            None
        };

        iter.update(using_iter);
        Ok(Self(bindmod, ident, pat.map(Box::new)))
    }
}

#[derive(Debug)]
pub struct PathPat(pub Option<Box<QSelf>>, pub Path);

impl Eatable for PathPat {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let qself = QSelf::try_eat(&mut using_iter)?;
        let path = Path::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(qself.map(Box::new), path))
    }
}

#[derive(Debug)]
pub struct RefPat(pub Box<Pat>, pub Mutability);

impl Eatable for RefPat {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::And);
        let m = Mutability::eat(&mut using_iter)?;
        let pat = Pat::eat(&mut using_iter)?;

        iter.update(using_iter);
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
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let qself = QSelf::try_eat(&mut using_iter)?;
        let path = Path::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::OpenCurly);

        let mut fields = Vec::new();
        let mut rest = PatFieldsRest::None;

        loop_until!(using_iter, TokenType::CloseCurly, {
            match using_iter.peek()?.token_type {
                TokenType::DotDot => {
                    using_iter.advance();
                    rest = PatFieldsRest::Rest;
                    peek_keyword!(using_iter, TokenType::CloseCurly);
                    break;
                }
                _ => {
                    fields.push(PatField::eat(&mut using_iter)?);
                }
            }

            skip_keyword_or_break!(using_iter, TokenType::Comma, TokenType::CloseCurly);
        });

        iter.update(using_iter);
        Ok(Self(qself.map(Box::new), path, fields, rest))
    }
}

#[derive(Debug)]
pub struct PatField {
    pub ident: Ident,
    pub pat: Box<Pat>,
    pub is_shorthand: bool,
}

impl Eatable for PatField {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let pat = Pat::eat(&mut using_iter)?;

        let err = ASTError {
            kind: crate::ast::ASTErrorKind::MisMatchPat,
            pos: using_iter.peek()?.pos.clone(),
        };

        let (ident, pat, is_shorthand) = if is_keyword!(using_iter, TokenType::Colon) {
            (
                match pat.kind {
                    PatKind::Path(PathPat(None, Path { ref segments })) => match &segments[..] {
                        [PathSegment { ident, args: None }] => ident.clone(),
                        _ => return Err(err),
                    },
                    _ => return Err(err),
                },
                Pat::eat(&mut using_iter)?,
                false,
            )
        } else {
            (
                match pat.kind {
                    PatKind::Ident(IdentPat(_, ref ident, _)) => ident.clone(),
                    PatKind::Path(PathPat(None, Path { ref segments })) => match &segments[..] {
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

        iter.update(using_iter);
        Ok(Self {
            ident,
            pat: Box::new(pat),
            is_shorthand,
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
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenPar);

        let mut pats = Vec::new();
        loop_until!(using_iter, TokenType::ClosePar, {
            pats.push(Box::new(Pat::eat(&mut using_iter)?));
            skip_keyword_or_break!(using_iter, TokenType::Comma, TokenType::ClosePar);
        });

        iter.update(using_iter);
        Ok(Self(pats))
    }
}

#[derive(Debug)]
pub struct SlicePat(pub Vec<Box<Pat>>);

impl Eatable for SlicePat {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenSqu);

        let mut pats = Vec::new();
        loop_until!(using_iter, TokenType::CloseSqu, {
            pats.push(Box::new(Pat::eat(&mut using_iter)?));
            skip_keyword_or_break!(using_iter, TokenType::Comma, TokenType::CloseSqu);
        });

        iter.update(using_iter);
        Ok(Self(pats))
    }
}

#[derive(Debug)]
pub struct RangePat(pub Option<Box<Expr>>, pub Option<Box<Expr>>, pub RangeEnd);

impl Eatable for RangePat {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let get_expr = |using_iter: &mut TokenIter<'_>| {
            if let Ok(o) = LitPat::eat(using_iter) {
                return Some(o.0);
            }
            if let Ok(path) = PathPat::eat(using_iter) {
                return Some(Box::new(Expr {
                    kind: ExprKind::Path(PathExpr(path.0, path.1)),
                }));
            }
            None
        };

        let lit1 = get_expr(&mut using_iter);

        let end = match using_iter.next()? {
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

        let lit2 = get_expr(&mut using_iter);

        iter.update(using_iter);
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
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::DotDot);

        iter.update(using_iter);
        Ok(Self)
    }
}

#[derive(Debug)]
pub struct LitPat(pub Box<Expr>);

impl Eatable for LitPat {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let is_negative = is_keyword!(using_iter, TokenType::Minus);
        let literal = Box::new(Expr {
            kind: ExprKind::Lit(LitExpr::eat(&mut using_iter)?),
        });

        iter.update(using_iter);
        if is_negative {
            Ok(Self(Box::new(Expr {
                kind: ExprKind::Unary(UnaryExpr(crate::ast::expr::UnOp::Neg, literal)),
            })))
        } else {
            Ok(Self(literal))
        }
    }
}

#[derive(Debug)]
pub struct WildPat;

impl Eatable for WildPat {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Underscor);

        iter.update(using_iter);
        std::unimplemented!()
    }
}
