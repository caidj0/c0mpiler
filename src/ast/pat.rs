use crate::{
    ast::{
        ASTError, ASTResult, BindingMode, Eatable, Ident, Mutability, OptionEatable,
        path::{Path, PathSegment, QSelf},
    },
    is_keyword, kind_check,
    lexer::TokenIter,
    loop_until, match_keyword, skip_keyword, skip_keyword_or_break,
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
    // Wild,
    Ident(IdentPat),
    Struct(StructPat),
    // TupleStruct(Option<P<QSelf>>, Path, ThinVec<P<Pat>>),
    // Or(ThinVec<P<Pat>>),
    Path(PathPat),
    // Tuple(ThinVec<P<Pat>>),
    // Box(P<Pat>),
    // Deref(P<Pat>),
    Ref(RefPat),
    // Expr(P<Expr>),
    // Range(Option<P<Expr>>, Option<P<Expr>>, Spanned<RangeEnd>),
    // Slice(ThinVec<P<Pat>>),
    // Rest,
    // Never,
    // Guard(P<Pat>, P<Expr>),
    // Paren(P<Pat>),
    // MacCall(P<MacCall>),
    // Err(ErrorGuaranteed),
}

impl Eatable for Pat {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let kind = kind_check!(iter, PatKind, Pat, (Struct, Path, Ident, Ref));

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
                    skip_keyword!(using_iter, TokenType::Comma);
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
