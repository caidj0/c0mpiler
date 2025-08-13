use crate::{
    ast::{
        ASTResult, BindingMode, Ident, Mutability, OptionEatable, Eatable,
        path::{Path, PathSegment, QSelf},
    },
    kind_check,
    lexer::TokenIter,
    match_keyword,
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
    // Struct(Option<P<QSelf>>, Path, ThinVec<PatField>, PatFieldsRest),
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
        let kind = kind_check!(iter, PatKind, Pat, (Path, Ident, Ref));

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
