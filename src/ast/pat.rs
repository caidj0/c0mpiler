use crate::{
    ast::{BindingMode, Ident, Visitable},
    lexer::TokenIter,
};

#[derive(Debug)]
pub struct Pat {
    kind: PatKind,
}

#[derive(Debug)]
pub enum PatKind {
    // Missing,
    // Wild,
    Ident(IdentPat),
    // Struct(Option<P<QSelf>>, Path, ThinVec<PatField>, PatFieldsRest),
    // TupleStruct(Option<P<QSelf>>, Path, ThinVec<P<Pat>>),
    // Or(ThinVec<P<Pat>>),
    // Path(Option<P<QSelf>>, Path),
    // Tuple(ThinVec<P<Pat>>),
    // Box(P<Pat>),
    // Deref(P<Pat>),
    // Ref(P<Pat>, Mutability),
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

impl Visitable for Pat {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut kind: Option<PatKind> = None;
        kind = kind.or_else(|| IdentPat::eat(iter).map(PatKind::Ident));

        Some(Self { kind: kind? })
    }
}

impl Pat {
    fn range_eat(iter: &mut TokenIter) -> Option<Self> {
        // TODO
        None
    }
}

#[derive(Debug)]
pub struct IdentPat(pub BindingMode, pub Ident, pub Option<Box<Pat>>);

impl Visitable for IdentPat {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let bindmod = BindingMode::eat(&mut using_iter).unwrap_or_default();
        let ident = using_iter.next()?.try_into().ok()?;
        let range_pat = Pat::range_eat(&mut using_iter);

        iter.update(using_iter);
        Some(Self(bindmod, ident, range_pat.map(Box::new)))
    }
}
