use crate::{
    ast::{
        ASTResult, Mutability, Visitable,
        expr::AnonConst,
        path::{Path, QSelf},
    },
    kind_check,
    lexer::TokenIter,
    match_keyword,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Ty {
    pub kind: TyKind,
}

#[derive(Debug)]
pub enum TyKind {
    Slice(SliceTy),
    Array(ArrayTy),
    // Ptr(MutTy),
    Ref(RefTy),
    // PinnedRef(Option<Lifetime>, MutTy),
    // FnPtr(P<FnPtrTy>),
    // UnsafeBinder(P<UnsafeBinderTy>),
    // Never,
    // Tup(ThinVec<P<Ty>>),
    Path(PathTy),
    // TraitObject(GenericBounds, TraitObjectSyntax),
    // ImplTrait(NodeId, GenericBounds),
    // Paren(P<Ty>),
    // Typeof(AnonConst),
    // Infer,
    ImplicitSelf,
    // MacCall(P<MacCall>),
    // CVarArgs,
    // Pat(P<Ty>, P<TyPat>),
    // Dummy,
    // Err(ErrorGuaranteed),
}

impl Visitable for Ty {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let kind = kind_check!(iter, TyKind, Ty, (Path, Array, Slice, Ref));

        Ok(Ty { kind: kind? })
    }
}

#[derive(Debug)]
pub struct PathTy(pub Option<Box<QSelf>>, pub Path);

impl Visitable for PathTy {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let qself = Option::<QSelf>::eat(&mut using_iter)?;
        let path = Path::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(qself.map(Box::new), path))
    }
}

#[derive(Debug)]
pub struct ArrayTy(pub Box<Ty>, pub AnonConst);

impl Visitable for ArrayTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenSqu);

        let ty = Ty::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::Semi);

        let anon = AnonConst::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::CloseSqu);

        iter.update(using_iter);
        Ok(Self(Box::new(ty), anon))
    }
}

#[derive(Debug)]
pub struct MutTy {
    pub ty: Box<Ty>,
    pub mutbl: Mutability,
}

impl Visitable for MutTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let mutbl = Mutability::eat(&mut using_iter)?;
        let ty = Ty::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self {
            ty: Box::new(ty),
            mutbl,
        })
    }
}

#[derive(Debug)]
pub struct RefTy(pub MutTy);

impl Visitable for RefTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::And);
        let ty = MutTy::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(ty))
    }
}

#[derive(Debug)]
pub struct SliceTy(pub Box<Ty>);

impl Visitable for SliceTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenSqu);

        let ty = Ty::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::CloseSqu);

        iter.update(using_iter);
        Ok(Self(Box::new(ty)))
    }
}
