use crate::{
    ast::{
        ASTError, ASTResult, Eatable, Mutability, OptionEatable,
        expr::AnonConst,
        generic::GenericBounds,
        path::{Path, QSelf},
    },
    is_keyword, kind_check,
    lexer::TokenIter,
    loop_until, match_keyword, skip_keyword_or_break,
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
    Tup(TupTy),
    Path(PathTy),
    TraitObject(TraitObjectTy),
    ImplTrait(ImplTraitTy),
    // Paren(P<Ty>),
    // Typeof(AnonConst),
    Infer(InferTy),
    ImplicitSelf,
    // MacCall(P<MacCall>),
    // CVarArgs,
    // Pat(P<Ty>, P<TyPat>),
    // Dummy,
    // Err(ErrorGuaranteed),
}

impl Eatable for Ty {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let kind = kind_check!(
            iter,
            TyKind,
            Ty,
            (Path, Array, Slice, Ref, Tup, TraitObject, ImplTrait, Infer)
        );

        Ok(Ty { kind: kind? })
    }
}

#[derive(Debug)]
pub struct PathTy(pub Option<Box<QSelf>>, pub Path);

impl Eatable for PathTy {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let qself = QSelf::try_eat(&mut using_iter)?;
        let path = Path::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(qself.map(Box::new), path))
    }
}

#[derive(Debug)]
pub struct ArrayTy(pub Box<Ty>, pub AnonConst);

impl Eatable for ArrayTy {
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

impl Eatable for MutTy {
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

impl Eatable for RefTy {
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

impl Eatable for SliceTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenSqu);

        let ty = Ty::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::CloseSqu);

        iter.update(using_iter);
        Ok(Self(Box::new(ty)))
    }
}

#[derive(Debug)]
pub struct TupTy(pub Vec<Box<Ty>>);

impl Eatable for TupTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenPar);

        let mut ties = Vec::new();

        loop_until!(using_iter, TokenType::ClosePar, {
            ties.push(Box::new(Ty::eat(&mut using_iter)?));

            skip_keyword_or_break!(using_iter, TokenType::Comma, TokenType::CloseCurly);
        });

        iter.update(using_iter);
        Ok(Self(ties))
    }
}

#[derive(Debug)]
pub struct TraitObjectTy(pub GenericBounds);

impl Eatable for TraitObjectTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let t = using_iter.peek()?;
        let has_dyn = is_keyword!(using_iter, TokenType::Dyn);
        let bounds = GenericBounds::eat(&mut using_iter)?;
        if !has_dyn && bounds.0.len() == 1 {
            return Err(ASTError {
                kind: crate::ast::ASTErrorKind::MisMatch {
                    expected: "dyn".to_owned(),
                    actual: format!("{:?}", t.token_type.clone()),
                },
                pos: t.pos.clone(),
            });
        }

        iter.update(using_iter);
        Ok(Self(bounds))
    }
}

#[derive(Debug)]
pub struct ImplTraitTy(pub GenericBounds);

impl Eatable for ImplTraitTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Impl);
        let bounds = GenericBounds::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(bounds))
    }
}

#[derive(Debug)]
pub struct InferTy;

impl Eatable for InferTy {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Underscor);

        iter.update(using_iter);
        Ok(Self)
    }
}
