use crate::{
    ast::{
        expr::AnonConst, generic::GenericBounds, path::{Path, QSelf}, ASTError, ASTResult, Eatable, Mutability, NodeId, OptionEatable, Span
    },
    is_keyword, kind_check,
    lexer::TokenIter,
    loop_until, match_keyword, skip_keyword_or_break,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Ty {
    pub kind: TyKind,
    pub id: NodeId,
    pub span: Span,
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

impl Ty {
    pub fn implicit_self(iter: &mut crate::lexer::TokenIter) -> Self {
        Ty {
            kind: TyKind::ImplicitSelf,
            id: iter.assign_id(),
            span: Span {
                begin: iter.get_pos(),
                end: iter.get_pos(),
            },
        }
    }
}

impl Eatable for Ty {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let kind = kind_check!(
            iter,
            TyKind,
            Ty,
            (Path, Array, Slice, Ref, Tup, TraitObject, ImplTrait, Infer)
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
pub struct PathTy(pub Option<Box<QSelf>>, pub Path);

impl Eatable for PathTy {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let qself = QSelf::try_eat(iter)?;
        let path = Path::eat(iter)?;

        Ok(Self(qself.map(Box::new), path))
    }
}

#[derive(Debug)]
pub struct ArrayTy(pub Box<Ty>, pub AnonConst);

impl Eatable for ArrayTy {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenSqu);

        let ty = Ty::eat(iter)?;

        match_keyword!(iter, TokenType::Semi);

        let anon = AnonConst::eat(iter)?;

        match_keyword!(iter, TokenType::CloseSqu);

        Ok(Self(Box::new(ty), anon))
    }
}

#[derive(Debug)]
pub struct MutTy {
    pub ty: Box<Ty>,
    pub mutbl: Mutability,
}

impl Eatable for MutTy {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let mutbl = Mutability::eat(iter)?;
        let ty = Ty::eat(iter)?;

        Ok(Self {
            ty: Box::new(ty),
            mutbl,
        })
    }
}

#[derive(Debug)]
pub struct RefTy(pub MutTy);

impl Eatable for RefTy {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::And);
        let ty = MutTy::eat(iter)?;

        Ok(Self(ty))
    }
}

#[derive(Debug)]
pub struct SliceTy(pub Box<Ty>);

impl Eatable for SliceTy {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenSqu);

        let ty = Ty::eat(iter)?;

        match_keyword!(iter, TokenType::CloseSqu);

        Ok(Self(Box::new(ty)))
    }
}

#[derive(Debug)]
pub struct TupTy(pub Vec<Box<Ty>>);

impl Eatable for TupTy {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenPar);

        let mut ties = Vec::new();

        loop_until!(iter, TokenType::ClosePar, {
            ties.push(Box::new(Ty::eat(iter)?));

            skip_keyword_or_break!(iter, TokenType::Comma, TokenType::CloseCurly);
        });

        Ok(Self(ties))
    }
}

#[derive(Debug)]
pub struct TraitObjectTy(pub GenericBounds);

impl Eatable for TraitObjectTy {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let t = iter.peek()?;
        let has_dyn = is_keyword!(iter, TokenType::Dyn);
        let bounds = GenericBounds::eat(iter)?;
        if !has_dyn && bounds.0.len() == 1 {
            return Err(ASTError {
                kind: crate::ast::ASTErrorKind::MisMatch {
                    expected: "dyn".to_owned(),
                    actual: format!("{:?}", t.token_type.clone()),
                },
                pos: t.pos.clone(),
            });
        }

        Ok(Self(bounds))
    }
}

#[derive(Debug)]
pub struct ImplTraitTy(pub GenericBounds);

impl Eatable for ImplTraitTy {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Impl);
        let bounds = GenericBounds::eat(iter)?;

        Ok(Self(bounds))
    }
}

#[derive(Debug)]
pub struct InferTy;

impl Eatable for InferTy {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Underscor);

        Ok(Self)
    }
}
