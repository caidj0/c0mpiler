use crate::ast::{Visitable, path::Path};

#[derive(Debug)]
pub struct Ty {
    pub kind: TyKind,
}

#[derive(Debug)]
pub enum TyKind {
    // Slice(P<Ty>),
    // Array(P<Ty>, AnonConst),
    // Ptr(MutTy),
    // Ref(Option<Lifetime>, MutTy),
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
    // ImplicitSelf,
    // MacCall(P<MacCall>),
    // CVarArgs,
    // Pat(P<Ty>, P<TyPat>),
    // Dummy,
    // Err(ErrorGuaranteed),
}

impl Visitable for Ty {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut kind = None;
        kind = kind.or_else(|| PathTy::eat(iter).map(TyKind::Path));

        Some(Ty { kind: kind? })
    }
}

#[derive(Debug)]
pub struct PathTy(pub Option<Box<QSelf>>, pub Path);

impl Visitable for PathTy {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let qself = QSelf::eat(&mut using_iter);
        let path = Path::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self(qself.map(Box::new), path))
    }
}

#[derive(Debug)]
pub struct QSelf {
    pub ty: Box<Ty>,
    pub position: usize,
}

impl Visitable for QSelf {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        // TODO
        None
    }
}
