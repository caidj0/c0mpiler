use crate::{
    ast::{Ident, Visitable, expr::BlockExpr, generic::Generics, pat::Pat, ty::Ty},
    match_keyword,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    // ExternCrate(Option<Symbol>, Ident),
    // Use(UseTree),
    // Static(Box<StaticItem>),
    // Const(Box<ConstItem>),
    Fn(FnItem),
    // Mod(Safety, Ident, ModKind),
    // ForeignMod(ForeignMod),
    // GlobalAsm(Box<InlineAsm>),
    // TyAlias(Box<TyAlias>),
    // Enum(Ident, Generics, EnumDef),
    // Struct(Ident, Generics, VariantData),
    // Union(Ident, Generics, VariantData),
    // Trait(Box<Trait>),
    // TraitAlias(Ident, Generics, GenericBounds),
    // Impl(Box<Impl>),
    // MacCall(P<MacCall>),
    // MacroDef(Ident, MacroDef),
    // Delegation(Box<Delegation>),
    // DelegationMac(Box<DelegationMac>),
}

impl Visitable for Item {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut kind: Option<ItemKind> = None;
        kind = kind.or_else(|| FnItem::eat(iter).map(ItemKind::Fn));

        Some(Self { kind: kind? })
    }
}

#[derive(Debug)]
pub struct FnItem {
    pub ident: Ident,
    pub generics: Generics,
    pub sig: FnSig,
    pub body: Option<Box<BlockExpr>>,
}

impl Visitable for FnItem {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Fn);

        let ident = using_iter.next()?.try_into().ok()?;
        let generics = Generics::eat(&mut using_iter).unwrap_or_default();
        let sig = FnSig::eat(&mut using_iter)?;
        let body = BlockExpr::eat(&mut using_iter);

        iter.update(using_iter);
        Some(Self {
            ident,
            generics,
            sig,
            body: body.map(Box::new),
        })
    }
}

#[derive(Debug)]
pub struct FnSig {
    pub decl: Box<FnDecl>,
}

impl Visitable for FnSig {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        Some(Self {
            decl: Box::new(FnDecl::eat(iter)?),
        })
    }
}

#[derive(Debug)]
pub struct FnDecl {
    pub inputs: Vec<Param>,
    pub output: FnRetTy,
}

impl Visitable for FnDecl {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenPar);

        let mut inputs = Vec::new();

        while let Some(param) = Param::eat(&mut using_iter) {
            inputs.push(param);

            if using_iter.peek()?.token_type == TokenType::Comma {
                using_iter.next();
            } else {
                break;
            }
        }

        match_keyword!(using_iter, TokenType::ClosePar);

        let output = FnRetTy::eat(&mut using_iter).unwrap_or_default();

        iter.update(using_iter);
        Some(Self { inputs, output })
    }
}

#[derive(Debug)]
pub enum FnRetTy {
    Default,
    Ty(Box<Ty>),
}

impl Default for FnRetTy {
    fn default() -> Self {
        Self::Default
    }
}

impl Visitable for FnRetTy {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::RArrow);

        let ty = Ty::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self::Ty(Box::new(ty)))
    }
}

#[derive(Debug)]
pub struct Param {
    pub ty: Box<Ty>,
    pub pat: Box<Pat>,
}

impl Visitable for Param {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let pat = Pat::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::Colon);
        let ty = Ty::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self {
            ty: Box::new(ty),
            pat: Box::new(pat),
        })
    }
}
