use crate::{
    ast::{
        Ident, Visitable,
        expr::{AnonConst, BlockExpr, Expr},
        generic::Generics,
        pat::Pat,
        path::Path,
        ty::{Ty, TyKind},
    },
    lexer::TokenIter,
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
    Const(ConstItem),
    Fn(FnItem),
    // Mod(Safety, Ident, ModKind),
    // ForeignMod(ForeignMod),
    // GlobalAsm(Box<InlineAsm>),
    // TyAlias(Box<TyAlias>),
    Enum(EnumItem),
    Struct(StructItem),
    // Union(Ident, Generics, VariantData),
    // Trait(Box<Trait>),
    // TraitAlias(Ident, Generics, GenericBounds),
    Impl(ImplItem),
    // MacCall(P<MacCall>),
    // MacroDef(Ident, MacroDef),
    // Delegation(Box<Delegation>),
    // DelegationMac(Box<DelegationMac>),
}

impl Visitable for Item {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut kind: Option<ItemKind> = None;
        kind = kind.or_else(|| ConstItem::eat(iter).map(ItemKind::Const));
        kind = kind.or_else(|| FnItem::eat(iter).map(ItemKind::Fn));
        kind = kind.or_else(|| EnumItem::eat(iter).map(ItemKind::Enum));
        kind = kind.or_else(|| StructItem::eat(iter).map(ItemKind::Struct));
        kind = kind.or_else(|| ImplItem::eat(iter).map(ItemKind::Impl));

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

#[derive(Debug)]
pub struct ConstItem {
    pub ident: Ident,
    pub ty: Box<Ty>,
    pub expr: Option<Box<Expr>>,
}

impl Visitable for ConstItem {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Const);

        // Spec 中说此处可以为 "_"，但是只用于编译期求值，而编译期求值不被要求
        let ident = using_iter.next()?.try_into().ok()?;

        match_keyword!(using_iter, TokenType::Colon);

        let ty = Ty::eat(&mut using_iter)?;

        let expr = if using_iter.peek()?.token_type == TokenType::Eq {
            using_iter.next();
            Some(Box::new(Expr::eat(&mut using_iter)?))
        } else {
            None
        };

        match_keyword!(using_iter, TokenType::Semi);

        iter.update(using_iter);
        Some(Self {
            ident,
            ty: Box::new(ty),
            expr,
        })
    }
}

#[derive(Debug)]
pub struct EnumItem(pub Ident, pub Generics, pub Vec<Variant>);

impl Visitable for EnumItem {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Enum);
        let ident = using_iter.next()?.try_into().ok()?;
        let generics = Generics::eat(&mut using_iter).unwrap_or_default();

        match_keyword!(using_iter, TokenType::OpenCurly);
        let mut variants = Vec::new();
        while let Some(variant) = Variant::eat(&mut using_iter) {
            variants.push(variant);

            if using_iter.peek()?.token_type == TokenType::Comma {
                using_iter.next();
            } else {
                break;
            }
        }
        match_keyword!(using_iter, TokenType::CloseCurly);

        iter.update(using_iter);
        Some(Self(ident, generics, variants))
    }
}

#[derive(Debug)]
pub struct Variant {
    pub ident: Ident,
    pub data: VariantData,
    pub disr_expr: Option<AnonConst>,
}

impl Visitable for Variant {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let ident = using_iter.next()?.try_into().ok()?;
        let data = VariantData::eat(&mut using_iter)?;

        let disr_expr = if using_iter.peek()?.token_type == TokenType::Eq {
            using_iter.next();

            Some(AnonConst::eat(&mut using_iter)?)
        } else {
            None
        };

        iter.update(using_iter);
        Some(Self {
            ident,
            data,
            disr_expr,
        })
    }
}

#[derive(Debug)]
pub enum VariantData {
    Struct { fields: Vec<FieldDef> },
    Tuple(Vec<FieldDef>),
    Unit,
}

impl Visitable for VariantData {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let ret = match using_iter.peek()?.token_type {
            TokenType::OpenCurly => {
                using_iter.next();

                let mut fields = Vec::new();

                while let Some(def) = FieldDef::eat_with_ident(&mut using_iter, true) {
                    fields.push(def);

                    if using_iter.peek()?.token_type == TokenType::Comma {
                        using_iter.next();
                    } else {
                        break;
                    }
                }

                match_keyword!(using_iter, TokenType::CloseCurly);

                Self::Struct { fields }
            }
            TokenType::OpenPar => {
                using_iter.next();

                let mut fields = Vec::new();

                while let Some(def) = FieldDef::eat_with_ident(&mut using_iter, false) {
                    fields.push(def);

                    if using_iter.peek()?.token_type == TokenType::Comma {
                        using_iter.next();
                    } else {
                        break;
                    }
                }

                match_keyword!(using_iter, TokenType::ClosePar);

                Self::Tuple(fields)
            }
            _ => Self::Unit,
        };

        iter.update(using_iter);
        Some(ret)
    }
}

#[derive(Debug)]
pub struct FieldDef {
    pub ident: Option<Ident>,
    pub ty: Box<Ty>,
}

impl Visitable for FieldDef {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        Self::eat_with_ident(iter, true)
    }
}

impl FieldDef {
    fn eat_with_ident(iter: &mut TokenIter, has_ident: bool) -> Option<Self> {
        let mut using_iter = iter.clone();

        let ident = if has_ident {
            let t = Some(using_iter.next()?.try_into().ok()?);
            match_keyword!(using_iter, TokenType::Colon);
            t
        } else {
            None
        };

        let ty = Ty::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self {
            ident,
            ty: Box::new(ty),
        })
    }
}

#[derive(Debug)]
pub struct StructItem(pub Ident, pub Generics, pub VariantData);

impl Visitable for StructItem {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Struct);
        let ident = using_iter.next()?.try_into().ok()?;
        let generics = Generics::eat(&mut using_iter).unwrap_or_default();
        let variant_data = VariantData::eat(&mut using_iter)?;

        iter.update(using_iter);
        Some(Self(ident, generics, variant_data))
    }
}

#[derive(Debug)]
pub struct ImplItem {
    pub generics: Generics,
    pub of_trait: Option<TraitRef>,
    pub self_ty: Box<Ty>,
    pub items: Vec<Box<AssocItem>>,
}

impl Visitable for ImplItem {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Impl);
        let generics = Generics::eat(&mut using_iter).unwrap_or_default();
        let self_ty = Ty::eat(&mut using_iter)?;

        let (of_trait, self_ty) = if using_iter.peek()?.token_type == TokenType::For {
            using_iter.next();
            if let TyKind::Path(path_ty) = self_ty.kind {
                (
                    Some(TraitRef { path: path_ty.1 }),
                    Ty::eat(&mut using_iter)?,
                )
            } else {
                return None;
            }
        } else {
            (None, self_ty)
        };

        match_keyword!(using_iter, TokenType::OpenCurly);

        let mut items = Vec::new();
        while let Some(item) = AssocItem::eat(&mut using_iter) {
            items.push(Box::new(item));
        }

        match_keyword!(using_iter, TokenType::CloseCurly);

        iter.update(using_iter);
        Some(Self {
            generics,
            of_trait,
            self_ty: Box::new(self_ty),
            items,
        })
    }
}

#[derive(Debug)]
pub struct TraitRef {
    pub path: Path,
}

#[derive(Debug)]
pub struct AssocItem {
    pub kind: AssocItemKind,
}

#[derive(Debug)]
pub enum AssocItemKind {
    Const(ConstItem),
    Fn(FnItem),
    // Type(Box<TyAlias>),
    // MacCall(Box<MacCall>),
    // Delegation(Box<Delegation>),
    // DelegationMac(Box<DelegationMac>),
}

impl Visitable for AssocItem {
    fn eat(iter: &mut TokenIter) -> Option<Self> {
        let mut kind: Option<AssocItemKind> = None;
        kind = kind.or_else(|| ConstItem::eat(iter).map(AssocItemKind::Const));
        kind = kind.or_else(|| FnItem::eat(iter).map(AssocItemKind::Fn));

        Some(Self { kind: kind? })
    }
}
