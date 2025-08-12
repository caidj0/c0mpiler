use crate::{
    ast::{
        ASTError, ASTErrorKind, ASTResult, Ident, Visitable,
        expr::{AnonConst, BlockExpr, Expr},
        generic::{GenericBounds, Generics},
        pat::Pat,
        path::Path,
        ty::{Ty, TyKind},
    },
    kind_check,
    lexer::{Token, TokenIter},
    loop_until, match_keyword, match_prefix, skip_keyword,
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
    Mod(ModItem),
    // ForeignMod(ForeignMod),
    // GlobalAsm(Box<InlineAsm>),
    // TyAlias(Box<TyAlias>),
    Enum(EnumItem),
    Struct(StructItem),
    // Union(Ident, Generics, VariantData),
    Trait(TraitItem),
    // TraitAlias(Ident, Generics, GenericBounds),
    Impl(ImplItem),
    // MacCall(P<MacCall>),
    // MacroDef(Ident, MacroDef),
    // Delegation(Box<Delegation>),
    // DelegationMac(Box<DelegationMac>),
}

impl Visitable for Item {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let kind = kind_check!(
            iter,
            ItemKind,
            Item,
            (Const, Fn, Enum, Struct, Impl, Mod, Trait)
        );

        Ok(Self { kind: kind? })
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
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Fn);

        let ident = using_iter.next()?.try_into()?;
        let generics = Generics::eat(&mut using_iter)?;
        let sig = FnSig::eat(&mut using_iter)?;
        let body = match using_iter.peek()?.token_type {
            TokenType::Semi => {
                using_iter.advance();
                None
            }
            _ => Some(BlockExpr::eat(&mut using_iter)?),
        };

        iter.update(using_iter);
        Ok(Self {
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
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        Ok(Self {
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
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::OpenPar);

        let mut inputs = Vec::new();

        loop_until!(using_iter, TokenType::ClosePar, {
            inputs.push(Param::eat(&mut using_iter)?);
            skip_keyword!(using_iter, TokenType::Comma);
        });

        let output = Option::<FnRetTy>::eat(&mut using_iter)?.unwrap_or_default();

        iter.update(using_iter);
        Ok(Self { inputs, output })
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

impl Visitable for Option<FnRetTy> {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_prefix!(using_iter, TokenType::RArrow);

        let ty = Ty::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Some(FnRetTy::Ty(Box::new(ty))))
    }
}

#[derive(Debug)]
pub struct Param {
    pub ty: Box<Ty>,
    pub pat: Box<Pat>,
}

impl Visitable for Param {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let pat = Pat::eat(&mut using_iter)?;

        let ty = if using_iter.peek()?.token_type != TokenType::Colon {
            if pat.is_self() {
                Ty {
                    kind: TyKind::ImplicitSelf,
                }
            } else {
                return Err(ASTError {
                    kind: ASTErrorKind::MisMatch {
                        expected: stringify!($e).to_owned(),
                        actual: format!("{:?}", using_iter.peek()?.token_type),
                    },
                    pos: using_iter.peek()?.pos.clone(),
                });
            }
        } else {
            using_iter.advance();
            Ty::eat(&mut using_iter)?
        };

        iter.update(using_iter);
        Ok(Self {
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
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Const);

        // Spec 中说此处可以为 "_"，但是只用于编译期求值，而编译期求值不被要求
        let ident = using_iter.next()?.try_into()?;

        match_keyword!(using_iter, TokenType::Colon);

        let ty = Ty::eat(&mut using_iter)?;

        let expr = if using_iter.peek()?.token_type == TokenType::Eq {
            using_iter.advance();
            Some(Box::new(Expr::eat(&mut using_iter)?))
        } else {
            None
        };

        match_keyword!(using_iter, TokenType::Semi);

        iter.update(using_iter);
        Ok(Self {
            ident,
            ty: Box::new(ty),
            expr,
        })
    }
}

#[derive(Debug)]
pub struct EnumItem(pub Ident, pub Generics, pub Vec<Variant>);

impl Visitable for EnumItem {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Enum);
        let ident = using_iter.next()?.try_into()?;
        let generics = Generics::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::OpenCurly);
        let mut variants = Vec::new();

        loop_until!(using_iter, TokenType::CloseCurly, {
            variants.push(Variant::eat(&mut using_iter)?);
            skip_keyword!(using_iter, TokenType::Comma);
        });

        iter.update(using_iter);
        Ok(Self(ident, generics, variants))
    }
}

#[derive(Debug)]
pub struct Variant {
    pub ident: Ident,
    pub data: VariantData,
    pub disr_expr: Option<AnonConst>,
}

impl Visitable for Variant {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let ident = using_iter.next()?.try_into()?;
        let data = VariantData::eat(&mut using_iter)?;

        let disr_expr = if using_iter.peek()?.token_type == TokenType::Eq {
            using_iter.advance();

            Some(AnonConst::eat(&mut using_iter)?)
        } else {
            None
        };

        iter.update(using_iter);
        Ok(Self {
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
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let ret = match using_iter.peek()?.token_type {
            TokenType::OpenCurly => {
                using_iter.next().unwrap();

                let mut fields = Vec::new();

                loop_until!(using_iter, TokenType::CloseCurly, {
                    fields.push(FieldDef::eat_with_ident(&mut using_iter, true)?);
                    skip_keyword!(using_iter, TokenType::Comma);
                });

                Self::Struct { fields }
            }
            TokenType::OpenPar => {
                using_iter.next().unwrap();

                let mut fields = Vec::new();

                loop_until!(using_iter, TokenType::ClosePar, {
                    fields.push(FieldDef::eat_with_ident(&mut using_iter, false)?);
                    skip_keyword!(using_iter, TokenType::Comma);
                });

                Self::Tuple(fields)
            }
            _ => Self::Unit,
        };

        iter.update(using_iter);
        Ok(ret)
    }
}

#[derive(Debug)]
pub struct FieldDef {
    pub ident: Option<Ident>,
    pub ty: Box<Ty>,
}

impl Visitable for FieldDef {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        Self::eat_with_ident(iter, true)
    }
}

impl FieldDef {
    fn eat_with_ident(iter: &mut TokenIter, has_ident: bool) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let ident = if has_ident {
            let t = Some(using_iter.next()?.try_into()?);
            match_keyword!(using_iter, TokenType::Colon);
            t
        } else {
            None
        };

        let ty = Ty::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self {
            ident,
            ty: Box::new(ty),
        })
    }
}

#[derive(Debug)]
pub struct StructItem(pub Ident, pub Generics, pub VariantData);

impl Visitable for StructItem {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Struct);
        let ident = using_iter.next()?.try_into()?;
        let generics = Generics::eat(&mut using_iter)?;
        let variant_data = VariantData::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self(ident, generics, variant_data))
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
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Impl);
        let generics = Generics::eat(&mut using_iter)?;

        let self_ty_token = using_iter.peek()?;
        let self_ty = Ty::eat(&mut using_iter)?;

        let (of_trait, self_ty) = if using_iter.peek()?.token_type == TokenType::For {
            using_iter.advance();
            if let TyKind::Path(path_ty) = self_ty.kind {
                (
                    Some(TraitRef { path: path_ty.1 }),
                    Ty::eat(&mut using_iter)?,
                )
            } else {
                return Err(crate::ast::ASTError {
                    kind: crate::ast::ASTErrorKind::MisMatch {
                        expected: "Path type".to_owned(),
                        actual: format!("{self_ty:?}"),
                    },
                    pos: self_ty_token.pos.clone(),
                });
            }
        } else {
            (None, self_ty)
        };

        match_keyword!(using_iter, TokenType::OpenCurly);

        let mut items = Vec::new();

        loop_until!(using_iter, TokenType::CloseCurly, {
            items.push(Box::new(AssocItem::eat(&mut using_iter)?));
        });

        iter.update(using_iter);
        Ok(Self {
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
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let kind = kind_check!(iter, AssocItemKind, Item, (Const, Fn));

        Ok(Self { kind: kind? })
    }
}

#[derive(Debug)]
pub struct ModItem(pub Ident, pub ModKind);

impl Visitable for ModItem {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Mod);
        let ident = using_iter.next()?.try_into()?;
        let kind = match using_iter.next()? {
            Token {
                token_type: TokenType::OpenCurly,
                lexeme: _,
                pos: _,
            } => {
                let mut items = Vec::new();
                loop_until!(using_iter, TokenType::CloseCurly, {
                    items.push(Box::new(Item::eat(&mut using_iter)?));
                });
                ModKind::Loaded(items, Inline::Yes)
            }
            Token {
                token_type: TokenType::Semi,
                lexeme: _,
                pos: _,
            } => ModKind::Unloaded,
            Token {
                token_type,
                lexeme: _,
                pos,
            } => {
                return Err(ASTError {
                    kind: ASTErrorKind::MisMatch {
                        expected: "{ or ;".to_owned(),
                        actual: format!("{:?}", token_type.clone()),
                    },
                    pos: pos.clone(),
                });
            }
        };

        iter.update(using_iter);
        Ok(Self(ident, kind))
    }
}

#[derive(Debug)]
pub enum ModKind {
    Loaded(Vec<Box<Item>>, Inline),
    Unloaded,
}

#[derive(Debug)]
pub enum Inline {
    Yes,
    No,
}

#[derive(Debug)]
pub struct TraitItem {
    pub ident: Ident,
    pub generics: Generics,
    pub bounds: GenericBounds,
    pub items: Vec<Box<AssocItem>>,
}

impl Visitable for TraitItem {
    fn eat(iter: &mut TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        match_keyword!(using_iter, TokenType::Trait);
        let ident = using_iter.next()?.try_into()?;
        let generics = Generics::eat(&mut using_iter)?;
        let bounds = GenericBounds::eat(&mut using_iter)?;

        match_keyword!(using_iter, TokenType::OpenCurly);
        let mut items = Vec::new();
        loop_until!(using_iter, TokenType::CloseCurly, {
            items.push(Box::new(AssocItem::eat(&mut using_iter)?));
        });

        iter.update(using_iter);
        Ok(Self {
            ident,
            generics,
            bounds,
            items,
        })
    }
}
