use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        ASTError, ASTErrorKind, ASTResult, Eatable, Ident, NodeId, OptionEatable, Span,
        expr::{AnonConst, BlockExpr, Expr},
        generic::{GenericBounds, Generics},
        pat::Pat,
        path::Path,
        ty::{Ty, TyKind},
    },
    is_keyword, kind_check,
    lexer::{Token, TokenIter},
    loop_until, match_keyword, match_prefix, skip_keyword_or_break,
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Item<K = ItemKind> {
    pub kind: K,
    pub id: NodeId,
    pub span: Span,
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

impl Eatable for Item {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let kind = kind_check!(
            iter,
            ItemKind,
            Item,
            (Const, Fn, Enum, Struct, Impl, Mod, Trait)
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
pub struct FnItem {
    pub ident: Ident,
    pub generics: Generics,
    pub sig: FnSig,
    pub body: Option<Box<BlockExpr>>,
}

impl Eatable for FnItem {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Fn);

        let ident = iter.next()?.try_into()?;
        let generics = Generics::eat(iter)?;
        let sig = FnSig::eat(iter)?;
        let body = match iter.peek()?.token_type {
            TokenType::Semi => {
                iter.advance();
                None
            }
            _ => Some(BlockExpr::eat(iter)?),
        };

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
    pub span: Span,
}

impl Eatable for FnSig {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();
        Ok(Self {
            decl: Box::new(FnDecl::eat(iter)?),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

#[derive(Debug)]
pub struct FnDecl {
    pub inputs: Vec<Param>,
    pub output: FnRetTy,
}

impl Eatable for FnDecl {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::OpenPar);

        let mut inputs = Vec::new();

        loop_until!(iter, TokenType::ClosePar, {
            inputs.push(Param::eat(iter)?);
            skip_keyword_or_break!(iter, TokenType::Comma, TokenType::ClosePar);
        });

        let output = FnRetTy::try_eat(iter)?.unwrap_or_default();

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

impl OptionEatable for FnRetTy {
    fn try_eat_impl(iter: &mut TokenIter) -> ASTResult<Option<Self>> {
        match_prefix!(iter, TokenType::RArrow);

        let ty = Ty::eat(iter)?;

        Ok(Some(FnRetTy::Ty(Box::new(ty))))
    }
}

#[derive(Debug)]
pub struct Param {
    pub ty: Box<Ty>,
    pub pat: Box<Pat>,
    pub id: NodeId,
    pub span: Span,
}

impl Eatable for Param {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let pat = Pat::eat_no_alt(iter)?;

        let (pat, ty) = if iter.peek()?.token_type != TokenType::Colon {
            Pat::to_self_pat_ty(pat, iter).ok_or(ASTError {
                kind: ASTErrorKind::MisMatch {
                    expected: stringify!($e).to_owned(),
                    actual: format!("{:?}", iter.peek()?.token_type),
                },
                pos: iter.peek()?.pos,
            })?
        } else {
            iter.advance();
            (pat, Ty::eat(iter)?)
        };

        Ok(Self {
            ty: Box::new(ty),
            pat: Box::new(pat),
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

#[derive(Debug)]
pub struct ConstItem {
    pub ident: Ident,
    pub ty: Box<Ty>,
    pub expr: Option<Box<Expr>>,
}

impl Eatable for ConstItem {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Const);

        // Spec 中说此处可以为 "_"，但是只用于编译期求值，而编译期求值不被要求
        let ident = iter.next()?.try_into()?;

        match_keyword!(iter, TokenType::Colon);

        let ty = Ty::eat(iter)?;

        let expr = if iter.peek()?.token_type == TokenType::Eq {
            iter.advance();
            Some(Box::new(Expr::eat(iter)?))
        } else {
            None
        };

        match_keyword!(iter, TokenType::Semi);

        Ok(Self {
            ident,
            ty: Box::new(ty),
            expr,
        })
    }
}

#[derive(Debug)]
pub struct EnumItem(pub Ident, pub Generics, pub Vec<Variant>);

impl Eatable for EnumItem {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Enum);
        let ident = iter.next()?.try_into()?;
        let generics = Generics::eat(iter)?;

        match_keyword!(iter, TokenType::OpenCurly);
        let mut variants = Vec::new();

        loop_until!(iter, TokenType::CloseCurly, {
            variants.push(Variant::eat(iter)?);
            skip_keyword_or_break!(iter, TokenType::Comma, TokenType::CloseCurly);
        });

        Ok(Self(ident, generics, variants))
    }
}

#[derive(Debug)]
pub struct Variant {
    pub ident: Ident,
    pub data: VariantData,
    pub disr_expr: Option<AnonConst>,
    pub id: NodeId,
    pub span: Span,
}

impl Eatable for Variant {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let ident = iter.next()?.try_into()?;
        let data = VariantData::eat(iter)?;

        let disr_expr = if iter.peek()?.token_type == TokenType::Eq {
            iter.advance();

            Some(AnonConst::eat(iter)?)
        } else {
            None
        };

        Ok(Self {
            ident,
            data,
            disr_expr,
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

#[derive(Debug, EnumAsInner)]
pub enum VariantData {
    Struct { fields: Vec<FieldDef> },
    Tuple(Vec<FieldDef>),
    Unit,
}

impl Eatable for VariantData {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let ret = match iter.peek()?.token_type {
            TokenType::OpenCurly => {
                iter.next().unwrap();

                let mut fields = Vec::new();

                loop_until!(iter, TokenType::CloseCurly, {
                    fields.push(FieldDef::eat_with_ident(iter, true)?);
                    skip_keyword_or_break!(iter, TokenType::Comma, TokenType::CloseCurly);
                });

                Self::Struct { fields }
            }
            TokenType::OpenPar => {
                iter.next().unwrap();

                let mut fields = Vec::new();

                loop_until!(iter, TokenType::ClosePar, {
                    fields.push(FieldDef::eat_with_ident(iter, false)?);
                    skip_keyword_or_break!(iter, TokenType::Comma, TokenType::ClosePar);
                });

                Self::Tuple(fields)
            }
            _ => Self::Unit,
        };

        Ok(ret)
    }
}

#[derive(Debug)]
pub struct FieldDef {
    pub ident: Option<Ident>,
    pub ty: Box<Ty>,
    pub id: NodeId,
    pub span: Span,
}

impl Eatable for FieldDef {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        Self::eat_with_ident(iter, true)
    }
}

impl FieldDef {
    fn eat_with_ident(iter: &mut TokenIter, has_ident: bool) -> ASTResult<Self> {
        let begin = iter.get_pos();

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
            id: iter.assign_id(),
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

#[derive(Debug)]
pub struct StructItem(pub Ident, pub Generics, pub VariantData);

impl Eatable for StructItem {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Struct);
        let ident = iter.next()?.try_into()?;
        let generics = Generics::eat(iter)?;
        let variant_data = VariantData::eat(iter)?;
        if !matches!(variant_data, VariantData::Struct { fields: _ }) {
            match_keyword!(iter, TokenType::Semi);
        }

        Ok(Self(ident, generics, variant_data))
    }
}

#[derive(Debug)]
pub struct ImplItem {
    pub generics: Generics,
    pub of_trait: Option<TraitRef>,
    pub self_ty: Box<Ty>,
    pub items: Vec<AssocItem>,
}

impl Eatable for ImplItem {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Impl);
        let generics = Generics::eat(iter)?;

        let self_ty_token = iter.peek()?;
        let self_ty = Ty::eat(iter)?;

        let (of_trait, self_ty) = if iter.peek()?.token_type == TokenType::For {
            iter.advance();
            if let TyKind::Path(path_ty) = self_ty.kind {
                (Some(TraitRef { path: path_ty.1 }), Ty::eat(iter)?)
            } else {
                return Err(crate::ast::ASTError {
                    kind: crate::ast::ASTErrorKind::MisMatch {
                        expected: "Path type".to_owned(),
                        actual: format!("{self_ty:?}"),
                    },
                    pos: self_ty_token.pos,
                });
            }
        } else {
            (None, self_ty)
        };

        match_keyword!(iter, TokenType::OpenCurly);

        let mut items = Vec::new();

        loop_until!(iter, TokenType::CloseCurly, {
            items.push(AssocItem::eat(iter)?);
        });

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

pub type AssocItem = Item<AssocItemKind>;

#[derive(Debug)]
pub enum AssocItemKind {
    Const(ConstItem),
    Fn(FnItem),
    // Type(Box<TyAlias>),
    // MacCall(Box<MacCall>),
    // Delegation(Box<Delegation>),
    // DelegationMac(Box<DelegationMac>),
}

impl Eatable for AssocItem {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();
        let kind = kind_check!(iter, AssocItemKind, Item, (Const, Fn));

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
pub struct ModItem(pub Ident, pub ModKind);

impl Eatable for ModItem {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Mod);
        let ident = iter.next()?.try_into()?;
        let kind = match iter.next()? {
            Token {
                token_type: TokenType::OpenCurly,
                lexeme: _,
                pos: _,
            } => {
                let mut items = Vec::new();
                loop_until!(iter, TokenType::CloseCurly, {
                    items.push(Box::new(Item::eat(iter)?));
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
                    pos: *pos,
                });
            }
        };

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
    pub items: Vec<AssocItem>,
}

impl Eatable for TraitItem {
    fn eat_impl(iter: &mut TokenIter) -> ASTResult<Self> {
        match_keyword!(iter, TokenType::Trait);
        let ident = iter.next()?.try_into()?;
        let generics = Generics::eat(iter)?;
        let bounds = if is_keyword!(iter, TokenType::Colon) {
            GenericBounds::eat(iter)?
        } else {
            GenericBounds::default()
        };

        match_keyword!(iter, TokenType::OpenCurly);
        let mut items = Vec::new();
        loop_until!(iter, TokenType::CloseCurly, {
            items.push(AssocItem::eat(iter)?);
        });

        Ok(Self {
            ident,
            generics,
            bounds,
            items,
        })
    }
}
