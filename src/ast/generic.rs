use crate::{
    ast::{ASTResult, Eatable, OptionEatable, item::TraitRef, path::Path, ty::Ty},
    loop_until, loop_while, match_prefix, skip_keyword_or_break,
    tokens::TokenType,
};

#[derive(Debug)]
pub enum GenericArgs {
    AngleBracketed(AngleBracketedArgs),
    // 以下两种用于模板参数为函数的情况
    // Parenthesized(ParenthesizedArgs),
    // ParenthesizedElided(Span),
}

impl OptionEatable for GenericArgs {
    fn try_eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        let angle_args = AngleBracketedArgs::try_eat(&mut using_iter)?;

        iter.update(using_iter);
        match angle_args {
            Some(x) => Ok(Some(GenericArgs::AngleBracketed(x))),
            None => Ok(None),
        }
    }
}

#[derive(Debug)]
pub struct AngleBracketedArgs {
    pub args: Vec<AngleBracketedArg>,
}

impl OptionEatable for AngleBracketedArgs {
    fn try_eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Option<Self>> {
        let mut using_iter = iter.clone();

        match_prefix!(using_iter, TokenType::Lt);

        let mut args = Vec::new();

        loop_until!(using_iter, TokenType::Gt, {
            args.push(AngleBracketedArg::eat(&mut using_iter)?);

            skip_keyword_or_break!(using_iter, TokenType::Comma, TokenType::Gt);
        });

        iter.update(using_iter);
        Ok(Some(Self { args }))
    }
}

#[derive(Debug)]
pub enum AngleBracketedArg {
    Arg(GenericArg),
    // Constraint(AssocItemConstraint),
}

impl Eatable for AngleBracketedArg {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let arg = GenericArg::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self::Arg(arg))
    }
}

#[derive(Debug)]
pub enum GenericArg {
    // Lifetime(Lifetime),
    Type(Box<Ty>),
    // Const(AnonConst),
}

impl Eatable for GenericArg {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let ty = Ty::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self::Type(Box::new(ty)))
    }
}

#[derive(Debug, Default)]
pub struct Generics {
    // pub params: ThinVec<GenericParam>,
    // pub where_clause: WhereClause,
}

impl Eatable for Generics {
    fn eat(_iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        // TODO
        Ok(Generics::default())
    }
}

#[derive(Debug, Default)]
pub struct GenericBounds(pub Vec<GenericBound>);

impl Eatable for GenericBounds {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let mut bounds = Vec::new();
        bounds.push(GenericBound::eat(&mut using_iter)?);
        loop_while!(using_iter, TokenType::Plus, {
            bounds.push(GenericBound::eat(&mut using_iter)?);
        });

        iter.update(using_iter);
        Ok(GenericBounds(bounds))
    }
}

#[derive(Debug)]
pub enum GenericBound {
    Trait(PolyTraitRef),
    // Outlives(Lifetime),
    // Use(ThinVec<PreciseCapturingArg>, Span),
}

impl Eatable for GenericBound {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let poly_trait_ref = PolyTraitRef::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self::Trait(poly_trait_ref))
    }
}

#[derive(Debug)]
pub struct PolyTraitRef {
    // pub bound_generic_params: Vec<GenericParam>,
    pub trait_ref: TraitRef,
}

impl Eatable for PolyTraitRef {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let trait_ref = TraitRef::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self { trait_ref })
    }
}

impl Eatable for TraitRef {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let path = Path::eat(&mut using_iter)?;

        iter.update(using_iter);
        Ok(Self { path })
    }
}
