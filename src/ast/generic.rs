use crate::{
    ast::{ASTResult, OptionEatable, Eatable, ty::Ty},
    loop_until, match_prefix, skip_keyword,
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

            skip_keyword!(using_iter, TokenType::Comma);
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

#[derive(Debug)]
pub struct GenericBounds(pub Vec<GenericBound>);

impl Eatable for GenericBounds {
    fn eat(_iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        // TODO
        Ok(GenericBounds(vec![]))
    }
}

#[derive(Debug)]
pub enum GenericBound {
    // Trait(PolyTraitRef),
    // Outlives(Lifetime),
    // Use(ThinVec<PreciseCapturingArg>, Span),
}
