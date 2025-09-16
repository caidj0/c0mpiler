use crate::{
    ast::{ASTResult, Eatable, OptionEatable, Span, item::TraitRef, path::Path, ty::Ty},
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
    fn try_eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Option<Self>> {
        let angle_args = AngleBracketedArgs::try_eat(iter)?;

        match angle_args {
            Some(x) => Ok(Some(GenericArgs::AngleBracketed(x))),
            None => Ok(None),
        }
    }
}

#[derive(Debug)]
pub struct AngleBracketedArgs {
    pub args: Vec<AngleBracketedArg>,
    pub span: Span,
}

impl OptionEatable for AngleBracketedArgs {
    fn try_eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Option<Self>> {
        let begin = iter.get_pos();

        match_prefix!(iter, TokenType::Lt);

        let mut args = Vec::new();

        // 与 exp1 < exp2 有二义性，以下是一个 work around
        if (|| -> Result<(), crate::ast::ASTError> {
            loop_until!(iter, TokenType::Gt, {
                args.push(AngleBracketedArg::eat(iter)?);

                skip_keyword_or_break!(iter, TokenType::Comma, TokenType::Gt);
            });
            Ok(())
        })()
        .is_err()
        {
            return Ok(None);
        }

        Ok(Some(Self {
            args,
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        }))
    }
}

#[derive(Debug)]
pub enum AngleBracketedArg {
    Arg(GenericArg),
    // Constraint(AssocItemConstraint),
}

impl Eatable for AngleBracketedArg {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let arg = GenericArg::eat(iter)?;

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
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let ty = Ty::eat(iter)?;

        Ok(Self::Type(Box::new(ty)))
    }
}

#[derive(Debug, Default)]
pub struct Generics {
    // pub params: ThinVec<GenericParam>,
    // pub where_clause: WhereClause,
}

impl Eatable for Generics {
    fn eat_impl(_iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        Ok(Generics::default())
    }
}

#[derive(Debug, Default)]
pub struct GenericBounds(pub Vec<GenericBound>);

impl Eatable for GenericBounds {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut bounds = Vec::new();
        bounds.push(GenericBound::eat(iter)?);
        loop_while!(iter, TokenType::Plus, {
            bounds.push(GenericBound::eat(iter)?);
        });

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
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let poly_trait_ref = PolyTraitRef::eat(iter)?;

        Ok(Self::Trait(poly_trait_ref))
    }
}

#[derive(Debug)]
pub struct PolyTraitRef {
    // pub bound_generic_params: Vec<GenericParam>,
    pub trait_ref: TraitRef,
    pub span: Span,
}

impl Eatable for PolyTraitRef {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();

        let trait_ref = TraitRef::eat(iter)?;

        Ok(Self {
            trait_ref,
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

impl Eatable for TraitRef {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let path = Path::eat(iter)?;

        Ok(Self { path })
    }
}
