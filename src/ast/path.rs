use crate::{
    ast::{ASTResult, Eatable, Ident, OptionEatable, Span, Symbol, generic::GenericArgs, ty::Ty},
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

impl Path {
    pub fn is_self(&self) -> bool {
        self.segments.len() == 1 && {
            let first = self.segments.first().unwrap();
            first.args.is_none() && first.ident.symbol.is_self()
        }
    }

    pub fn get_symbol(&self) -> &Symbol {
        &self.segments.last().unwrap().ident.symbol
    }
}

#[derive(Debug)]
pub struct PathSegment {
    pub ident: Ident,
    pub args: Option<Box<GenericArgs>>,
}

impl Eatable for Path {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let begin = iter.get_pos();
        let mut segments = Vec::new();

        if iter.peek()?.token_type == TokenType::PathSep {
            // 上面的短路运算是合法的，因为这里不可能是末尾
            segments.push(PathSegment {
                ident: Ident {
                    symbol: Symbol::default(),
                    span: Span {
                        begin: iter.get_pos(),
                        end: iter.get_pos(),
                    },
                },
                args: None,
            });
            iter.advance();
        }

        segments.push(PathSegment::eat(iter)?);

        while iter.peek()?.token_type == TokenType::PathSep {
            iter.advance();
            segments.push(PathSegment::eat(iter)?);
        }

        Ok(Self {
            segments,
            span: Span {
                begin,
                end: iter.get_pos(),
            },
        })
    }
}

impl Eatable for PathSegment {
    fn eat_impl(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let ident = iter.next()?.try_into()?;
        let args = GenericArgs::try_eat(iter)?;

        Ok(Self {
            ident,
            args: args.map(Box::new),
        })
    }
}

#[derive(Debug)]
pub struct QSelf {
    pub ty: Box<Ty>,
    pub position: usize,
}

impl OptionEatable for QSelf {
    fn try_eat_impl(_iter: &mut crate::lexer::TokenIter) -> ASTResult<Option<Self>> {
        // TODO
        Ok(None)
    }
}
