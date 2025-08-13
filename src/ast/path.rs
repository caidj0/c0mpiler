use crate::{
    ast::{ASTResult, Eatable, Ident, OptionEatable, generic::GenericArgs, ty::Ty},
    tokens::TokenType,
};

#[derive(Debug, Default)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Default)]
pub struct PathSegment {
    pub ident: Ident,
    pub args: Option<Box<GenericArgs>>,
}

impl Eatable for Path {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();
        let mut segments = Vec::new();

        if using_iter.peek()?.token_type == TokenType::PathSep {
            // 上面的短路运算是合法的，因为这里不可能是末尾
            segments.push(PathSegment::default());
            using_iter.advance();
        }

        segments.push(PathSegment::eat(&mut using_iter)?);

        while using_iter.peek()?.token_type == TokenType::PathSep {
            using_iter.advance();
            segments.push(PathSegment::eat(&mut using_iter)?);
        }

        iter.update(using_iter);
        Ok(Self { segments })
    }
}

impl Eatable for PathSegment {
    fn eat(iter: &mut crate::lexer::TokenIter) -> ASTResult<Self> {
        let mut using_iter = iter.clone();

        let ident = using_iter.next()?.try_into()?;
        let args = GenericArgs::try_eat(&mut using_iter)?;

        iter.update(using_iter);
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
    fn try_eat(_iter: &mut crate::lexer::TokenIter) -> ASTResult<Option<Self>> {
        // TODO
        Ok(None)
    }
}
