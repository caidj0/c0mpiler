use crate::{
    ast::{Ident, Visitable, generic::GenericArgs, ty::Ty},
    tokens::TokenType,
};

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug)]
pub struct PathSegment {
    pub ident: Ident,
    pub args: Option<Box<GenericArgs>>,
}

impl Visitable for Path {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();
        let mut segments = Vec::new();

        if using_iter.peek()?.token_type == TokenType::PathSep {
            // 上面的短路运算是合法的，因为这里不可能是末尾
            segments.push(PathSegment::default());
            using_iter.next();
        }

        segments.push(PathSegment::eat(&mut using_iter)?);

        while using_iter.peek()?.token_type == TokenType::PathSep {
            using_iter.next();
            segments.push(PathSegment::eat(&mut using_iter)?);
        }

        iter.update(using_iter);
        Some(Self { segments })
    }
}

impl Default for PathSegment {
    fn default() -> Self {
        Self {
            ident: Default::default(),
            args: Default::default(),
        }
    }
}

impl Visitable for PathSegment {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        let mut using_iter = iter.clone();

        let ident = using_iter.next()?.try_into().ok()?;
        let args = GenericArgs::eat(&mut using_iter);

        iter.update(using_iter);
        Some(Self {
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

impl Visitable for QSelf {
    fn eat(iter: &mut crate::lexer::TokenIter) -> Option<Self> {
        // TODO
        None
    }
}
