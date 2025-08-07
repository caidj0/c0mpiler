use crate::lexer::TokenIter;

pub struct Expr {
    pub kind: ExprKind,
}

pub enum ExprKind {
    Lit(Lit),
}

#[derive(Debug)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: String,
    pub suffix: Option<String>,
}

#[derive(Debug)]
pub enum LitKind {
    Bool,
    Byte,
    Char,
    Integer,
    Float,
    Str,
    StrRaw(u8),
    ByteStr,
    ByteStrRaw(u8),
    CStr,
    CStrRaw(u8),
}

impl Lit {
    pub fn visit(iter: &mut TokenIter) -> Option<Lit> {
        let mut using_iter = iter.clone();

        let ret = using_iter.next_token()?.try_into().ok()?;

        iter.update(using_iter);
        Some(ret)
    }
}
