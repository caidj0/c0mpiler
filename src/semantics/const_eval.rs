use crate::{
    ast::expr::{Expr, ExprKind, LitExpr, LitKind},
    semantics::SemanticError,
};

#[derive(Debug)]
pub enum ConstEvalError {
    NotSupportedExpr,
    IncorrectSuffix,
    Overflow,
    InvaildDigit,
    TypeMisMatch,
}

impl TryInto<u32> for &Expr {
    type Error = SemanticError;

    fn try_into(self) -> Result<u32, Self::Error> {
        match &self.kind {
            ExprKind::Lit(lit_expr) => lit_expr.try_into().map_err(SemanticError::ConstEvalError),
            _ => Err(SemanticError::ConstEvalError(
                ConstEvalError::NotSupportedExpr,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lit(symbol: &str, suffix: Option<&str>) -> LitExpr {
        LitExpr {
            kind: LitKind::Integer,
            symbol: symbol.to_owned(),
            suffix: suffix.map(|s| s.to_owned()),
        }
    }

    #[test]
    fn parse_decimal() {
        let l = lit("12345", None);
        let v: u32 = (&l).try_into().unwrap();
        assert_eq!(v, 12345);
    }

    #[test]
    fn parse_hex() {
        let l = lit("0xFF", None);
        let v: u32 = (&l).try_into().unwrap();
        assert_eq!(v, 255);
    }

    #[test]
    fn parse_octal() {
        let l = lit("0o77", None);
        let v: u32 = (&l).try_into().unwrap();
        assert_eq!(v, 63);
    }

    #[test]
    fn parse_binary() {
        let l = lit("0b1010", None);
        let v: u32 = (&l).try_into().unwrap();
        assert_eq!(v, 10);
    }

    #[test]
    fn parse_underscores() {
        let l = lit("1_000_000", None);
        let v: u32 = (&l).try_into().unwrap();
        assert_eq!(v, 1_000_000);
    }

    #[test]
    fn overflow_detected() {
        // value larger than u32::MAX
        let big = format!("{}", (u64::from(u32::MAX) + 1));
        let l = lit(&big, None);
        let r: Result<u32, _> = (&l).try_into();
        assert!(r.is_err());
    }

    #[test]
    fn suffix_rejection() {
        let l = lit("123", Some("i32"));
        let r: Result<u32, _> = (&l).try_into();
        assert!(r.is_err());
    }
}

impl TryInto<u32> for &LitExpr {
    type Error = ConstEvalError;

    fn try_into(self) -> Result<u32, Self::Error> {
        match self.kind {
            LitKind::Integer => {
                if let Some(suffix) = &self.suffix {
                    if suffix != "u32" {
                        return Err(ConstEvalError::IncorrectSuffix);
                    }
                }
                let raw = self.symbol.replace('_', "");

                let (radix, digits) = if let Some(rest) = raw.strip_prefix("0b") {
                    (2u32, rest)
                } else if let Some(rest) = raw.strip_prefix("0o") {
                    (8u32, rest)
                } else if let Some(rest) = raw.strip_prefix("0x") {
                    (16u32, rest)
                } else {
                    (10u32, raw.as_str())
                };

                if digits.is_empty() {
                    return Err(ConstEvalError::InvaildDigit);
                }

                let mut value: u32 = 0;
                for ch in digits.chars() {
                    let digit = ch.to_digit(radix).ok_or(ConstEvalError::InvaildDigit)?;
                    value = value
                        .checked_mul(radix)
                        .and_then(|v| v.checked_add(digit))
                        .ok_or(ConstEvalError::Overflow)?;
                }

                Ok(value)
            }
            _ => Err(ConstEvalError::TypeMisMatch),
        }
    }
}
