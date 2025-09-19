use crate::{
    ast::expr::{LitExpr, LitKind},
    const_eval::{ConstEvalError, ConstEvalValue},
};

impl LitExpr {
    pub fn to_integer(&self) -> Result<ConstEvalValue, ConstEvalError> {
        match self.kind {
            LitKind::Integer => {
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
                    return Err(ConstEvalError::InvalidDigit);
                }

                let mut value: u32 = 0;
                for ch in digits.chars() {
                    let digit = ch.to_digit(radix).ok_or(ConstEvalError::InvalidDigit)?;
                    value = value
                        .checked_mul(radix)
                        .and_then(|v| v.checked_add(digit))
                        .ok_or(ConstEvalError::Overflow)?;
                } // 此处不会出现负数

                match &self.suffix {
                    Some(suffix) => match suffix.as_str() {
                        "u32" => Ok(ConstEvalValue::U32(value)),
                        "usize" => Ok(ConstEvalValue::USize(value)),
                        "i32" => Ok(ConstEvalValue::I32(
                            value.try_into().map_err(|_| ConstEvalError::Overflow)?,
                        )),
                        "isize" => Ok(ConstEvalValue::ISize(
                            value.try_into().map_err(|_| ConstEvalError::Overflow)?,
                        )),
                        _ => Err(ConstEvalError::IncorrectSuffix),
                    },
                    None => Ok(ConstEvalValue::Integer(value)),
                }
            }
            _ => Err(ConstEvalError::TypeMisMatch),
        }
    }
}

impl TryInto<ConstEvalValue> for &LitExpr {
    type Error = ConstEvalError;

    fn try_into(self) -> Result<ConstEvalValue, Self::Error> {
        match self.kind {
            LitKind::Bool => Ok(ConstEvalValue::Bool(self.try_into()?)),
            LitKind::Char => Ok(ConstEvalValue::Char(self.try_into()?)),
            LitKind::Integer => self.to_integer(),
            LitKind::Str | LitKind::StrRaw(_) | LitKind::CStr | LitKind::CStrRaw(_) => {
                Ok(ConstEvalValue::RefStr(self.try_into()?))
            }

            LitKind::Byte | LitKind::Float | LitKind::ByteStr | LitKind::ByteStrRaw(_) => {
                Err(ConstEvalError::NotSupportedExpr)
            }
        }
    }
}

impl TryInto<bool> for &LitExpr {
    type Error = ConstEvalError;

    fn try_into(self) -> Result<bool, Self::Error> {
        match self.kind {
            LitKind::Bool => {
                if self.symbol == "true" {
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            _ => Err(ConstEvalError::TypeMisMatch),
        }
    }
}

impl TryInto<char> for &LitExpr {
    type Error = ConstEvalError;

    fn try_into(self) -> Result<char, Self::Error> {
        match self.kind {
            LitKind::Char => Ok(self.symbol.chars().next().unwrap()),
            _ => Err(ConstEvalError::TypeMisMatch),
        }
    }
}

impl TryInto<String> for &LitExpr {
    type Error = ConstEvalError;

    fn try_into(self) -> Result<String, Self::Error> {
        match self.kind {
            LitKind::Str | LitKind::StrRaw(_) | LitKind::CStr | LitKind::CStrRaw(_) => {
                Ok(self.symbol.clone())
            }
            _ => Err(ConstEvalError::TypeMisMatch),
        }
    }
}
