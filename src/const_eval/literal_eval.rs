use crate::{
    ast::expr::{LitExpr, LitKind},
    const_eval::{ConstEvalError, ConstEvalValue},
};

impl LitExpr {
    pub fn to_integer(&self, container: ConstEvalValue) -> Result<ConstEvalValue, ConstEvalError> {
        match self.kind {
            LitKind::Integer => {
                if let Some(suffix) = &self.suffix {
                    if (matches!(container, ConstEvalValue::U32(_)) && suffix != "u32")
                        || (matches!(container, ConstEvalValue::I32(_)) && suffix != "i32")
                        || (matches!(container, ConstEvalValue::USize(_)) && suffix != "usize")
                        || (matches!(container, ConstEvalValue::ISize(_)) && suffix != "isize")
                    {
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
                } // 此处不会出现负数

                Ok(match container {
                    ConstEvalValue::U32(_) => ConstEvalValue::U32(value),
                    ConstEvalValue::I32(_) => {
                        ConstEvalValue::I32(value.try_into().map_err(|_| ConstEvalError::Overflow)?)
                    }
                    ConstEvalValue::USize(_) => ConstEvalValue::USize(value),
                    ConstEvalValue::ISize(_) => ConstEvalValue::ISize(
                        value.try_into().map_err(|_| ConstEvalError::Overflow)?,
                    ),
                    _ => panic!("Impossible"),
                })
            }
            _ => Err(ConstEvalError::TypeMisMatch),
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
