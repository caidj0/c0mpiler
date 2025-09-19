#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Integer(u32);

impl From<Integer> for u32 {
    fn from(val: Integer) -> Self {
        val.0
    }
}

impl From<Integer> for i32 {
    fn from(val: Integer) -> Self {
        val.0 as i32
    }
}
