#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Integer(u32);

impl Into<u32> for Integer {
    fn into(self) -> u32 {
        self.0
    }
}

impl Into<i32> for Integer {
    fn into(self) -> i32 {
        self.0 as i32
    }
}
