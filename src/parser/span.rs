use std::ops::{BitOr, BitOrAssign};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Span {
    start: usize,
    len: usize,
}

impl Span {
    const fn end(&self) -> usize {
        self.start + self.len
    }
}

impl From<(usize, usize)> for Span {
    fn from((start, len): (usize, usize)) -> Self {
        Self { start, len }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        (value.start, value.len).into()
    }
}

#[expect(
    clippy::suspicious_arithmetic_impl,
    reason = "easier to find end than len"
)]
impl BitOr for Span {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        let start = self.start.min(rhs.start);
        Self {
            start,
            len: self.end().max(rhs.end()) - start,
        }
    }
}

impl BitOrAssign for Span {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}
