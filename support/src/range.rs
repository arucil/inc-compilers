#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Range {
  pub start: usize,
  pub end: usize,
}

impl From<(usize, usize)> for Range {
  fn from((start, end): (usize, usize)) -> Self {
    Self { start, end }
  }
}

impl Range {
  pub fn is_empty(&self) -> bool {
    self.start == self.end
  }
}