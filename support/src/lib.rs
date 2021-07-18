use std::fmt::{self, Write};

pub trait WritePretty {
  fn write(&self, f: &mut impl Write) -> fmt::Result;
}

impl WritePretty for () {
  fn write(&self, f: &mut impl Write) -> fmt::Result {
    Ok(())
  }
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileError {
  pub range: Range,
  pub message: String,
}

pub struct CompileErrorPrinter {
  line_offsets: Vec<usize>,
}

impl CompileErrorPrinter {
}