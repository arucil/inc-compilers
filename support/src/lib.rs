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
  input: String,
  line_offsets: Vec<usize>,
}

impl CompileErrorPrinter {
  pub fn new(input: impl ToString) -> Self {
    let input = input.to_string();
    let line_offsets = Self::compute_line_offsets(&input);
    Self {
      input,
      line_offsets,
    }
  }

  pub fn print(err: CompileError) {
  }

  fn compute_line_offsets(input: &str) -> Vec<usize> {
    let mut offset = 0;
    let mut offsets = vec![];
    for line in input.split('\n') {
      offsets.push(offset);
      offset += line.len() + 1;
    }
    offsets
  }
}