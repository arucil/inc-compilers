use std::path::{Path, PathBuf};
use crate::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileError {
  pub range: Range,
  pub message: String,
}

pub struct CompileErrorPrinter {
  path: PathBuf,
  line_offsets: Vec<usize>,
}

impl CompileErrorPrinter {
  pub fn new<P: AsRef<Path>, S: ToString>(file: P, input: S) -> Self {
    let input = input.to_string();
    let line_offsets = Self::compute_line_offsets(&input);
    Self {
      path: file.as_ref().to_owned(),
      line_offsets,
    }
  }

  fn get_location(&self, position: usize) -> (usize, usize) {
    let mut low = 0;
    let mut high = self.line_offsets.len();
    while low < high {
      let mid = (low + high) / 2;
      let x = self.line_offsets[mid];
      if x > position {
        high = mid;
      } else {
        low = mid + 1;
      }
    }
    let line = low - 1;
    let offset = self.line_offsets[line];
    let column = position - offset;
    (line, column)
  }

  pub fn print(&self, err: &CompileError) {
    let start = self.get_location(err.range.start);
    let end = self.get_location(if err.range.is_empty() {
      err.range.end
    } else {
      err.range.end - 1
    });
    eprintln!(
      "error at {}:{}:{}{}\n    {}",
      self.path.display(),
      start.0 + 1,
      start.1 + 1,
      if end.0 == start.0 {
        format!(" - {}", end.1 + 2)
      } else {
        format!(" - {}:{}", end.0 + 1, end.1 + 2)
      },
      err.message
    );
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

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn line_offsets() {
    let printer = CompileErrorPrinter::new("foo", "abcde\nabc\n\nfoobar 1");
    assert_eq!(printer.line_offsets, vec![0, 6, 10, 11]);
  }

  #[test]
  fn location() {
    let printer = CompileErrorPrinter::new("foo", "abcde\nabc\n\nfoobar 1");
    assert_eq!(printer.get_location(3), (0, 3));
    assert_eq!(printer.get_location(10), (2, 0));
    assert_eq!(printer.get_location(11), (3, 0));
    assert_eq!(printer.get_location(12), (3, 1));
  }
}