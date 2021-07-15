#![feature(never_type, box_syntax)]

pub mod parser;

pub use parser::{parse, ParseError, Result};

pub type Range = (usize, usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
  body: Vec<(Range, Exp)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Exp {
  Int(i64),
  Prim {
    op: (Range, String),
    args: Vec<(Range, Exp)>,
  },
  Var(String),
  Str(String),
  Let {
    var: (Range, String),
    init: Box<(Range, Exp)>,
    body: Box<(Range, Exp)>,
  }
}