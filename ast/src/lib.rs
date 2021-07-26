#![feature(never_type, box_syntax)]

use pretty::*;
use std::fmt::{self, Debug, Formatter};
use std::iter;
use support::Range;

pub mod parser;

pub use parser::{parse, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<VAR = String> {
  pub body: Vec<(Range, Exp<VAR>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Exp<VAR = String> {
  Int(i64),
  Prim {
    op: (Range, &'static str),
    args: Vec<(Range, Exp<VAR>)>,
  },
  Var(VAR),
  Str(String),
  Let {
    var: (Range, VAR),
    init: Box<(Range, Exp<VAR>)>,
    body: Box<(Range, Exp<VAR>)>,
  },
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IdxVar {
  pub name: String,
  pub index: usize,
}

impl Debug for IdxVar {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}.{}", self.name, self.index)
  }
}

impl<VAR: Debug> Program<VAR> {
  pub fn to_string_pretty(&self) -> String {
    let doc = self.to_doc();
    let mut buf = String::new();
    doc.render_fmt(30, &mut buf).unwrap();
    buf
  }

  fn to_doc(&self) -> RcDoc {
    RcDoc::intersperse(self.body.iter().map(|(_, exp)| exp.to_doc()), Doc::line())
  }
}

impl<VAR: Debug> Exp<VAR> {
  fn to_doc(&self) -> RcDoc {
    match self {
      Exp::Int(n) => RcDoc::text(format!("{}", n)),
      Exp::Str(s) => RcDoc::text(format!("{:?}", s)),
      Exp::Var(var) => RcDoc::text(format!("{:?}", var)),
      Exp::Prim { op, args } => RcDoc::text("(")
        .append(
          RcDoc::intersperse(
            iter::once(RcDoc::text(op.1)).chain(args.iter().map(|(_, arg)| arg.to_doc())),
            Doc::line(),
          )
          .nest(1)
          .group(),
        )
        .append(RcDoc::text(")")),
      Exp::Let { var, init, body } => RcDoc::text("(")
        .append(
          RcDoc::text("let")
            .append(Doc::line())
            .append(
              RcDoc::text("[")
                .append(
                  RcDoc::text(format!("{:?}", var.1))
                    .append(Doc::line())
                    .append(init.1.to_doc())
                    .nest(1)
                    .group(),
                )
                .append(RcDoc::text("]")),
            )
            .append(Doc::line())
            .append(body.1.to_doc())
            .nest(1)
            .group(),
        )
        .append(RcDoc::text(")")),
    }
  }
}
