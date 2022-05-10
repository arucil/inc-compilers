#![feature(never_type, box_syntax)]

use id_arena::{Arena, Id};
use pretty::*;
use std::fmt::{self, Debug, Formatter};
use std::iter;
use support::Range;

pub mod parser;

pub use parser::{parse, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<VAR = String> {
  pub body: Vec<Exp<VAR>>,
  pub types: Arena<CompType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp<VAR = String> {
  Int {
    value: i64,
    range: Range,
  },
  Prim {
    op: (Range, &'static str),
    args: Vec<Exp<VAR>>,
    range: Range,
  },
  Var {
    var: VAR,
    range: Range,
  },
  Str {
    value: String,
    range: Range,
  },
  Let {
    var: (Range, VAR),
    /// is HasType after typecheck
    init: Box<Exp<VAR>>,
    /// is HasType after typecheck
    body: Box<Exp<VAR>>,
    range: Range,
  },
  Bool {
    value: bool,
    range: Range,
  },
  If {
    cond: Box<Exp<VAR>>,
    conseq: Box<Exp<VAR>>,
    alt: Box<Exp<VAR>>,
    range: Range,
  },
  Set {
    var: (Range, VAR),
    exp: Box<Exp<VAR>>,
    range: Range,
  },
  Get {
    var: VAR,
    range: Range,
  },
  Begin {
    seq: Vec<Exp<VAR>>,
    last: Box<Exp<VAR>>,
    range: Range,
  },
  While {
    cond: Box<Exp<VAR>>,
    body: Box<Exp<VAR>>,
    range: Range,
  },
  Void(Range),
  Print {
    /// nonempty. is HasTypes after typecheck
    args: Vec<Exp<VAR>>,
    range: Range,
  },
  NewLine(Range),
  HasType {
    exp: Box<Exp<VAR>>,
    ty: Type,
  },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
  Int,
  Bool,
  Str,
  Comp(TypeId),
  Void,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(Id<CompType>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompType {
  Vector(Vec<Type>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IdxVar {
  pub name: String,
  pub index: usize,
}

impl IdxVar {
  pub fn new<S: ToString>(name: S) -> Self {
    let name = name.to_string();
    if let Some(i) = name.find('.') {
      if let Ok(index) = name[i + 1..].parse::<usize>() {
        return Self {
          name: name[..i].to_owned(),
          index,
        };
      }
    }
    Self { name, index: 0 }
  }
}

impl<VAR> Exp<VAR> {
  pub fn range(&self) -> Range {
    match self {
      Self::Int { range, .. }
      | Self::Prim { range, .. }
      | Self::Var { range, .. }
      | Self::Str { range, .. }
      | Self::Let { range, .. }
      | Self::Bool { range, .. }
      | Self::If { range, .. }
      | Self::Set { range, .. }
      | Self::Get { range, .. }
      | Self::Begin { range, .. }
      | Self::While { range, .. }
      | Self::Void(range)
      | Self::Print { range, .. }
      | Self::NewLine(range) => *range,
      Self::HasType { exp, .. } => exp.range(),
    }
  }
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
    RcDoc::intersperse(self.body.iter().map(|exp| exp.to_doc()), Doc::line())
  }
}

impl<VAR: Debug> Exp<VAR> {
  fn to_doc(&self) -> RcDoc {
    match self {
      Exp::Int { value, .. } => RcDoc::text(format!("{}", value)),
      Exp::Str { value, .. } => RcDoc::text(format!("{:?}", value)),
      Exp::Var { var, .. } | Exp::Get { var, .. } => {
        RcDoc::text(format!("{:?}", var))
      }
      Exp::Prim { op, args, .. } => RcDoc::text("(")
        .append(
          RcDoc::intersperse(
            iter::once(RcDoc::text(op.1))
              .chain(args.iter().map(|arg| arg.to_doc())),
            Doc::line(),
          )
          .nest(1)
          .group(),
        )
        .append(RcDoc::text(")")),
      Exp::Let {
        var, init, body, ..
      } => RcDoc::text("(")
        .append(
          RcDoc::text("let")
            .append(Doc::line())
            .append(
              RcDoc::text("[")
                .append(
                  RcDoc::text(format!("{:?}", var.1))
                    .append(Doc::line())
                    .append(init.to_doc())
                    .nest(1)
                    .group(),
                )
                .append(RcDoc::text("]")),
            )
            .append(Doc::line())
            .append(body.to_doc())
            .nest(1)
            .group(),
        )
        .append(RcDoc::text(")")),
      Exp::Bool { value: true, .. } => RcDoc::text("#t"),
      Exp::Bool { value: false, .. } => RcDoc::text("#t"),
      Exp::If {
        cond, conseq, alt, ..
      } => RcDoc::text("(")
        .append(
          RcDoc::text("if")
            .append(Doc::line())
            .append(cond.to_doc())
            .group()
            .append(Doc::line())
            .append(conseq.to_doc().append(Doc::line()).append(alt.to_doc()))
            .nest(1)
            .group(),
        )
        .append(RcDoc::text(")")),
      Exp::Set { var, exp, .. } => RcDoc::text("(")
        .append(
          RcDoc::text("set!")
            .append(Doc::line())
            .append(RcDoc::text(format!("{:?}", var.1)))
            .append(Doc::line())
            .append(exp.to_doc())
            .nest(1)
            .group(),
        )
        .append(RcDoc::text(")")),
      Exp::Begin { seq, last, .. } => RcDoc::text("(")
        .append(
          RcDoc::text("begin").append(Doc::line()).append(
            RcDoc::intersperse(
              seq
                .iter()
                .chain(std::iter::once(&**last))
                .map(|exp| exp.to_doc()),
              Doc::line(),
            )
            .nest(1)
            .group(),
          ),
        )
        .append(RcDoc::text(")")),
      Exp::While { cond, body, .. } => RcDoc::text("(")
        .append(
          RcDoc::text("while")
            .append(Doc::line())
            .append(cond.to_doc())
            .group()
            .append(Doc::line())
            .append(body.to_doc())
            .nest(1)
            .group(),
        )
        .append(RcDoc::text(")")),
      Exp::Void { .. } => RcDoc::text("(void)"),
      Exp::Print { args, .. } => RcDoc::text("(")
        .append(
          RcDoc::intersperse(
            iter::once(RcDoc::text("print"))
              .chain(args.iter().map(|arg| arg.to_doc())),
            Doc::line(),
          )
          .nest(1)
          .group(),
        )
        .append(RcDoc::text(")")),
      Exp::NewLine { .. } => RcDoc::text("(newline)"),
      Exp::HasType { exp, .. } => exp.to_doc(),
    }
  }
}
