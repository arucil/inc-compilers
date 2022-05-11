#![feature(never_type, box_syntax)]

use id_arena::{Arena, Id};
use pretty::*;
use std::fmt::{self, Debug, Formatter};
use std::iter;
use support::Range;

pub mod parser;

pub use parser::{parse, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<VAR = String, TYPE = ()> {
  pub body: Vec<Exp<VAR, TYPE>>,
  pub types: Arena<CompType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exp<VAR = String, TYPE = ()> {
  pub kind: ExpKind<VAR, TYPE>,
  pub range: Range,
  pub ty: TYPE,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpKind<VAR, TYPE> {
  Int(i64),
  Prim {
    op: (Range, &'static str),
    args: Vec<Exp<VAR, TYPE>>,
  },
  Var(VAR),
  Str(String),
  Let {
    var: (Range, VAR),
    init: Box<Exp<VAR, TYPE>>,
    body: Box<Exp<VAR, TYPE>>,
  },
  Bool(bool),
  If {
    cond: Box<Exp<VAR, TYPE>>,
    conseq: Box<Exp<VAR, TYPE>>,
    alt: Box<Exp<VAR, TYPE>>,
  },
  Set {
    var: (Range, VAR),
    exp: Box<Exp<VAR, TYPE>>,
  },
  Get(VAR),
  Begin {
    seq: Vec<Exp<VAR, TYPE>>,
    last: Box<Exp<VAR, TYPE>>,
  },
  While {
    cond: Box<Exp<VAR, TYPE>>,
    body: Box<Exp<VAR, TYPE>>,
  },
  Void,
  Print(
    /// nonempty
    Vec<Exp<VAR, TYPE>>,
  ),
  NewLine,
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

impl Debug for IdxVar {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}.{}", self.name, self.index)
  }
}

impl<VAR: Debug, TYPE: Debug> Program<VAR, TYPE> {
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

impl<VAR: Debug, TYPE: Debug> Exp<VAR, TYPE> {
  fn to_doc(&self) -> RcDoc {
    match &self.kind {
      ExpKind::Int(n) => RcDoc::text(format!("{}", n)),
      ExpKind::Str(s) => RcDoc::text(format!("{:?}", s)),
      ExpKind::Var(var) | ExpKind::Get(var) => {
        RcDoc::text(format!("{:?}", var))
      }
      ExpKind::Prim { op, args } => RcDoc::text("(")
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
      ExpKind::Let { var, init, body } => RcDoc::text("(")
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
      ExpKind::Bool(true) => RcDoc::text("#t"),
      ExpKind::Bool(false) => RcDoc::text("#f"),
      ExpKind::If { cond, conseq, alt } => RcDoc::text("(")
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
      ExpKind::Set { var, exp } => RcDoc::text("(")
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
      ExpKind::Begin { seq, last } => RcDoc::text("(")
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
      ExpKind::While { cond, body } => RcDoc::text("(")
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
      ExpKind::Void => RcDoc::text("(void)"),
      ExpKind::Print(args) => RcDoc::text("(")
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
      ExpKind::NewLine => RcDoc::text("(newline)"),
    }
  }
}
