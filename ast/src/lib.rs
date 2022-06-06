#![feature(never_type, box_syntax)]

use id_arena::{Arena, Id};
use indexmap::IndexMap;
use pretty::*;
use std::fmt::{self, Debug, Display, Formatter};
use std::iter;
use support::Range;

pub mod parser;

pub use parser::{parse, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<VAR = String, TYPE = ()> {
  pub type_defs: IndexMap<String, Type>,
  pub fun_defs: IndexMap<String, FunDef<VAR, TYPE>>,
  pub body: Exp<VAR, TYPE>,
  pub types: Arena<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunDef<VAR = String, TYPE = ()> {
  pub params: Vec<(VAR, Type)>,
  pub ret: Type,
  pub body: Exp<VAR, TYPE>,
  pub range: Range,
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
  Apply {
    fun: Box<Exp<VAR, TYPE>>,
    args: Vec<Exp<VAR, TYPE>>,
    r#struct: Option<StructApp>,
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
  FunRef {
    name: String,
    arity: usize,
  },
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
  Error(Error<VAR, TYPE>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructApp {
  Ctor,
  Getter(u32),
  Setter(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error<VAR, TYPE> {
  Length(Box<Exp<VAR, TYPE>>),
  OutOfBounds {
    index: Box<Exp<VAR, TYPE>>,
    len: Box<Exp<VAR, TYPE>>,
  },
  DivByZero,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  Void,
  Int,
  Bool,
  Str,
  Tuple(Vec<Type>),
  Array(Box<Type>),
  Alias(TypeAlias),
  Struct(IndexMap<String, Type>),
  Fun { params: Vec<Type>, ret: Box<Type> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAlias {
  Pending(Range, String),
  Resolved(TypeId),
}

pub type TypeId = Id<Type>;

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

impl Display for IdxVar {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}.{}", self.name, self.index)
  }
}

impl Debug for IdxVar {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    Display::fmt(self, f)
  }
}

impl<VAR: Display, TYPE: Debug> Program<VAR, TYPE> {
  pub fn to_string_pretty(&self) -> String {
    let doc = self.to_doc();
    let mut buf = String::new();
    doc.render_fmt(50, &mut buf).unwrap();
    buf
  }

  fn to_doc(&self) -> RcDoc {
    RcDoc::intersperse(
      self
        .fun_defs
        .iter()
        .map(|(name, def)| def.to_doc(name))
        .chain(std::iter::once(self.body.to_doc())),
      Doc::line(),
    )
  }
}

impl<VAR: Display, TYPE: Debug> FunDef<VAR, TYPE> {
  fn to_doc<'a>(&'a self, name: &str) -> RcDoc<'a> {
    RcDoc::text("(")
      .append(
        RcDoc::text("define")
          .append(Doc::line())
          .append(
            RcDoc::text("(")
              .append(
                RcDoc::text(name.to_owned())
                  .append(Doc::line())
                  .append(RcDoc::intersperse(
                    self.params.iter().map(|(name, ty)| {
                      RcDoc::text("[")
                        .append(name.to_string())
                        .append(Doc::line())
                        .append(ty.to_doc())
                        .append("]")
                        .group()
                    }),
                    Doc::line(),
                  ))
                  .nest(1),
              )
              .append(")")
              .group(),
          )
          .group()
          .append(Doc::space())
          .append(":")
          .append(Doc::space())
          .append(self.ret.to_doc())
          .group()
          .append(Doc::line())
          .append(self.body.to_doc())
          .nest(1),
      )
      .append(")")
  }
}

impl<VAR: Display, TYPE: Debug> Exp<VAR, TYPE> {
  fn to_doc(&self) -> RcDoc {
    match &self.kind {
      ExpKind::Int(n) => RcDoc::text(format!("{}", n)),
      ExpKind::Str(s) => RcDoc::text(format!("{:?}", s)),
      ExpKind::Var(var) => RcDoc::text(format!("{}", var)),
      ExpKind::Get(var) => {
        RcDoc::text("{").append(format!("{}", var)).append("}")
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
        .append(")"),
      ExpKind::Apply {
        fun,
        args,
        r#struct: _,
      } => RcDoc::text("(")
        .append(
          RcDoc::intersperse(
            iter::once(fun.to_doc()).chain(args.iter().map(|arg| arg.to_doc())),
            Doc::line(),
          )
          .nest(1)
          .group(),
        )
        .append(")"),
      ExpKind::Let { var, init, body } => RcDoc::text("(")
        .append(
          RcDoc::text("let")
            .append(Doc::line())
            .append(
              RcDoc::text("[")
                .append(
                  RcDoc::text(format!("{}", var.1))
                    .append(Doc::line())
                    .append(init.to_doc())
                    .nest(1)
                    .group(),
                )
                .append("]"),
            )
            .append(Doc::line())
            .append(body.to_doc())
            .nest(1),
        )
        .append(")"),
      ExpKind::Bool(true) => RcDoc::text("#t"),
      ExpKind::Bool(false) => RcDoc::text("#f"),
      ExpKind::If { cond, conseq, alt } => RcDoc::text("(")
        .append(
          RcDoc::text("if")
            .append(Doc::line())
            .append(cond.to_doc())
            .group()
            .append(Doc::line())
            .append(conseq.to_doc())
            .append(Doc::line())
            .append(alt.to_doc())
            .nest(1),
        )
        .append(")"),
      ExpKind::Set { var, exp } => RcDoc::text("(")
        .append(
          RcDoc::text("set!")
            .append(Doc::line())
            .append(format!("{}", var.1))
            .append(Doc::line())
            .append(exp.to_doc())
            .nest(1)
            .group(),
        )
        .append(")"),
      ExpKind::Begin { seq, last } => RcDoc::text("(")
        .append(
          RcDoc::text("begin")
            .append(Doc::line())
            .append(RcDoc::intersperse(
              seq
                .iter()
                .chain(std::iter::once(&**last))
                .map(|exp| exp.to_doc()),
              Doc::line(),
            ))
            .nest(1),
        )
        .append(")"),
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
        .append(")"),
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
        .append(")"),
      ExpKind::NewLine => RcDoc::text("(newline)"),
      ExpKind::Error(Error::Length(arg)) => RcDoc::text("(")
        .append(
          RcDoc::text("length-error")
            .append(Doc::line())
            .append(arg.to_doc())
            .nest(1)
            .group(),
        )
        .append(")"),
      ExpKind::Error(Error::OutOfBounds { index, len }) => RcDoc::text("(")
        .append(
          RcDoc::text("out-of-bounds-error")
            .append(Doc::line())
            .append(index.to_doc())
            .append(Doc::line())
            .append(len.to_doc())
            .nest(1)
            .group(),
        )
        .append(")"),
      ExpKind::Error(Error::DivByZero) => RcDoc::text("(div-by-zero)"),
      ExpKind::FunRef { name, arity } => RcDoc::text("{")
        .append(
          RcDoc::text("fun-ref")
            .append(Doc::line())
            .append(name)
            .append(Doc::line())
            .append(format!("{}", arity))
            .nest(1)
            .group(),
        )
        .append("}"),
    }
  }
}

impl<VAR, TYPE> ExpKind<VAR, TYPE> {
  pub fn is_atomic(&self) -> bool {
    match self {
      Self::Int(_) => true,
      Self::Prim { .. } => false,
      Self::Apply { .. } => false,
      Self::Var(_) => true,
      Self::Str(_) => true,
      Self::Let { .. } => false,
      Self::Bool(_) => true,
      Self::If { .. } => false,
      Self::Set { .. } => false,
      Self::Get(_) => false,
      Self::FunRef { .. } => true,
      Self::Begin { .. } => false,
      Self::While { .. } => false,
      Self::Void => true,
      Self::Print(..) => false,
      Self::NewLine => false,
      Self::Error(_) => false,
    }
  }
}

impl Type {
  pub fn is_ref(&self, types: &Arena<Type>) -> bool {
    match self {
      Self::Tuple(_) => true,
      Self::Array(_) => true,
      Self::Str => true,
      Self::Alias(TypeAlias::Resolved(id)) => types[*id].is_ref(types),
      Self::Alias(TypeAlias::Pending(..)) => unreachable!(),
      _ => false,
    }
  }

  pub fn resolved(&self, types: &Arena<Type>) -> Self {
    match self {
      Self::Alias(TypeAlias::Resolved(id)) => types[*id].resolved(types),
      Self::Alias(TypeAlias::Pending(..)) => unreachable!(),
      _ => self.clone(),
    }
  }

  fn to_doc(&self) -> RcDoc {
    match self {
      Self::Void => RcDoc::text("Void"),
      Self::Int => RcDoc::text("Int"),
      Self::Bool => RcDoc::text("Bool"),
      Self::Str => RcDoc::text("Str"),
      Self::Tuple(tys) => RcDoc::text("(")
        .append(RcDoc::intersperse(
          tys.iter().map(|ty| ty.to_doc()),
          Doc::line(),
        ))
        .nest(1)
        .append(")"),
      Self::Array(ty) => RcDoc::text("(")
        .append(
          RcDoc::text("array-of")
            .append(Doc::line())
            .append(ty.to_doc())
            .nest(1),
        )
        .append(")"),
      Self::Alias(TypeAlias::Pending(_, alias)) => RcDoc::text(alias),
      Self::Alias(TypeAlias::Resolved(id)) => {
        RcDoc::text(format!("(alias {})", id.index()))
      }
      Self::Struct(fields) => RcDoc::text("(")
        .append(
          RcDoc::text("struct")
            .append(Doc::line())
            .append(RcDoc::intersperse(
              fields.iter().map(|(name, ty)| {
                RcDoc::text("[")
                  .append(name)
                  .append(Doc::line())
                  .append(ty.to_doc())
                  .append("]")
              }),
              Doc::line(),
            ))
            .nest(1),
        )
        .append(")"),
      Self::Fun { params, ret } => RcDoc::text("(")
        .append(RcDoc::intersperse(
          params.iter().map(|ty| ty.to_doc()),
          Doc::line(),
        ))
        .append(Doc::line())
        .group()
        .append("->")
        .append(Doc::line())
        .append(ret.to_doc())
        .nest(1)
        .append(")"),
    }
  }
}
