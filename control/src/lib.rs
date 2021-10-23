use asm::{CmpResult, Label};
use ast::IdxVar;
use std::fmt::{self, Debug, Formatter, Write};
use std::str::FromStr;

pub struct CProgram<INFO> {
  pub info: INFO,
  pub body: Vec<(Label, CTail)>,
}

pub struct CDef {
  pub label: String,
  pub params: Vec<CParam>,
  pub ty: CType,
  pub body: Vec<(Label, CTail)>,
}

pub struct CParam {
  pub name: IdxVar,
  pub ty: CType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CType {
  Int,
  Bool,
  Str,
  Void,
}

#[non_exhaustive]
#[derive(Clone)]
pub enum CTail {
  Seq(CStmt, Box<CTail>),
  Return(CExp),
  Goto(Label),
  If(CCmpOp, CAtom, CAtom, Label, Label),
}

#[non_exhaustive]
#[derive(Clone)]
pub enum CStmt {
  Assign { var: IdxVar, exp: CExp },
  Print { val: CExp, ty: CType },
  NewLine,
  Read,
}

#[derive(Clone)]
pub enum CExp {
  Atom(CAtom),
  Prim(CPrim),
}

#[non_exhaustive]
#[derive(Clone)]
pub enum CPrim {
  Read,
  Neg(CAtom),
  Add(CAtom, CAtom),
  Sub(CAtom, CAtom),
  Not(CAtom),
  Cmp(CCmpOp, CAtom, CAtom),
}

#[derive(Clone, Copy)]
pub enum CCmpOp {
  Eq,
  Lt,
  Le,
  Gt,
  Ge,
}

#[non_exhaustive]
#[derive(Clone)]
pub enum CAtom {
  Int(i64),
  Var(IdxVar),
  Bool(bool),
  Str(String),
}

impl<INFO: Debug> CProgram<INFO> {
  #[allow(unused)]
  pub fn to_string_pretty(&self) -> String {
    let mut buf = format!("{:?}", self.info);
    for (label, block) in &self.body {
      writeln!(&mut buf, "{:?}:", label).unwrap();
      buf += &format!("{:?}\n", block);
    }
    buf
  }
}

impl Debug for CTail {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let mut tail = self;
    loop {
      match tail {
        Self::Seq(stmt, tail1) => {
          writeln!(f, "    {:?}", stmt)?;
          tail = tail1;
        }
        Self::Return(exp) => return write!(f, "    return {:?}", exp),
        Self::Goto(label) => return write!(f, "    goto {:?}", label),
        Self::If(cmp, arg1, arg2, conseq, alt) => {
          return write!(
            f,
            "    if ({:?} {:?} {:?}) goto {:?} else goto {:?}",
            cmp, arg1, arg2, conseq, alt
          )
        }
      }
    }
  }
}

impl Debug for CStmt {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Assign { var, exp } => {
        write!(f, "{:?} = {:?}", var, exp)
      }
      Self::Read => {
        write!(f, "read")
      }
      Self::Print { val, ty } => {
        write!(f, "print {:?} {:?}", ty, val)
      }
      Self::NewLine => {
        write!(f, "newline")
      }
    }
  }
}

impl Debug for CExp {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Atom(atom) => atom.fmt(f),
      Self::Prim(prim) => prim.fmt(f),
    }
  }
}

impl Debug for CPrim {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Add(arg1, arg2) => {
        write!(f, "(+ {:?} {:?})", arg1, arg2)
      }
      Self::Sub(arg1, arg2) => {
        write!(f, "(- {:?} {:?})", arg1, arg2)
      }
      Self::Neg(arg) => {
        write!(f, "(- {:?})", arg)
      }
      Self::Read => {
        write!(f, "(read)")
      }
      Self::Not(arg) => {
        write!(f, "(not {:?})", arg)
      }
      Self::Cmp(op, arg1, arg2) => {
        write!(f, "({:?} {:?} {:?})", op, arg1, arg2)
      }
    }
  }
}

impl Debug for CCmpOp {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Eq => write!(f, "eq?"),
      Self::Lt => write!(f, "<"),
      Self::Le => write!(f, "<="),
      Self::Gt => write!(f, ">"),
      Self::Ge => write!(f, ">="),
    }
  }
}

impl Debug for CAtom {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Int(n) => write!(f, "{}", n),
      Self::Var(n) => write!(f, "{:?}", n),
      Self::Bool(true) => write!(f, "#t"),
      Self::Bool(false) => write!(f, "#f"),
      Self::Str(s) => write!(f, "{:?}", s),
    }
  }
}

#[derive(Debug, Clone)]
pub struct InvalidCmpOp;

impl FromStr for CCmpOp {
  type Err = InvalidCmpOp;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "eq?" => Ok(Self::Eq),
      ">" => Ok(Self::Gt),
      ">=" => Ok(Self::Ge),
      "<" => Ok(Self::Lt),
      "<=" => Ok(Self::Le),
      _ => Err(InvalidCmpOp),
    }
  }
}

impl Into<CmpResult> for CCmpOp {
  fn into(self) -> CmpResult {
    match self {
      CCmpOp::Eq => CmpResult::Eq,
      CCmpOp::Gt => CmpResult::Gt,
      CCmpOp::Ge => CmpResult::Ge,
      CCmpOp::Lt => CmpResult::Lt,
      CCmpOp::Le => CmpResult::Le,
    }
  }
}
