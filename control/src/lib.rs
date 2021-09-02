use asm::Label;
use ast::IdxVar;
use std::fmt::{self, Debug, Formatter, Write};

pub struct CProgram<INFO> {
  pub info: INFO,
  pub body: Vec<(Label, CTail)>,
}

#[non_exhaustive]
#[derive(Clone)]
pub enum CTail {
  Seq(CStmt, Box<CTail>),
  Return(CExp),
  Goto(Label),
  If(CExp, Label, Label),
}

#[non_exhaustive]
#[derive(Clone)]
pub enum CStmt {
  Assign { var: IdxVar, exp: CExp },
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
        Self::If(exp, conseq, alt) => {
          return write!(
            f,
            "    if {:?} goto {:?} else goto {:?}",
            exp, conseq, alt
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
    }
  }
}

impl CCmpOp {
  pub fn from_str(str: &str) -> Option<Self> {
    match str {
      "eq?" => Some(Self::Eq),
      ">" => Some(Self::Gt),
      ">=" => Some(Self::Ge),
      "<" => Some(Self::Lt),
      "<=" => Some(Self::Le),
      _ => None,
    }
  }
}
