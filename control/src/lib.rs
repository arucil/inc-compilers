use ast::IdxVar;
use std::fmt::{self, Debug, Formatter, Write};

pub struct CProgram<INFO> {
  pub info: INFO,
  pub body: Vec<(String, CBlock)>,
}

pub struct CBlock {
  pub stmts: Vec<CStmt>,
  pub tail: CTail,
}

#[non_exhaustive]
pub enum CTail {
  Return(CExp),
}

#[non_exhaustive]
pub enum CStmt {
  Assign { var: IdxVar, exp: CExp },
}

pub enum CExp {
  Atom(CAtom),
  Prim(CPrim),
}

#[non_exhaustive]
pub enum CPrim {
  Read,
  Neg(CAtom),
  Add(CAtom, CAtom),
  Sub(CAtom, CAtom),
  Not(CAtom),
  Cmp(CCmpOp, CAtom, CAtom),
}

pub enum CCmpOp {
  Eq,
  Lt,
  Le,
  Gt,
  Ge,
}

#[non_exhaustive]
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
      writeln!(&mut buf, "{}:", label).unwrap();
      buf += &format!("{:?}", block);
    }
    buf
  }
}

impl Debug for CBlock {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    for stmt in &self.stmts {
      writeln!(f, "    {:?}", stmt)?;
    }
    self.tail.fmt(f)
  }
}

impl Debug for CTail {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Return(exp) => {
        write!(f, "    return {:?}", exp)
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
