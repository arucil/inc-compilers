use asm::{CmpResult, Label};
use ast::{IdxVar, Type};
use id_arena::Arena;
use std::fmt::{self, Debug, Formatter, Write};
use std::str::FromStr;

pub struct CProgram<INFO> {
  pub info: INFO,
  pub funs: Vec<CFun<INFO>>,
  pub body: Vec<(Label, CTail)>,
  pub types: Arena<Type>,
}

pub struct CFun<INFO> {
  pub name: String,
  pub params: Vec<(IdxVar, Type)>,
  pub info: INFO,
  pub ty: Type,
  pub blocks: Vec<(Label, CTail)>,
}

#[derive(Clone)]
pub enum CTail {
  Seq(CStmt, Box<CTail>),
  Return(CExp),
  Error(CError),
  Goto(Label),
  If {
    cmp: CCmpOp,
    lhs: CAtom,
    rhs: CAtom,
    conseq: Label,
    alt: Label,
  },
  TailCall(CAtom, Vec<CAtom>),
}

#[derive(Clone)]
pub enum CError {
  Length(CAtom),
  OutOfBounds { index: CAtom, len: CAtom },
  DivByZero,
}

#[derive(Clone)]
pub enum CStmt {
  Assign {
    var: IdxVar,
    exp: CExp,
  },
  PrintBool(CAtom),
  PrintInt(CAtom),
  PrintStr(CAtom),
  NewLine,
  Read,
  ArrSet {
    vec: CAtom,
    index: CAtom,
    val: CAtom,
  },
  TupSet {
    tup: CAtom,
    fields_before: Vec<Type>,
    val: CAtom,
  },
  CopyStr {
    dest: CAtom,
    src: CAtom,
    offset: CAtom,
  },
  Call(CAtom, Vec<CAtom>),
}

#[derive(Clone)]
pub enum CExp {
  Atom(CAtom),
  Prim(CPrim),
  Call(CAtom, Vec<CAtom>),
}

#[derive(Clone)]
pub enum CPrim {
  Read,
  Neg(CAtom),
  Add(CAtom, CAtom),
  Sub(CAtom, CAtom),
  Mul(CAtom, CAtom),
  Div(CAtom, CAtom),
  Rem(CAtom, CAtom),
  Not(CAtom),
  Cmp(CCmpOp, CAtom, CAtom),
  MakeArr {
    len: CAtom,
    val: CAtom,
    ty: Type,
  },
  ArrRef {
    vec: CAtom,
    index: CAtom,
  },
  ArrLen(CAtom),
  Tuple(Vec<(CAtom, Type)>),
  TupRef {
    tup: CAtom,
    fields_before: Vec<Type>,
  },
  TupLen(CAtom),
  AllocStr(CAtom),
  StrLen(CAtom),
}

#[derive(Clone, Copy)]
pub enum CCmpOp {
  Eq,
  Lt,
  Le,
  Gt,
  Ge,
}

#[derive(Clone)]
pub enum CAtom {
  Int(i64),
  Var(IdxVar),
  Bool(bool),
  Void,
  Str(String),
  FunRef(String, usize),
}

impl<INFO: Debug> CProgram<INFO> {
  #[allow(unused)]
  pub fn to_string_pretty(&self) -> String {
    let mut buf = String::new();
    if self.types.len() != 0 {
      write!(&mut buf, "types: ").unwrap();
      let mut comma = false;
      for (id, ty) in &self.types {
        if comma {
          write!(&mut buf, ", ").unwrap();
        }
        comma = true;
        write!(&mut buf, "{} => {:?}", id.index(), ty).unwrap();
      }
      writeln!(&mut buf).unwrap();
    }

    for fun in &self.funs {
      write!(&mut buf, "--------------------------- ").unwrap();
      writeln!(&mut buf, "{} -------------------------", fun.name).unwrap();
      write!(&mut buf, "params: ").unwrap();
      let mut comma = false;
      for (name, ty) in &fun.params {
        if comma {
          write!(&mut buf, ", ").unwrap();
        }
        comma = true;
        write!(&mut buf, "{}: {:?}", name, ty).unwrap();
      }
      writeln!(&mut buf, "\nreturns: {:?}", fun.ty).unwrap();
      writeln!(&mut buf, "{:?}", fun.info).unwrap();
      writeln!(&mut buf).unwrap();
      for (label, block) in &fun.blocks {
        writeln!(&mut buf, "{}:", label.name()).unwrap();
        writeln!(&mut buf, "{:?}", block).unwrap();
      }
    }
    if !self.funs.is_empty() {
      writeln!(&mut buf, "\n----------------------------------------").unwrap();
    }

    writeln!(&mut buf, "{:?}", self.info).unwrap();
    writeln!(&mut buf).unwrap();
    for (label, block) in &self.body {
      writeln!(&mut buf, "{}:", label.name()).unwrap();
      writeln!(&mut buf, "{:?}", block).unwrap();
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
        Self::Error(CError::Length(arg)) => {
          return write!(f, "    length-error {:?}", arg)
        }
        Self::Error(CError::OutOfBounds { index, len }) => {
          return write!(f, "    out-of-bounds-error {:?} {:?}", index, len)
        }
        Self::Error(CError::DivByZero) => {
          return write!(f, "    div-by-zero-error")
        }
        Self::Goto(label) => return write!(f, "    goto {}", label.name()),
        Self::If {
          cmp,
          lhs,
          rhs,
          conseq,
          alt,
        } => {
          return write!(
            f,
            "    if ({:?} {:?} {:?}) goto {} else goto {}",
            cmp,
            lhs,
            rhs,
            conseq.name(),
            alt.name()
          );
        }
        Self::TailCall(fun, args) => {
          write!(f, "    ")?;
          fun.fmt(f)?;
          for arg in args {
            write!(f, " ")?;
            arg.fmt(f)?;
          }
          return Ok(());
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
      Self::PrintBool(val) => {
        write!(f, "print-bool {:?}", val)
      }
      Self::PrintInt(val) => {
        write!(f, "print-int {:?}", val)
      }
      Self::PrintStr(val) => {
        write!(f, "print-str {:?}", val)
      }
      Self::NewLine => {
        write!(f, "newline")
      }
      Self::TupSet {
        tup,
        fields_before,
        val,
      } => {
        write!(f, "vector-set! {:?} {:?} {:?}", tup, fields_before, val)
      }
      Self::ArrSet { vec, index, val } => {
        write!(f, "vector-set! {:?} {:?} {:?}", vec, index, val)
      }
      Self::CopyStr { dest, src, offset } => {
        write!(f, "copy-string! {:?} {:?} {:?}", dest, offset, src)
      }
      Self::Call(fun, args) => {
        fun.fmt(f)?;
        for arg in args {
          write!(f, " ")?;
          arg.fmt(f)?;
        }
        Ok(())
      }
    }
  }
}

impl Debug for CExp {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Atom(atom) => atom.fmt(f),
      Self::Prim(prim) => prim.fmt(f),
      Self::Call(fun, args) => {
        write!(f, "(")?;
        fun.fmt(f)?;
        for arg in args {
          write!(f, " ")?;
          arg.fmt(f)?;
        }
        write!(f, ")")
      }
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
      Self::Mul(arg1, arg2) => {
        write!(f, "(* {:?} {:?})", arg1, arg2)
      }
      Self::Div(arg1, arg2) => {
        write!(f, "(qoutient {:?} {:?})", arg1, arg2)
      }
      Self::Rem(arg1, arg2) => {
        write!(f, "(remainder {:?} {:?})", arg1, arg2)
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
      Self::MakeArr { len, val, ty } => {
        write!(f, "(make-vector {:?} {:?} : {:?})", len, val, ty)
      }
      Self::ArrRef { vec, index } => {
        write!(f, "(vector-ref {:?} {:?})", vec, index)
      }
      Self::ArrLen(arg) => {
        write!(f, "(vector-length {:?})", arg)
      }
      Self::Tuple(fields) => {
        write!(f, "(vector")?;
        for (field, ty) in fields {
          write!(f, " ({:?} : {:?})", field, ty)?;
        }
        write!(f, ")")
      }
      Self::TupRef { tup, fields_before } => {
        write!(f, "(vector-ref {:?} {:?})", tup, fields_before)
      }
      Self::TupLen(arg) => {
        write!(f, "(vector-length {:?})", arg)
      }
      Self::AllocStr(arg) => {
        write!(f, "(alloc-string {:?})", arg)
      }
      Self::StrLen(arg) => {
        write!(f, "(string-length {:?})", arg)
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
      Self::Void => write!(f, "#<void>"),
      Self::Str(s) => write!(f, "{:?}", s),
      Self::FunRef(label, arity) => write!(f, "(fun-ref {} {})", label, arity),
    }
  }
}

#[derive(Debug, Clone)]
pub struct InvalidCmpOp(String);

impl FromStr for CCmpOp {
  type Err = InvalidCmpOp;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "eq?" => Ok(Self::Eq),
      ">" => Ok(Self::Gt),
      ">=" => Ok(Self::Ge),
      "<" => Ok(Self::Lt),
      "<=" => Ok(Self::Le),
      _ => Err(InvalidCmpOp(s.to_owned())),
    }
  }
}

impl From<CCmpOp> for CmpResult {
  fn from(x: CCmpOp) -> Self {
    match x {
      CCmpOp::Eq => Self::Eq,
      CCmpOp::Gt => Self::Gt,
      CCmpOp::Ge => Self::Ge,
      CCmpOp::Lt => Self::Lt,
      CCmpOp::Le => Self::Le,
    }
  }
}
