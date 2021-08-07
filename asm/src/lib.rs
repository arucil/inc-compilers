#![feature(never_type)]

use num_derive::{FromPrimitive, ToPrimitive};
use std::fmt::{self, Debug, Formatter, Write};

#[derive(Debug, Clone)]
pub struct Program<INFO = (), VAR = !> {
  pub info: INFO,
  pub blocks: Vec<(String, Block<VAR>)>,
}

#[derive(Clone)]
pub struct Block<VAR = !> {
  pub code: Vec<Instr<VAR>>,
}

#[derive(Clone)]
#[non_exhaustive]
pub enum Instr<VAR = !> {
  Add(Arg<VAR>, Arg<VAR>),
  Sub(Arg<VAR>, Arg<VAR>),
  Mov(Arg<VAR>, Arg<VAR>),
  Neg(Arg<VAR>),
  Call(String, usize),
  Ret,
  Push(Arg<VAR>),
  Pop(Arg<VAR>),
  Jmp(String),
  Syscall,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Arg<VAR = !> {
  Imm(i64),
  Reg(Reg),
  Deref(Reg, i32),
  Var(VAR),
}

#[derive(
  Clone, Copy, FromPrimitive, ToPrimitive, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub enum Reg {
  Rsp,
  Rbp,
  Rax,
  Rbx,
  Rcx,
  Rdx,
  Rsi,
  Rdi,
  R8,
  R9,
  R10,
  R11,
  R12,
  R13,
  R14,
  R15,
}

impl<INFO: Debug, VAR: Debug> Program<INFO, VAR> {
  pub fn to_string_pretty(&self) -> String {
    let mut buf = format!("{:?}", self.info);
    for (label, block) in &self.blocks {
      writeln!(&mut buf, "{}:", label).unwrap();
      buf += &format!("{:?}", block);
    }
    buf
  }

  pub fn to_nasm(&self) -> String {
    let mut buf = format!(
      "extern read_int, print_int, print_newline\n\
      section .text\n"
    );
    for (label, block) in &self.blocks {
      buf += "\n";
      if label == "_start" {
        buf += "    global _start\n";
      }
      writeln!(&mut buf, "{}:", label).unwrap();
      buf += &format!("{:?}", block);
    }
    buf
  }
}

impl<VAR: Debug> Debug for Block<VAR> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    for instr in &self.code {
      write!(f, "    ")?;
      instr.fmt(f)?;
      writeln!(f)?;
    }
    Ok(())
  }
}

impl<VAR: Debug> Debug for Instr<VAR> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Add(src, dest) => write!(f, "add {:?}, {:?}", dest, src),
      Self::Mov(src, dest) => write!(f, "mov {:?}, {:?}", dest, src),
      Self::Call(label, _) => write!(f, "call {}", label),
      Self::Jmp(label) => write!(f, "jmp {}", label),
      Self::Neg(dest) => write!(f, "neg {:?}", dest),
      Self::Pop(dest) => write!(f, "pop {:?}", dest),
      Self::Push(src) => write!(f, "push {:?}", src),
      Self::Ret => write!(f, "ret"),
      Self::Sub(src, dest) => write!(f, "sub {:?}, {:?}", dest, src),
      Self::Syscall => write!(f, "syscall"),
    }
  }
}

impl<VAR: Debug> Debug for Arg<VAR> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Imm(n) => write!(f, "{}", n),
      Self::Var(var) => var.fmt(f),
      Self::Deref(r, i) => {
        if *i > 0 {
          write!(f, "qword [{:?} + {}]", r, i)
        } else if *i == 0 {
          write!(f, "qword [{:?}]", r)
        } else {
          write!(f, "qword [{:?} - {}]", r, -i)
        }
      }
      Self::Reg(r) => r.fmt(f),
    }
  }
}

impl Reg {
  pub fn is_callee_saved(&self) -> bool {
    match self {
      Reg::Rsp
      | Reg::Rbp
      | Reg::Rbx
      | Reg::R12
      | Reg::R13
      | Reg::R14
      | Reg::R15 => true,
      _ => false,
    }
  }
}

impl Debug for Reg {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    use Reg::*;
    let op = match self {
      Rsp => "rsp",
      Rbp => "rbp",
      Rax => "rax",
      Rbx => "rbx",
      Rcx => "rcx",
      Rdx => "rdx",
      Rsi => "rsi",
      Rdi => "rdi",
      R8 => "r8",
      R9 => "r9",
      R10 => "r10",
      R11 => "r11",
      R12 => "r12",
      R13 => "r13",
      R14 => "r14",
      R15 => "r15",
    };
    f.write_str(op)
  }
}
