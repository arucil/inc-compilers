#![feature(never_type)]

use num_derive::FromPrimitive;
use std::fmt::{self, Debug, Write};
use support::WritePretty;

#[derive(Debug, Clone)]
pub struct Program<INFO=(),VAR=!> {
  pub info: INFO,
  pub blocks: Vec<(String, Block<VAR>)>,
}

#[derive(Debug, Clone)]
pub struct Block<VAR=!> {
  pub code: Vec<Instr<VAR>>,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Instr<VAR=!> {
  Addq(Arg<VAR>, Arg<VAR>),
  Subq(Arg<VAR>, Arg<VAR>),
  Movq(Arg<VAR>, Arg<VAR>),
  Negq(Arg<VAR>),
  Callq(String, usize),
  Retq,
  Pushq(Arg<VAR>),
  Popq(Arg<VAR>),
  Jmp(String),
  Syscall,
}

#[derive(Debug, Clone)]
pub enum Arg<VAR=!> {
  Imm(i64),
  Reg(Reg),
  Deref(Reg, i32),
  Var(VAR),
}

#[derive(Clone, FromPrimitive)]
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

impl<INFO: WritePretty, VAR: Debug> Program<INFO, VAR> {
  pub fn to_string_pretty(&self) -> String {
    let mut buf = String::new();
    self.info.write(&mut buf).unwrap();
    for (label, block) in &self.blocks {
      writeln!(&mut buf, "{}:", label).unwrap();
      block.write(&mut buf).unwrap();
    }
    buf
  }

  pub fn to_nasm(&self) -> String {
    let mut buf = format!("section .text\n");
    for (label, block) in &self.blocks {
      writeln!(&mut buf, "{}:", label).unwrap();
      block.write(&mut buf).unwrap();
    }
    buf
  }
}

impl<VAR: Debug> WritePretty for Block<VAR> {
  fn write(&self, f: &mut impl Write) -> fmt::Result {
    for instr in &self.code {
      write!(f, "    ")?;
      instr.write(f)?;
      writeln!(f)?;
    }
    Ok(())
  }
}

impl<VAR: Debug> WritePretty for Instr<VAR> {
  fn write(&self, f: &mut impl Write) -> fmt::Result {
    match self {
      Self::Addq(src, dest) => self.write_binary(f, "addq", src, dest),
      Self::Movq(src, dest) => self.write_binary(f, "mov", src, dest),
      Self::Callq(label, _) => write!(f, "callq {}", label),
      Self::Jmp(label) => write!(f, "jmp {}", label),
      Self::Negq(dest) => self.write_unary(f, "negq", dest),
      Self::Popq(dest) => self.write_unary(f, "popq", dest),
      Self::Pushq(src) => self.write_unary(f, "pushq", src),
      Self::Retq => write!(f, "retq"),
      Self::Subq(src, dest) => self.write_binary(f, "subq", src, dest),
      Self::Syscall => write!(f, "syscall"),
    }
  }
}

impl<VAR: Debug> Instr<VAR> {
  fn write_unary(
    &self,
    f: &mut impl Write,
    op: &str,
    arg: &Arg<VAR>,
  ) -> fmt::Result {
    write!(f, "{} ", op)?;
    arg.write(f)?;
    write!(f, ", ")
  }

  fn write_binary(
    &self,
    f: &mut impl Write,
    op: &str,
    arg1: &Arg<VAR>,
    arg2: &Arg<VAR>,
  ) -> fmt::Result {
    write!(f, "{} ", op)?;
    arg1.write(f)?;
    write!(f, ", ")?;
    arg2.write(f)
  }
}

impl<VAR: Debug> WritePretty for Arg<VAR> {
  fn write(&self, f: &mut impl Write) -> fmt::Result {
    match self {
      Self::Imm(n) => write!(f, "{}", n),
      Self::Var(var) => write!(f, "{:?}", var),
      Self::Deref(r, i) => write!(f, "{}({:?})", i, r),
      Self::Reg(r) => write!(f, "{:?}", r),
    }
  }
}

impl Debug for Reg {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    write!(f, "{}", op)
  }
}