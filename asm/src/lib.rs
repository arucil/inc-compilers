#![feature(never_type)]

use ast::Type;
use id_arena::Arena;
use indexmap::IndexMap;
use num_derive::{FromPrimitive, ToPrimitive};
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt::{self, Display, Formatter, Write};
use std::iter::IntoIterator;

#[derive(Clone)]
pub struct Program<INFO = (), VAR = !> {
  pub info: INFO,
  pub constants: IndexMap<String, String>,
  pub externs: BTreeSet<String>,
  pub funs: Vec<Fun<INFO, VAR>>,
  /// The order matters.
  pub blocks: Vec<Block<VAR>>,
  pub types: Arena<Type>,
}

#[derive(Clone)]
pub struct Fun<INFO = (), VAR = !> {
  pub name: String,
  pub info: INFO,
  /// The order matters.
  ///
  /// Prologue block must comes first.
  pub blocks: Vec<Block<VAR>>,
}

#[derive(Clone)]
pub struct Block<VAR = !> {
  pub label: Label,
  pub code: Vec<Instr<VAR>>,
}

#[derive(Clone)]
pub enum Instr<VAR = !> {
  Add {
    src: Arg<VAR>,
    dest: Arg<VAR>,
  },
  Sub {
    src: Arg<VAR>,
    dest: Arg<VAR>,
  },
  IMul {
    /// Must not be Imm.
    src: Arg<VAR>,
    /// Must be reg.
    dest: Arg<VAR>,
  },
  IMul3 {
    /// Must not be Imm.
    src: Arg<VAR>,
    num: i32,
    /// Must be reg.
    dest: Arg<VAR>,
  },
  /// Arg must not be Imm.
  IDiv(Arg<VAR>),
  Mov {
    src: Arg<VAR>,
    dest: Arg<VAR>,
  },
  Neg(Arg<VAR>),
  Call {
    label: LabelOrArg<VAR>,
    arity: usize,
    /// If the function call may trigger GC.
    gc: bool,
  },
  /// Pop the frame and do JMP. Used for tailcall.
  ///
  /// When performing tailcall, the current frame must be popped, but we don't
  /// know the frame size yet when selecting instructions, so we add a new
  /// instruction that will be patched after register allocation.
  TailJmp {
    label: LabelOrArg<VAR>,
    arity: usize,
  },
  Ret,
  Push(Arg<VAR>),
  Pop(Arg<VAR>),
  LocalJmp(Label),
  /// Used for tailcall.
  Jmp(LabelOrArg<VAR>),
  Syscall,
  Xor {
    src: Arg<VAR>,
    dest: Arg<VAR>,
  },
  Cmp {
    src: Arg<VAR>,
    dest: Arg<VAR>,
  },
  SetIf {
    cmp: CmpResult,
    dest: Arg<VAR>,
  },
  Movzx {
    src: Arg<VAR>,
    dest: Arg<VAR>,
  },
  JumpIf {
    cmp: CmpResult,
    label: Label,
  },
  /// `count` must be byte integer or CL.
  Shr {
    dest: Arg<VAR>,
    count: Arg<VAR>,
  },
  /// `count` must be byte integer or CL.
  Shl {
    dest: Arg<VAR>,
    count: Arg<VAR>,
  },
  And {
    src: Arg<VAR>,
    dest: Arg<VAR>,
  },
  Or {
    src: Arg<VAR>,
    dest: Arg<VAR>,
  },
  Lea {
    label: String,
    /// Must be reg.
    dest: Arg<VAR>,
  },
}

#[derive(Clone)]
pub enum LabelOrArg<VAR> {
  Arg(Arg<VAR>),
  Label(String),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Label {
  Tmp(u32),
  /// entry point of main or function
  Start,
  /// before Start
  Prologue,
  Epilogue,
  /// entry point of entire program
  EntryPoint,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CmpResult {
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Arg<VAR = !> {
  Imm(i64),
  Reg(Reg),
  ByteReg(ByteReg),
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ByteReg {
  Al,
  Ah,
  Bl,
  Bh,
  Cl,
  Ch,
  Dl,
  Dh,
}

impl<VAR: Clone> Arg<VAR> {
  pub fn bytereg_to_reg(&self) -> Self {
    match self {
      Self::ByteReg(reg) => Self::Reg((*reg).into()),
      _ => (*self).clone(),
    }
  }
}

impl<INFO: Display, VAR: Display> Program<INFO, VAR> {
  pub fn to_string_pretty(&self) -> String {
    let mut buf = format!("{}constants: {:?}\n\n", self.info, self.constants);
    for block in &self.blocks {
      write!(&mut buf, "{}", block).unwrap();
    }
    buf
  }

  pub fn to_nasm(&self, str_len: bool) -> String {
    let mut buf = String::new();

    if !self.externs.is_empty() {
      buf += "extern ";
      let mut comma = false;
      for sym in &self.externs {
        if comma {
          buf += ", ";
        }
        comma = true;
        buf += sym;
      }
      buf += "\n";
    }
    if !self.constants.is_empty() {
      buf += "section .rodata\n";
      for (name, str) in &self.constants {
        buf += "    ";
        buf += name;
        if str_len {
          buf += " dq ";
          writeln!(&mut buf, "{}", str.len()).unwrap();
          buf += "    ";
          for _ in 0..name.len() {
            buf += " ";
          }
        }
        buf += " db ";
        buf.push('`');
        let str = format!("{:?}", str);
        buf += &str[1..str.len() - 1];
        buf += "`\n";
      }
    }
    buf += "section .text\n";
    for block in &self.blocks {
      buf += "\n";
      if let Label::EntryPoint = block.label {
        buf += "    global ";
        writeln!(&mut buf, "{}", block.label).unwrap();
        buf += "    align 8\n";
      }
      write!(&mut buf, "{}", block).unwrap();
    }
    for fun in &self.funs {
      for block in &fun.blocks {
        buf += "\n";
        if let Label::Prologue = block.label {
          buf += "    align 8\n";
          writeln!(&mut buf, "{}:", fun.name).unwrap();
          for instr in &block.code {
            write!(&mut buf, "    ").unwrap();
            writeln!(&mut buf, "{}", instr).unwrap();
          }
        } else {
          write!(&mut buf, "{}", block).unwrap();
        }
      }
    }
    buf
  }
}

impl<INFO: Default, VAR> Default for Program<INFO, VAR> {
  fn default() -> Self {
    Self {
      info: Default::default(),
      constants: Default::default(),
      externs: Default::default(),
      funs: Default::default(),
      blocks: Default::default(),
      types: Default::default(),
    }
  }
}

impl<VAR: Display> Display for Block<VAR> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "{}:", self.label)?;
    for instr in &self.code {
      write!(f, "    ")?;
      instr.fmt(f)?;
      writeln!(f)?;
    }
    Ok(())
  }
}

impl<VAR: Display> Display for Instr<VAR> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Add { src, dest } => write!(f, "add {}, {}", dest, src),
      Self::IMul { src, dest } => write!(f, "imul {}, {}", dest, src),
      Self::IMul3 { src, num, dest } => {
        write!(f, "imul {}, {}, {}", dest, src, num)
      }
      Self::IDiv(src) => write!(f, "idiv {}", src),
      Self::Mov { src, dest } => write!(f, "mov {}, {}", dest, src),
      Self::Call { label, .. } => write!(f, "call {}", label),
      Self::TailJmp { label, .. } => write!(f, "tailjmp {}", label),
      Self::LocalJmp(label) => write!(f, "jmp {}", label),
      Self::Jmp(label) => write!(f, "jmp {}", label),
      Self::Neg(dest) => write!(f, "neg {}", dest),
      Self::Pop(dest) => write!(f, "pop {}", dest),
      Self::Push(src) => write!(f, "push {}", src),
      Self::Ret => write!(f, "ret"),
      Self::Sub { src, dest } => write!(f, "sub {}, {}", dest, src),
      Self::Syscall => write!(f, "syscall"),
      Self::Xor { src, dest } => write!(f, "xor {}, {}", dest, src),
      Self::Cmp { src, dest } => write!(f, "cmp {}, {}", dest, src),
      Self::Movzx { src, dest } => write!(f, "movzx {}, {}", dest, src),
      Self::SetIf { cmp, dest } => write!(f, "set{} {}", cmp, dest),
      Self::JumpIf { cmp, label } => write!(f, "j{} {}", cmp, label),
      Self::Shr { dest, count } => write!(f, "shr {}, {}", dest, count),
      Self::Shl { dest, count } => write!(f, "shl {}, {}", dest, count),
      Self::And { src, dest } => write!(f, "and {}, {}", dest, src),
      Self::Or { src, dest } => write!(f, "or {}, {}", dest, src),
      Self::Lea { label, dest } => write!(f, "lea {}, [rel {}]", dest, label),
    }
  }
}

impl<VAR: Display> Display for LabelOrArg<VAR> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Label(s) => write!(f, "{}", s),
      Self::Arg(arg) => arg.fmt(f),
    }
  }
}

impl<VAR: Display> Display for Arg<VAR> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Imm(n) => write!(f, "{}", n),
      Self::Var(var) => var.fmt(f),
      Self::Deref(r, i) => match i.cmp(&0) {
        Ordering::Greater => write!(f, "qword [{} + {}]", r, i),
        Ordering::Equal => write!(f, "qword [{}]", r),
        Ordering::Less => write!(f, "qword [{} - {}]", r, -i),
      },
      Self::Reg(r) => r.fmt(f),
      Self::ByteReg(r) => r.fmt(f),
    }
  }
}

impl Reg {
  pub fn is_callee_saved(&self) -> bool {
    matches!(
      self,
      Reg::Rsp
        | Reg::Rbp
        | Reg::Rbx
        | Reg::R12
        | Reg::R13
        | Reg::R14
        | Reg::R15
    )
  }

  pub fn caller_saved_regs() -> impl IntoIterator<Item = Self> {
    use Reg::*;
    [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11]
  }

  pub fn argument_regs() -> impl IntoIterator<Item = Self> {
    use Reg::*;
    [Rdi, Rsi, Rdx, Rcx, R8, R9]
  }

  pub fn all_regs() -> impl IntoIterator<Item = Self> {
    use Reg::*;
    [
      Rsp, Rbp, Rax, Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14,
      R15,
    ]
  }
}

impl Display for Reg {
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

impl Display for ByteReg {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    use ByteReg::*;
    let op = match self {
      Al => "al",
      Ah => "ah",
      Bl => "bl",
      Bh => "bh",
      Cl => "cl",
      Ch => "ch",
      Dl => "dl",
      Dh => "dh",
    };
    f.write_str(op)
  }
}

impl Display for CmpResult {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    use CmpResult::*;
    let op = match self {
      Eq => "e",
      Ne => "ne",
      Lt => "l",
      Le => "le",
      Gt => "g",
      Ge => "ge",
    };
    f.write_str(op)
  }
}

impl Display for Label {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Start => write!(f, ".start"),
      Self::EntryPoint => write!(f, "_start"),
      Self::Prologue => write!(f, ".prologue"),
      Self::Epilogue => write!(f, ".epilogue"),
      Self::Tmp(n) => write!(f, ".block{}", n),
    }
  }
}

pub fn parse_blocks<VAR: Clone>(
  make_var: fn(&str) -> VAR,
  code: &str,
) -> Vec<Block<VAR>> {
  let mut iter = code.split(':').peekable();
  let mut label = iter.next().unwrap().trim();
  let mut blocks = vec![];
  while let Some(mut code) = iter.next() {
    let next_label;
    if iter.peek().is_some() {
      next_label = Some(code.lines().last().unwrap().trim());
      code = &code[..code.rfind('\n').unwrap()];
    } else {
      next_label = None;
    }
    let block = Block {
      label: parse_label(label),
      code: parse_code(make_var, code),
    };
    blocks.push(block);
    if let Some(next_label) = next_label {
      label = next_label;
    }
  }
  blocks
}

fn parse_label(label: &str) -> Label {
  match label {
    "epilogue" => Label::Epilogue,
    "conclusion" => Label::Epilogue,
    "start" => Label::Start,
    "_start" => Label::EntryPoint,
    _ => {
      if let Some(index) = label.strip_prefix("block") {
        Label::Tmp(index.parse().unwrap())
      } else {
        panic!("invalid label {}", label)
      }
    }
  }
}

pub fn parse_code<VAR: Clone>(
  make_var: fn(&str) -> VAR,
  code: &str,
) -> Vec<Instr<VAR>> {
  let parse_arg = |arg: &str| -> Arg<VAR> {
    if let Some(reg) = Reg::from_str(arg) {
      Arg::Reg(reg)
    } else if let Some(r) = ByteReg::from_str(arg) {
      Arg::ByteReg(r)
    } else if let Ok(n) = arg.parse::<i64>() {
      Arg::Imm(n)
    } else if arg.starts_with('[') && arg.ends_with(']') {
      let arg = &arg[1..arg.len() - 1];
      let delim = arg
        .find(|c: char| !c.is_alphanumeric())
        .unwrap_or(arg.len());
      let reg = Reg::from_str(&arg[..delim]).unwrap();
      let arg = arg[delim..].trim();
      let off = if arg.is_empty() {
        0
      } else if let Some(offset) = arg.strip_prefix('-') {
        -offset.trim().parse::<i32>().unwrap()
      } else {
        arg[1..].trim().parse::<i32>().unwrap()
      };
      Arg::Deref(reg, off)
    } else {
      Arg::Var(make_var(arg))
    }
  };

  let get_args = |arg: &str| -> Vec<Arg<VAR>> {
    arg.split(',').map(|arg| parse_arg(arg.trim())).collect()
  };

  code
    .lines()
    .filter_map(|line| {
      let line = line.trim();
      if line.is_empty() {
        return None;
      }
      let ops = line.splitn(2, ' ').collect::<Vec<_>>();
      let instr = match ops[0] {
        "add" => {
          let args = get_args(ops[1]);
          Instr::Add {
            src: args[1].clone(),
            dest: args[0].clone(),
          }
        }
        "sub" => {
          let args = get_args(ops[1]);
          Instr::Sub {
            src: args[1].clone(),
            dest: args[0].clone(),
          }
        }
        "mov" => {
          let args = get_args(ops[1]);
          Instr::Mov {
            src: args[1].clone(),
            dest: args[0].clone(),
          }
        }
        "call" => {
          let args = ops[1]
            .splitn(3, ',')
            .map(|arg| arg.trim())
            .collect::<Vec<_>>();
          if args.len() == 1 {
            Instr::Call {
              label: LabelOrArg::Label(args[0].to_owned()),
              arity: 0,
              gc: false,
            }
          } else {
            Instr::Call {
              label: LabelOrArg::Label(args[0].to_owned()),
              arity: args[1].parse().unwrap(),
              gc: if args.len() == 3 {
                assert_eq!(args[2], "gc");
                true
              } else {
                false
              },
            }
          }
        }
        "jmp" => Instr::LocalJmp(parse_label(ops[1])),
        "neg" => {
          let args = get_args(ops[1]);
          Instr::Neg(args[0].clone())
        }
        "pop" => {
          let args = get_args(ops[1]);
          Instr::Pop(args[0].clone())
        }
        "push" => {
          let args = get_args(ops[1]);
          Instr::Push(args[0].clone())
        }
        "ret" => Instr::Ret,
        "syscall" => Instr::Syscall,
        "xor" => {
          let args = get_args(ops[1]);
          Instr::Xor {
            src: args[1].clone(),
            dest: args[0].clone(),
          }
        }
        "cmp" => {
          let args = get_args(ops[1]);
          Instr::Cmp {
            src: args[1].clone(),
            dest: args[0].clone(),
          }
        }
        "movzx" => {
          let args = get_args(ops[1]);
          Instr::Movzx {
            src: args[1].clone(),
            dest: args[0].clone(),
          }
        }
        "lea" => {
          let mut args = ops[1].split(',').map(|arg| arg.trim());
          let dest = parse_arg(args.next().unwrap());
          let label = args.next().unwrap().to_owned();
          assert!(args.next().is_none(), "lea {}", ops[1]);
          Instr::Lea { label, dest }
        }
        _ => {
          if ops[0].starts_with("set") {
            let args = get_args(ops[1]);
            let cmp = CmpResult::from_str(&ops[0][3..]).unwrap();
            Instr::SetIf {
              cmp,
              dest: args[0].clone(),
            }
          } else if ops[0].starts_with('j') {
            let cmp = CmpResult::from_str(&ops[0][1..]).unwrap();
            let label = parse_label(ops[1]);
            Instr::JumpIf { cmp, label }
          } else {
            panic!("invalid instruction {}", line)
          }
        }
      };
      Some(instr)
    })
    .collect()
}

impl ByteReg {
  fn from_str(str: &str) -> Option<Self> {
    match str {
      "al" => Some(Self::Al),
      "ah" => Some(Self::Ah),
      "bl" => Some(Self::Bl),
      "bh" => Some(Self::Bh),
      "cl" => Some(Self::Cl),
      "ch" => Some(Self::Ch),
      "dl" => Some(Self::Dl),
      "dh" => Some(Self::Dh),
      _ => None,
    }
  }
}

impl From<ByteReg> for Reg {
  fn from(x: ByteReg) -> Reg {
    match x {
      ByteReg::Al | ByteReg::Ah => Reg::Rax,
      ByteReg::Bl | ByteReg::Bh => Reg::Rbx,
      ByteReg::Cl | ByteReg::Ch => Reg::Rcx,
      ByteReg::Dl | ByteReg::Dh => Reg::Rdx,
    }
  }
}

impl Reg {
  fn from_str(str: &str) -> Option<Self> {
    match str {
      "rsp" => Some(Reg::Rsp),
      "rbp" => Some(Reg::Rbp),
      "rax" => Some(Reg::Rax),
      "rbx" => Some(Reg::Rbx),
      "rcx" => Some(Reg::Rcx),
      "rdx" => Some(Reg::Rdx),
      "rsi" => Some(Reg::Rsi),
      "rdi" => Some(Reg::Rdi),
      "r8" => Some(Reg::R8),
      "r9" => Some(Reg::R9),
      "r10" => Some(Reg::R10),
      "r11" => Some(Reg::R11),
      "r12" => Some(Reg::R12),
      "r13" => Some(Reg::R13),
      "r14" => Some(Reg::R14),
      "r15" => Some(Reg::R15),
      _ => None,
    }
  }
}

impl CmpResult {
  fn from_str(str: &str) -> Option<Self> {
    match str {
      "e" => Some(Self::Eq),
      "ne" => Some(Self::Ne),
      "g" => Some(Self::Gt),
      "ge" => Some(Self::Ge),
      "l" => Some(Self::Lt),
      "le" => Some(Self::Le),
      _ => None,
    }
  }
}
