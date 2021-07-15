use num_derive::FromPrimitive;

#[derive(Debug, Clone)]
pub struct Program {
  pub blocks: Vec<(String, Block)>,
}

#[derive(Debug, Clone)]
pub struct Block {
  pub code: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub enum Instr {
  Addq(Arg, Arg),
  Subq(Arg, Arg),
  Movq(Arg, Arg),
  Negq(Arg),
  Callq(String, usize),
  Retq,
  Pushq(Arg),
  Popq(Arg),
  Jmp(String),
}

#[derive(Debug, Clone)]
pub enum Arg {
  Imm(i64),
  Reg(Reg),
  Deref(Reg, i32),
}

#[derive(Debug, Clone, FromPrimitive)]
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
