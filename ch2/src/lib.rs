#![feature(box_patterns, box_syntax, bindings_after_at)]

use asm::{Arg, Block, Instr, Program, Reg};
use support::CompileError;

pub mod pass;

pub fn compile(input: &str) -> Result<String, CompileError> {
  let prog = ast::parse(input)?;
  let prog = self::pass::partial::partial_evaluate(prog);
  let prog = self::pass::uniquify::uniquify(prog)?;
  let prog = self::pass::anf::anf(prog);
  let prog = self::pass::control::explicate_control(prog);
  let prog = self::pass::instruction::select_instruction(prog);
  let prog = self::pass::assign::assign_home(prog);
  let mut prog = self::pass::patch::patch_instructions(prog);
  add_prologue(&mut prog);
  add_epilogue(&mut prog);

  Ok(prog.to_nasm())
}

fn add_prologue(prog: &mut Program<self::pass::assign::Info>) {
  let stack_space = (prog.info.stack_space + 15) & !15;
  let block = Block {
    code: vec![
      Instr::Push(Arg::Reg(Reg::Rbp)),
      Instr::Mov(Arg::Reg(Reg::Rsp), Arg::Reg(Reg::Rbp)),
      Instr::Sub(Arg::Imm(stack_space as i64), Arg::Reg(Reg::Rsp)),
      Instr::Jmp("start".to_owned()),
    ],
  };
  prog.blocks.push(("_start".to_owned(), block));
}

fn add_epilogue(prog: &mut Program<self::pass::assign::Info>) {
  let block = Block {
    code: vec![
      Instr::Mov(Arg::Reg(Reg::Rbp), Arg::Reg(Reg::Rsp)),
      Instr::Pop(Arg::Reg(Reg::Rbp)),
      Instr::Call("print_int".to_owned(), 0),
      Instr::Call("print_newline".to_owned(), 0),
      Instr::Mov(Arg::Imm(60), Arg::Reg(Reg::Rax)),
      Instr::Mov(Arg::Imm(0), Arg::Reg(Reg::Rdi)),
      Instr::Syscall,
    ],
  };
  prog.blocks.push(("conclusion".to_owned(), block));
}
