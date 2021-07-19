#![feature(box_patterns, box_syntax, bindings_after_at)]

use asm::{Program, Block, Instr, Arg, Reg};
use support::CompileError;

mod pass;

pub fn compile(input: &str) -> Result<String, CompileError> {
  let prog = ast::parse(input)?;
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
      Instr::Pushq(Arg::Reg(Reg::Rbp)),
      Instr::Movq(Arg::Reg(Reg::Rsp), Arg::Reg(Reg::Rbp)),
      Instr::Subq(Arg::Reg(Reg::Rsp), Arg::Imm(stack_space as i64)),
      Instr::Jmp("start".to_owned()),
    ]
  };
  prog.blocks.push(("_start".to_owned(), block));
}

fn add_epilogue(prog: &mut Program<self::pass::assign::Info>) {
  let block = Block {
    code: vec![
      Instr::Movq(Arg::Reg(Reg::Rbp), Arg::Reg(Reg::Rsp)),
      Instr::Popq(Arg::Reg(Reg::Rbp)),
      Instr::Jmp("start".to_owned()),
    ]
  };
  prog.blocks.push(("conclusion".to_owned(), block));
}