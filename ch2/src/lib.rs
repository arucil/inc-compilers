#![feature(box_patterns, box_syntax)]

use asm::{Arg, Block, Instr, Program};
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
  use asm::Reg::*;
  use Arg::*;
  use Instr::*;
  let stack_space = (prog.info.stack_space + 15) & !15;
  let block = Block {
    code: vec![
      Push(Reg(Rbp)),
      Mov(Reg(Rsp), Reg(Rbp)),
      Sub(Imm(stack_space as i64), Reg(Rsp)),
      Jmp("start".to_owned()),
    ],
  };
  prog.blocks.push(("_start".to_owned(), block));
}

fn add_epilogue(prog: &mut Program<self::pass::assign::Info>) {
  use asm::Reg::*;
  use Arg::*;
  use Instr::*;
  let block = Block {
    code: vec![
      Mov(Reg(Rbp), Reg(Rsp)),
      Pop(Reg(Rbp)),
      Call("print_int".to_owned(), 0),
      Call("print_newline".to_owned(), 0),
      Mov(Imm(60), Reg(Rax)),
      Mov(Imm(0), Reg(Rdi)),
      Syscall,
    ],
  };
  prog.blocks.push(("conclusion".to_owned(), block));
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog = compile(r#"
(let
  ([x (read)]
   [y (+ 2 3)])
  (+ (- (read)) (+ y (- 2))))
    "#).unwrap();
    assert_snapshot!(prog);
  }
}