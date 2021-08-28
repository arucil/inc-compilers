#![feature(box_patterns, box_syntax)]

use asm::{Arg, Block, Instr, Program};
use support::CompileError;

pub mod pass;

pub fn compile(input: &str) -> Result<String, CompileError> {
  let prog = ast::parse(input)?;
  let prog = self::pass::partial_evaluation::partial_evaluate(prog);
  let prog = self::pass::uniquify::uniquify(prog)?;
  let prog = self::pass::anf::anf(prog);
  let prog = self::pass::explicate_control::explicate_control(prog);
  let prog = self::pass::select_instruction::select_instruction(prog);
  let prog = self::pass::assign_home::assign_home(prog);
  let mut prog = self::pass::patch_instructions::patch_instructions(prog);
  add_prologue(&mut prog);
  add_epilogue(&mut prog);

  Ok(prog.to_nasm())
}

fn add_prologue(prog: &mut Program<self::pass::assign_home::Info>) {
  use asm::Reg::*;
  use Arg::*;
  use Instr::*;
  let stack_space = (prog.info.stack_space + 15) & !15;
  let block = Block {
    global: true,
    code: vec![
      Push(Reg(Rbp)),
      Mov {
        src: Reg(Rsp),
        dest: Reg(Rbp),
      },
      Sub {
        src: Imm(stack_space as i64),
        dest: Reg(Rsp),
      },
      Jmp("start".to_owned()),
    ],
  };
  prog.blocks.push(("_start".to_owned(), block));
}

fn add_epilogue(prog: &mut Program<self::pass::assign_home::Info>) {
  use asm::Reg::*;
  use Arg::*;
  use Instr::*;
  let block = Block {
    global: false,
    code: vec![
      Mov {
        src: Reg(Rbp),
        dest: Reg(Rsp),
      },
      Pop(Reg(Rbp)),
      Call("print_int".to_owned(), 0),
      Call("print_newline".to_owned(), 0),
      Mov {
        src: Imm(60),
        dest: Reg(Rax),
      },
      Mov {
        src: Imm(0),
        dest: Reg(Rdi),
      },
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
    let prog = compile(
      r#"
(let
  ([x (read)]
   [y (+ 2 3)])
  (+ (- (read)) (+ y (- 2))))
    "#,
    )
    .unwrap();
    assert_snapshot!(prog);
  }
}
