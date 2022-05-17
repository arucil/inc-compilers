#![feature(box_patterns, box_syntax)]

use asm::{Arg, Block, Instr, Label, Program};
use support::CompileError;

pub mod pass;

pub fn compile(input: &str) -> Result<String, CompileError> {
  let prog = ast::parse(input)?;
  let prog = self::pass::partial_evaluation::partial_evaluate(prog);
  let prog = self::pass::uniquify::uniquify(prog)?;
  let prog = self::pass::remove_complex_operands::remove_complex_operands(prog);
  let prog = self::pass::explicate_control::explicate_control(prog);
  let prog = self::pass::instruction_selection::select_instruction(prog, false);
  let prog = self::pass::assign_home::assign_home(prog);
  let mut prog = self::pass::patch_instructions::patch_instructions(prog);
  add_prologue(&mut prog);
  add_epilogue(&mut prog);

  Ok(prog.to_nasm(true))
}

fn add_prologue(prog: &mut Program<self::pass::assign_home::Info>) {
  use asm::Reg::*;
  use Instr::*;
  let stack_space = (prog.info.stack_space + 15) & !15;
  let block = Block {
    global: true,
    code: vec![
      Push(Arg::Reg(Rbp)),
      Mov {
        src: Arg::Reg(Rsp),
        dest: Arg::Reg(Rbp),
      },
      Sub {
        src: Arg::Imm(stack_space as i64),
        dest: Arg::Reg(Rsp),
      },
      Jmp(Label::Start),
    ],
  };
  prog.blocks.push((Label::EntryPoint, block));
}

fn add_epilogue(prog: &mut Program<self::pass::assign_home::Info>) {
  use asm::Reg::*;
  use Instr::*;
  let block = Block {
    global: false,
    code: vec![
      Mov {
        src: Arg::Reg(Rbp),
        dest: Arg::Reg(Rsp),
      },
      Pop(Arg::Reg(Rbp)),
      Mov {
        src: Arg::Reg(Rax),
        dest: Arg::Reg(Rdi),
      },
      Call {
        label: "rt_print_int".to_owned(),
        arity: 0,
      },
      Call {
        label: "rt_print_newline".to_owned(),
        arity: 0,
      },
      Mov {
        src: Arg::Imm(60),
        dest: Arg::Reg(Rax),
      },
      Mov {
        src: Arg::Imm(0),
        dest: Arg::Reg(Rdi),
      },
      Syscall,
    ],
  };
  prog.blocks.push((Label::Conclusion, block));
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
