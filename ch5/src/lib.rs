#![feature(box_syntax)]

use asm::{Arg, Block, Instr, Label, Program, Reg};
use ch3::location_set::LocationSet;
use maplit::hashmap;
use support::CompileError;

pub mod pass;

pub fn compile(
  input: &str,
  regs: Option<&[Reg]>,
) -> Result<String, CompileError> {
  use Reg::*;
  let prog = ast::parse(input)?;
  let prog = self::pass::typecheck::typecheck(prog)?;
  let prog = ch4::pass::shrink::shrink(prog);
  let prog = ch4::pass::uniquify::uniquify(prog);
  let prog = self::pass::uncover_get::uncover_get(prog);
  let prog = ch2::pass::remove_complex_operands::remove_complex_operands(prog);
  let prog = ch4::pass::explicate_control::explicate_control(prog);
  let prog = ch4::pass::instruction_selection::select_instruction(prog, false);
  let prog = self::pass::liveness_analysis::analyze_liveness(
    prog,
    hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rbp])
    },
  );
  let prog = ch3::pass::interference::build_interference(prog);
  let prog = ch3::pass::move_biasing::build_move_graph(prog);
  let regs = regs.unwrap_or(&[
    Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15,
  ]);
  let prog = ch3::pass::register_allocation::allocate_registers(prog, regs);
  let mut prog = ch2::pass::patch_instructions::patch_instructions(prog);
  add_prologue(&mut prog);
  add_epilogue(&mut prog);
  let prog = ch4::pass::merge_blocks::merge_blocks(prog);

  Ok(prog.to_nasm())
}

fn add_prologue(prog: &mut Program<ch3::pass::register_allocation::Info>) {
  use asm::Reg::*;
  use Instr::*;
  let stack_space =
    prog.info.stack_space + prog.info.used_callee_saved_regs.len() * 8;
  let stack_space =
    ((stack_space + 15) & !15) - prog.info.used_callee_saved_regs.len() * 8;
  let mut code = vec![
    Push(Arg::Reg(Rbp)),
    Mov {
      src: Arg::Reg(Rsp),
      dest: Arg::Reg(Rbp),
    },
    Sub {
      src: Arg::Imm(stack_space as i64),
      dest: Arg::Reg(Rsp),
    },
  ];
  for &reg in &prog.info.used_callee_saved_regs {
    code.push(Push(Arg::Reg(reg)));
  }
  code.push(Jmp(Label::Start));
  let block = Block { global: true, code };
  prog.blocks.push((Label::EntryPoint, block));
}

fn add_epilogue(prog: &mut Program<ch3::pass::register_allocation::Info>) {
  use asm::Reg::*;
  use Instr::*;
  let mut code: Vec<Instr> = prog
    .info
    .used_callee_saved_regs
    .iter()
    .rev()
    .map(|&reg| Pop(Arg::Reg(reg)))
    .collect();
  code.extend_from_slice(&[
    Mov {
      src: Arg::Reg(Rbp),
      dest: Arg::Reg(Rsp),
    },
    Pop(Arg::Reg(Rbp)),
    Mov {
      src: Arg::Imm(60),
      dest: Arg::Reg(Rax),
    },
    Mov {
      src: Arg::Imm(0),
      dest: Arg::Reg(Rdi),
    },
    Syscall,
  ]);
  let block = Block {
    global: false,
    code,
  };
  prog.blocks.push((Label::Conclusion, block));
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_snapshot;

  #[test]
  fn if_form() {
    let prog = compile(r#"(print (if (eq? (read) 1) 42 0))"#, None).unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn nested_if() {
    let prog = compile(
      r#"
(let ([x (read)]
      [y (read)])
  (print
    (if (if (< x 1)
          (eq? x 0)
          (eq? x 2))
      (+ y 2)
      (+ y 10))))"#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn if_in_init() {
    let prog = compile(
      r#"(let ([x (if (>= (read) 3) 10 77)]) (print (if (not (eq? x 10)) 41 2)))"#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn while_loop() {
    let prog = compile(
      r#"
(let ([x 1] [y 2])
  (while (< x (- y))
    (set! x (+ x y))
    (set! y (if (> x 10) (- x 1) x)))
  (print (begin (set! x (+ x 1)) (if (eq? x y) "x" "Y")) "abc" (not #t) "def")
  (+ 77 (- (let ([k (read)]) (+ k 1)) 4))
  (void))
      "#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }
}
