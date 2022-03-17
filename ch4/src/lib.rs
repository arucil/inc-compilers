#![feature(box_syntax, box_patterns)]

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
  let prog = self::pass::shrink::shrink(prog);
  let prog = self::pass::uniquify::uniquify(prog);
  let prog = self::pass::anf::anf(prog);
  let prog = self::pass::explicate_control::explicate_control(prog);
  let prog = self::pass::select_instruction::select_instruction(prog);
  let prog = self::pass::liveness_analysis::analyze_liveness(
    prog,
    hashmap! {
      Label::Conclusion => {
        let mut set = LocationSet::new();
        set.add_reg(Rax);
        set.add_reg(Rbp);
        set
      }
    },
  );
  let prog = self::pass::interference::build_interference(prog);
  let prog = ch3::pass::move_biasing::build_move_graph(prog);
  let regs = regs.unwrap_or(&[
    Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15,
  ]);
  let prog = ch3::pass::register_allocation::allocate_registers(prog, regs);
  let mut prog = self::pass::patch_instructions::patch_instructions(prog);
  add_prologue(&mut prog);
  add_epilogue(&mut prog);
  let prog = self::pass::merge_jumps::merge_jumps(prog);

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
    Call {
      label: "print_int".to_owned(),
      arity: 0,
    },
    Call {
      label: "print_newline".to_owned(),
      arity: 0,
    },
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
    let prog = compile(r#"(if (eq? (read) 1) 42 0)"#, None).unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn nested_if() {
    let prog = compile(
      r#"
(let ([x (read)]
      [y (read)])
  (if (if (< x 1)
        (eq? x 0)
        (eq? x 2))
    (+ y 2)
    (+ y 10)))"#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn if_in_init() {
    let prog = compile(
      r#"(let ([x (if (>= (read) 3) 10 77)]) (if (not (eq? x 10)) 41 2))"#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }
}
