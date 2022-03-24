use self::location_set::LocationSet;
use asm::{Arg, Block, Instr, Label, Program, Reg};
use maplit::hashmap;
use support::CompileError;

pub mod location_graph;
pub mod location_set;
pub mod pass;

pub fn compile(
  input: &str,
  regs: Option<&[Reg]>,
) -> Result<String, CompileError> {
  use Reg::*;
  let prog = ast::parse(input)?;
  let prog = ch2::pass::partial_evaluation::partial_evaluate(prog);
  let prog = ch2::pass::uniquify::uniquify(prog)?;
  let prog = ch2::pass::remove_complex_operands::remove_complex_operands(prog);
  let prog = ch2::pass::explicate_control::explicate_control(prog);
  let prog = ch2::pass::select_instruction::select_instruction(prog);
  let prog = self::pass::liveness_analysis::analyze_liveness(
    prog,
    hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rbp])
    },
  );
  let prog = self::pass::interference::build_interference(prog);
  let prog = self::pass::move_biasing::build_move_graph(prog);
  let regs = regs.unwrap_or(&[
    Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15,
  ]);
  let prog = self::pass::register_allocation::allocate_registers(prog, regs);
  let mut prog = ch2::pass::patch_instructions::patch_instructions(prog);
  add_prologue(&mut prog);
  add_epilogue(&mut prog);

  Ok(prog.to_nasm())
}

fn add_prologue(prog: &mut Program<self::pass::register_allocation::Info>) {
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

fn add_epilogue(prog: &mut Program<self::pass::register_allocation::Info>) {
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
  fn nested_prims() {
    let prog = compile(
      r#"
(let
  ([x (read)]
   [y (+ 2 3)])
  (+ (- (read)) (+ y (- 2))))
    "#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn spilled() {
    let prog = compile(r#"
(let
  ([a (read)]
   [b (+ 2 3)]
   [c (- a)]
   [d 7]
   [e (+ (+ a c) b)]
   [f (- 12)]
   [g -50]
   [h 1]
   [i 0]
   [j 21]
   [k 77]
   [l 13]
   [m 13]
   [n 47]
   [o 1758]
   [p 1000])
  (+ (- a) (+ b (+ (+ c d) (+ e (+ (+ f (+ g (+ h (+ i (+ j (+ k (+ l (+ m (+ n o))))))))) p))))))
    "#, None).unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn move_biasing() {
    let prog = compile(
      r#"
(let ([v 1])
  (let ([w 42])
    (let ([x (+ v 7)])
      (let ([y x])
        (let ([z (+ x w)])
          (+ z (- y)))))))
    "#,
      Some(&[Reg::Rbx, Reg::Rcx]),
    )
    .unwrap();
    assert_snapshot!(prog);
  }
}
