#![allow(incomplete_features)]
#![feature(type_changing_struct_update)]

use self::location_set::LocationSet;
use asm::{Label, Reg};
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
  let prog = ch2::pass::uniquify::uniquify(prog);
  let prog = ch2::pass::remove_complex_operands::remove_complex_operands(prog);
  let prog = ch2::pass::explicate_control::explicate_control(prog);
  let prog = ch2::pass::instruction_selection::select_instruction(prog, false);
  let prog = self::pass::liveness_analysis::analyze_liveness(
    prog,
    hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rbp])
    },
  );
  let prog = self::pass::interference::build_interference(prog);
  let prog = self::pass::move_biasing::build_move_graph(prog);
  let regs = regs.unwrap_or(&[
    Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15,
  ]);
  let prog = self::pass::register_allocation::allocate_registers(prog, regs);
  let prog = ch2::pass::patch_instructions::patch_instructions(prog);
  let prog = self::pass::perilogue::add_perilogue(prog);

  Ok(prog.to_nasm(true))
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
