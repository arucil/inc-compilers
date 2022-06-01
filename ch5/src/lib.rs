#![allow(incomplete_features)]
#![feature(box_syntax, type_changing_struct_update)]

use asm::{Label, Reg};
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
  let prog = ch4::pass::interference::build_interference(prog);
  let prog = ch4::pass::move_biasing::build_move_graph(prog);
  let regs = regs.unwrap_or(&[
    Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15,
  ]);
  let prog = ch4::pass::register_allocation::allocate_registers(prog, regs);
  let mut prog = ch2::pass::patch_instructions::patch_instructions(prog);
  self::pass::perilogue::add_perilogue(&mut prog);
  let prog = ch4::pass::merge_blocks::merge_blocks(prog);

  Ok(prog.to_nasm(true))
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
