use super::explicate_control::CInfo;
use asm::Program;
use ast::IdxVar;
use ch2::pass::instruction_selection::{CodeGen, Info};
use control::*;

pub fn select_instruction(prog: CProgram<CInfo>) -> Program<Info, IdxVar> {
  let mut code_gen = CodeGen::new();
  Program {
    info: Info {
      locals: prog.info.locals,
    },
    blocks: prog
      .body
      .into_iter()
      .map(|(label, tail)| (label, code_gen.tail_block(tail)))
      .collect(),
    constants: code_gen.into_constants(),
  }
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use ast::*;
  use ch2::pass::remove_complex_operands;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn nested_if() {
    let prog = ast::parse(
      r#"
(let ([x (read)]
      [y (read)])
  (if (if (< x 1)
        (eq? x 0)
        (eq? x 2))
    (+ y 2)
    (+ y 10)))
    "#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn if_in_init() {
    let prog = ast::parse(
      r#"(let ([x (if (>= (read) 3) 10 77)]) (if (not (eq? x 10)) 41 2))"#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn complex_if() {
    let prog = ast::parse(
      r#"
(let ([x (read)])
  (if (> x 100)
    (let ([y (read)])
      (if (or (and (>= x y) (< (- x y) 10))
              (and (< x y) (< (- y x) 10)))
        -1
        (- y)))
    (if (and (> x 40) (< x 60))
      5000
      x)))
      "#,
    )
    .unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
