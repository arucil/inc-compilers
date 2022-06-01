#![allow(incomplete_features)]
#![feature(box_patterns, box_syntax, type_changing_struct_update)]

use support::CompileError;

pub mod pass;

pub fn compile(input: &str) -> Result<String, CompileError> {
  let prog = ast::parse(input)?;
  let prog = self::pass::partial_evaluation::partial_evaluate(prog);
  let prog = self::pass::uniquify::uniquify(prog);
  let prog = self::pass::remove_complex_operands::remove_complex_operands(prog);
  let prog = self::pass::explicate_control::explicate_control(prog);
  let prog = self::pass::instruction_selection::select_instruction(prog, false);
  let prog = self::pass::assign_home::assign_home(prog);
  let prog = self::pass::patch_instructions::patch_instructions(prog);
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
    )
    .unwrap();
    assert_snapshot!(prog);
  }
}
