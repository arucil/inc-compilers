#[cfg(test)]
mod tests {
  use super::super::*;
  use ast::*;
  use ch2::pass::remove_complex_operands;
  use ch4::pass::explicate_control;
  use ch4::pass::instruction_selection;
  use ch4::pass::uniquify;
  use insta::assert_snapshot;

  #[test]
  fn single_read() {
    let prog = parse(r#"(begin (read) (void))"#).unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = instruction_selection::select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn assign_void() {
    let prog =
      parse(r#"(let ([x (void)] [y (set! x (print 3))]) (set! x (print #t)))"#)
        .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = instruction_selection::select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
