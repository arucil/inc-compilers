#[cfg(test)]
mod tests {
  use ch2::pass::remove_complex_operands;
  use ch4::pass::explicate_control;
  use ch4::pass::instruction_selection;
  use ch4::pass::uniquify;
  use ch5::pass::typecheck;
  use insta::assert_snapshot;

  #[test]
  fn vector() {
    let prog = ast::parse(
      r#"
(let ([x (void)] [y (vector 1 #f (void) (vector (set! x x)) "abc")])
  (set! x (vector-set! y 1 (not (vector-ref y 1))))
  (set! x (vector-ref y 2))
  (vector-set! y 0 (vector-length y))
  (vector-set! y 2 (vector-ref (vector-ref y 3) 0)))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = instruction_selection::select_instruction(prog, true);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn empty_struct() {
    let prog = ast::parse(
      r#"
(define-struct foo)
(let ([x (foo)])
  (void))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = instruction_selection::select_instruction(prog, true);
    assert_snapshot!(result.to_string_pretty());
  }
}
