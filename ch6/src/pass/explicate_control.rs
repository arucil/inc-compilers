#[cfg(test)]
mod tests {
  use ch2::pass::remove_complex_operands;
  use ch4::pass::explicate_control;
  use ch4::pass::shrink;
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
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control::explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn r#struct() {
    let prog = ast::parse(
      r#"
(define-struct my-void (x Void))
(define-struct my-comp (x Int) (y Bool) (z Void) (u my-void) (v Str))
(let ([x (void)] [y (my-comp 1 #f (void) (my-void (set! x x)) "abc")])
  (set! x (set-my-comp-y! y (not (my-comp-y y))))
  (set! x (my-comp-z y))
  (set-my-comp-x! y (vector-length y))
  (set-my-comp-z! y (my-void-x (my-comp-u y))))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control::explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
