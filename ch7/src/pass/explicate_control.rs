#[cfg(test)]
mod tests {
  use ch2::pass::remove_complex_operands;
  use ch4::pass::explicate_control;
  use ch4::pass::shrink;
  use ch4::pass::uniquify;
  use ch5::pass::typecheck;
  use ch6::pass::array_bounds;
  use insta::assert_snapshot;

  #[test]
  fn call_in_cond() {
    let prog = ast::parse(
      r#"
(define (foo [x (Int Bool Void)]) : Bool
  (if (vector-ref x 1)
    (not (vector-ref x 1))
    (> 3 2)))
(print
  (if (foo (vector (read) #t (void)))
    (begin
      (print 3)
      #t)
    #f))
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
  fn call_effect() {
    let prog = ast::parse(
      r#"
(define (foo [x (Int Bool Void)]) : Bool
  (if (vector-ref x 1)
    (not (vector-ref x 1))
    (> 3 2)))
(foo (vector 1 (foo (vector 1 #f (void))) (newline)))
(void)
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = array_bounds::insert_bounds_check(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control::explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn void_in_param() {
    let prog = ast::parse(
      r#"
(define (foo [x Int] [y Void] [z Str]) : Bool
  (set! y (void))
  (let ([t (void)])
    (set! t y))
  y
  (print z)
  (not (eq? x 3)))
(void)
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = array_bounds::insert_bounds_check(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control::explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn tailcall() {
    let prog = ast::parse(
      r#"
(define (foo [x Int] [y Void] [z Str]) : Bool
  (set! y (void))
  (let ([t (void)])
    (set! t y))
  y
  (print z)
  (foo (+ x 1) y "z"))
(void)
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = array_bounds::insert_bounds_check(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control::explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
