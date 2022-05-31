#[cfg(test)]
mod tests {
  use ch2::pass::remove_complex_operands;
  use ch4::pass::explicate_control;
  use ch4::pass::shrink;
  use ch4::pass::uniquify;
  use ch5::pass::typecheck;
  use insta::assert_snapshot;

  use crate::pass::array_bounds;
  use crate::pass::division;
  use crate::pass::r#struct;

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
    let prog = array_bounds::insert_bounds_check(prog);
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
(define-type my-void
  (struct
    [x Void]))
(define-type my-comp
  (struct
    [x Int]
    [y Bool]
    [z Void]
    [u my-void]
    [v Str]))
(let ([x (void)]
      [y (my-comp 1 #f (void) (my-void (set! x x)) "abc")])
  (set! x (set-my-comp-y! y (not (my-comp-y y))))
  (set! x (my-comp-z y))
  (set-my-comp-x! y 5) ;(vector-length y))
  (set-my-comp-z! y (my-void-x (my-comp-u y))))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = array_bounds::insert_bounds_check(prog);
    let prog = r#struct::desugar_struct(prog);
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control::explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn array() {
    let prog = ast::parse(
      r#"
(let ([x (make-vector 3 #t)]
      [y (make-vector 0 2)]
      [z (make-vector 2 x)]
      [t (make-vector 10 (void))]
      [u (void)])
  (vector-set! x 1 #f)
  (vector-set! t 3 (void))
  (set! u (vector-ref t (vector-ref y 0)))
  (print (vector-length t)))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = array_bounds::insert_bounds_check(prog);
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control::explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn gcd() {
    let prog = ast::parse(
      r#"
(let ([a0 (read)] [b0 (read)] [a a0] [b b0])
  (print (quotient a b))
  (while (not (eq? b 0))
    (let ([tmp b])
      (set! b (remainder a b))
      (set! a tmp)
      (print "a=" a ", b=" b)))
  (print "gcd=" a ", lcm=" (quotient (* a0 b0) a)))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = array_bounds::insert_bounds_check(prog);
    let prog = division::insert_division_check(prog);
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control::explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
