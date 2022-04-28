#[cfg(test)]
mod tests {
  use super::super::*;
  use ch2::pass::remove_complex_operands;
  use ch4::pass::explicate_control;
  use ch4::pass::shrink;
  use ch4::pass::uniquify;
  use insta::assert_snapshot;

  #[test]
  fn begin() {
    let prog = ast::parse(
      r#"
(let ([x 1] [y (read)])
  (while (> x 10)
    (set! x (begin (print "x=" x) (+ x (- y 1))))
    (print (> x (- y 1))))
  (begin 1 2 3)
  (let ([k (+ x 1)])
    (print k)
    k)
  (+ 7 (+ (read) (if (begin (set! y 37) (not (eq? y x))) 11 23)))
  (+ x y)
  (void))
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
  fn assign_void() {
    let prog = ast::parse(
      r#"(let ([x (void)] [y 1]) (set! x (print)) (set! x (set! y 10)))"#,
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
