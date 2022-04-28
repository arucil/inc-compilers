#[cfg(test)]
mod tests {
  use super::super::*;
  use ast::*;
  use ch2::pass::remove_complex_operands;
  use ch4::pass::shrink;
  use ch4::pass::uniquify;
  use insta::assert_snapshot;

  #[test]
  fn set_get() {
    let prog =
      parse(r#"(let ([x 2] [y 0]) (+ y (+ x (begin (set! x 40) x))))"#)
        .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = uncover_get::uncover_get(prog);
    let result = remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn complex() {
    let prog = ast::parse(
      r#"
(let ([x 0] [y 10] [step (- y 7)] [z (read)])
  (print "start")
  (print (if (not (eq? y z)) "y!=z " "y=z") (eq? z y))
  (while (< x y)
    (let ([t (+ x step)])
      (set! x t)
      (print
        "?="
        (if (and (> x 5) (< x 9))
          (begin
            (print "neg!")
            (set! z (+ z 1))
            (- step))
          z)))))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
