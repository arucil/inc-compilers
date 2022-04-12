#[cfg(test)]
mod tests {
  use super::super::*;
  use ch2::pass::remove_complex_operands;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn init_with_var() {
    let prog = parse(r#"(let ([a 42]) (let ([b a]) b))"#).unwrap();
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_in_init() {
    let prog = parse(
      r#"(let ([a (+ (let ([x (read)] [y (- x)]) (+ x (- y))) 7)]) (- a))"#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn if_form() {
    let prog = parse(
      r#"(+ (- 3 2) (if (not (eq? 2 3)) (- (read)) (> (read) (not #t))))"#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn begin() {
    let prog = parse(
      r#"
(let ([x0 10])
  (let ([y1 0])
    (+ (+ (begin (set! y1 (read)) x0)
       (begin (set! x0 (read)) y1))
    x0)))"#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
