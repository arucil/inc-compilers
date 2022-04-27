#[cfg(test)]
mod tests {
  use super::super::*;
  use ast::*;
  use ch2::pass::remove_complex_operands;
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
}
