use ast::{IdxVar, Program};
use ch2::pass::uniquify::Uniq;

pub fn uniquify<TYPE>(prog: Program<String, TYPE>) -> Program<IdxVar, TYPE> {
  let mut uniq = Uniq::new();
  Program {
    func_defs: prog
      .func_defs
      .into_iter()
      .map(|(name, fun)| (name, uniq.uniquify_func(fun)))
      .collect(),
    body: prog
      .body
      .into_iter()
      .map(|exp| uniq.uniquify_exp(exp))
      .collect(),
    ..prog
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn let_form() {
    let prog = parse(r#"(let ([x (- (read))]) (+ 2 x))"#).unwrap();
    let result = uniquify(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_form_in_init() {
    let prog = parse(r#"(let ([x (let ([y 3]) y)]) (+ 2 x))"#).unwrap();
    let result = uniquify(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_form_in_init_shadows() {
    let prog = parse(r#"(let ([x (let ([x 3]) x)]) (+ 2 x))"#).unwrap();
    let result = uniquify(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_form_in_body_shadows() {
    let prog = parse(
      r#"(let ([x (let ([x 3]) x)]) (+ (let ([x 3] [y (read)]) (+ x y)) x))"#,
    )
    .unwrap();
    let result = uniquify(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn if_form() {
    let prog =
      parse(r#"(let ([x (read)]) (if (eq? x 3) (> 2 1) (not x)))"#).unwrap();
    let result = uniquify(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
