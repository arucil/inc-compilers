#[cfg(test)]
mod tests {
  use ast::*;
  use ch4::pass::uniquify;
  use ch5::pass::typecheck;
  use insta::assert_snapshot;

  #[test]
  fn params() {
    let prog = parse(
      r#"
(define (foo [x Int] [y Int]) : Int
  (+ x y))
(define (bar [x Int] [y Int]) : Int
  (let ([x (+ x 1)] [f bar])
    (set! f foo)
    (* x y)))
(let ([x (if #t foo bar)])
  (print (x 1 0)))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let result = uniquify::uniquify(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
