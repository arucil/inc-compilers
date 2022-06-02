#[cfg(test)]
mod tests {
  use ch2::pass::remove_complex_operands;
  use ch4::pass::shrink;
  use ch4::pass::uniquify;
  use ch5::pass::typecheck;
  use insta::assert_snapshot;

  #[test]
  fn fun_ref() {
    let prog = ast::parse(
      r#"
(define (foo [f (Int -> Int)]) : Void
  (f 1)
  (void))
(define (bar [x Int]) : Int
  (+ (bar x) 1))
(let ([x bar])
  (foo bar)
  (foo x))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn complex() {
    let prog = ast::parse(
      r#"
(define (foo [x (array-of (array-of Int))]) : Void
  (let ([y (read)] [z y])
    (vector-set! (vector-ref x (remainder (quotient 7 y) 2)) 3 z)
    (and #t (> z 1))
    (set! z (+ z 1))))
(define-type my-str
  (struct
    [s Str]))
(define (bar [s my-str]) : Void
  (set-my-str-s! s (string-append "-" (string-append "," (my-str-s s)))))
(void)
      "#,
    )
    .unwrap();
    let prog = ch5::pass::typecheck::typecheck(prog).unwrap();
    let prog = ch6::pass::r#struct::desugar_struct(prog);
    let prog = ch4::pass::shrink::shrink(prog);
    let prog = ch4::pass::uniquify::uniquify(prog);
    let prog = ch5::pass::uncover_get::uncover_get(prog);
    let prog = ch6::pass::array_bounds::insert_bounds_check(prog);
    let prog = ch6::pass::division::insert_division_check(prog);
    let prog = ch6::pass::string::expose_string_concat(prog);
    let prog =
      ch2::pass::remove_complex_operands::remove_complex_operands(prog);
    assert_snapshot!(prog.to_string_pretty());
  }
}
