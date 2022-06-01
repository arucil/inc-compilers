#[cfg(test)]
mod tests {
  use ast::*;
  use ch5::pass::typecheck::typecheck;
  use support::{CompileError, Range};

  #[test]
  fn fun_def() {
    let prog = parse(
      r#"
(define (foo [x Int] [y Int]) : Int
  (let ([x (+ x 1)])
    (* x y)))
(define (bar [x Int] [y Int]) : Int
  x)
(let ([x (if #t foo bar)])
  (print (x 1 (read))))
      "#,
    )
    .unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Ok(())
    );
  }

  #[test]
  fn ret_type_error() {
    let prog = parse(
      r#"
(define (foo [x Int] [y Int]) : Int
  "a")
(define (bar [x Int] [y Int]) : Int
  x)
(let ([x (if #t foo bar)])
  (print (x 1 (read))))
      "#,
    )
    .unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 1, end: 43 },
        message: "expected return type Int, found Str".to_owned(),
      })
    );
  }

  #[test]
  fn arity_mistmach() {
    let prog = parse(
      r#"
(define (foo [x Int] [y Int]) : Int
  0)
(define (bar [x Int] [y Int]) : Int
  x)
(let ([x (if #t foo bar)])
  (print (x 1)))
      "#,
    )
    .unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 119, end: 124 },
        message: "expected 2 argument(s), found 1".to_owned(),
      })
    );
  }

  #[test]
  fn arg_type_error() {
    let prog = parse(
      r#"
(define (foo [x Int] [y Int]) : Int
  0)
(define (bar [x Int] [y Int]) : Int
  x)
(let ([x (if #t foo bar)])
  (print (x 1 x)))
      "#,
    )
    .unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 119, end: 126 },
        message: "type mismatch for argument 2".to_owned(),
      })
    );
  }

  #[test]
  fn set_global_fun() {
    let prog = parse(
      r#"
(define (foo [x Int] [y Int]) : Int
  x)
(define (bar [x Int] [y Int]) : Int
  x)
(let ([x (if #t foo bar)])
  (set! x foo)
  (set! bar foo))
      "#,
    )
    .unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 127, end: 141 },
        message: "global function bar is immutable".to_owned(),
      })
    );
  }
}
