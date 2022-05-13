#[cfg(test)]
mod tests {
  use ast::*;
  use ch5::pass::typecheck::typecheck;
  use support::{CompileError, Range};

  #[test]
  fn vector_ref0() {
    let prog =
      parse(r#"(let ([x (vector 1 "a" #t)]) (vector-ref x 0))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 0, end: 46 },
        message: "expected Void, found Int".to_owned(),
      })
    );
  }

  #[test]
  fn vector_ref1() {
    let prog =
      parse(r#"(let ([x (vector 1 "a" #t)]) (vector-ref x 1))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 0, end: 46 },
        message: "expected Void, found Str".to_owned(),
      })
    );
  }

  #[test]
  fn nested_vector_ref() {
    let prog =
      parse(r#"(let ([x (vector 1 (vector (void)))]) (vector-ref (vector-ref x 1) 0))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn vector_len() {
    let prog = parse(r#"(let ([x (vector 1 #f)]) (vector-length x))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 0, end: 43 },
        message: "expected Void, found Int".to_owned(),
      })
    );
  }

  #[test]
  fn vector_set() {
    let prog = parse(
      r#"(let ([x (vector 1 (vector 0 "abc"))]) (vector-set! x 1 (vector 7 "")))"#,
    )
    .unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }
}
