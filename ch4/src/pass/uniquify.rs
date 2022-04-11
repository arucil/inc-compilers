use ast::{Exp, IdxVar, Program};
use ch2::pass::uniquify::uniquify_exp as ch2_uniquify_exp;
use ch2::pass::uniquify::uniquify_exp_fn;
use support::CompileError;
use std::collections::HashMap;

pub fn uniquify(prog: Program) -> Program<IdxVar> {
  let mut counter = 0;
  let mut env = HashMap::new();
  Program {
    body: prog
      .body
      .into_iter()
      .map(|(range, exp)| {
        (
          range,
          uniquify_exp(uniquify_exp, exp, &mut env, &mut counter).unwrap(),
        )
      })
      .collect(),
  }
}

pub fn uniquify_exp(
  recur: uniquify_exp_fn,
  exp: Exp,
  env: &mut HashMap<String, usize>,
  counter: &mut usize,
) -> Result<Exp<IdxVar>, CompileError> {
  match exp {
    Exp::Bool(b) => Ok(Exp::Bool(b)),
    Exp::If { cond, conseq, alt } => Ok(Exp::If {
      cond: box (cond.0, recur(recur, cond.1, env, counter)),
      conseq: box (conseq.0, recur(recur, conseq.1, env, counter)),
      alt: box (alt.0, recur(recur, alt.1, env, counter)),
    }),
    Exp::Str(s) => Ok(Exp::Str(s)),
    Exp::Set { var, exp } => Ok(Exp::Set {
      var: (
        var.0,
        IdxVar {
          name: var.1.clone(),
          index: env[&var.1],
        },
      ),
      exp: box (exp.0, recur(recur, exp.1, env, counter)),
    }),
    Exp::Begin { seq, last } => Ok(Exp::Begin {
      seq: seq
        .into_iter()
        .map(|exp| (exp.0, recur(recur, exp.1, env, counter)))
        .collect(),
      last: box (last.0, recur(recur, last.1, env, counter)),
    }),
    Exp::While { cond, body } => Ok(Exp::While {
      cond: box (cond.0, recur(recur, cond.1, env, counter)),
      body: box (body.0, recur(recur, body.1, env, counter)),
    }),
    Exp::Print { val, ty } => Ok(Exp::Print {
      val: box (val.0, recur(recur, val.1, env, counter)),
      ty,
    }),
    Exp::NewLine => Ok(Exp::NewLine),
    exp => ch2_uniquify_exp(recur, exp, env, counter),
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
