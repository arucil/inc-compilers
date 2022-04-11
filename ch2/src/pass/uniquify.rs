use ast::{Exp, IdxVar, Program};
use std::collections::HashMap;
use support::{CompileError, Range};

/// Make variable names unique.
pub fn uniquify(prog: Program) -> Result<Program<IdxVar>, CompileError> {
  let mut counter = 0;
  let mut env = HashMap::new();
  Ok(Program {
    body: prog
      .body
      .into_iter()
      .map(|exp| {
        Ok((
          exp.0,
          uniquify_exp(uniquify_exp, exp, &mut env, &mut counter)?,
        ))
      })
      .collect::<Result<_, _>>()?,
  })
}

pub type uniquify_exp_fn = fn(
  uniquify_exp_fn,
  (Range, Exp),
  &mut HashMap<String, usize>,
  &mut usize,
) -> Result<Exp<IdxVar>, CompileError>;

pub fn uniquify_exp(
  recur: uniquify_exp_fn,
  (range, exp): (Range, Exp),
  env: &mut HashMap<String, usize>,
  counter: &mut usize,
) -> Result<Exp<IdxVar>, CompileError> {
  match exp {
    Exp::Int(n) => Ok(Exp::Int(n)),
    Exp::Var(var) => env.get(&var).map_or_else(
      || {
        Err(CompileError {
          range,
          message: format!("variable {} not found", var),
        })
      },
      |&index| {
        Ok(Exp::Var(IdxVar {
          name: var.clone(),
          index,
        }))
      },
    ),
    Exp::Prim { op, args } => Ok(Exp::Prim {
      op,
      args: args
        .into_iter()
        .map(|exp| Ok((exp.0, recur(recur, exp, env, counter)?)))
        .collect::<Result<_, _>>()?,
    }),
    Exp::Let {
      var: var @ (var_range, _),
      init: box init @ (init_range, _),
      body: box body @ (body_range, _),
    } => {
      let init = recur(recur, init, env, counter)?;
      let index = *counter;
      *counter += 1;
      let old_value = env.insert(var.1.clone(), index);
      let body = recur(recur, body, env, counter)?;
      if let Some(v) = old_value {
        env.insert(var.1.clone(), v);
      }
      Ok(Exp::Let {
        var: (var_range, IdxVar { name: var.1, index }),
        init: box (init_range, init),
        body: box (body_range, body),
      })
    }
    exp => unimplemented!("unsupported form {:?}", exp),
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
    let result = uniquify(prog).unwrap();
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_form_in_init() {
    let prog = parse(r#"(let ([x (let ([y 3]) y)]) (+ 2 x))"#).unwrap();
    let result = uniquify(prog).unwrap();
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_form_in_init_shadows() {
    let prog = parse(r#"(let ([x (let ([x 3]) x)]) (+ 2 x))"#).unwrap();
    let result = uniquify(prog).unwrap();
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_form_in_body_shadows() {
    let prog = parse(
      r#"(let ([x (let ([x 3]) x)]) (+ (let ([x 3] [y (read)]) (+ x y)) x))"#,
    )
    .unwrap();
    let result = uniquify(prog).unwrap();
    assert_snapshot!(result.to_string_pretty());
  }
}
