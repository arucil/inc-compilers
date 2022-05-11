use ast::{Exp, ExpKind, IdxVar, Program};
use std::collections::HashMap;
use support::CompileError;

/// Make variable names unique.
pub fn uniquify(prog: Program) -> Result<Program<IdxVar>, CompileError> {
  let mut counter = 0;
  let mut env = HashMap::new();
  Ok(Program {
    body: prog
      .body
      .into_iter()
      .map(|exp| uniquify_exp(exp, &mut env, &mut counter))
      .collect::<Result<_, _>>()?,
    types: prog.types,
  })
}

pub fn uniquify_exp<TYPE>(
  exp: Exp<String, TYPE>,
  env: &mut HashMap<String, usize>,
  counter: &mut usize,
) -> Result<Exp<IdxVar, TYPE>, CompileError> {
  let range = exp.range;
  let ty = exp.ty;
  match exp.kind {
    ExpKind::Int(n) => Ok(Exp {
      kind: ExpKind::Int(n),
      range,
      ty,
    }),
    ExpKind::Var(var) | ExpKind::Get(var) => env.get(&var).map_or_else(
      || {
        Err(CompileError {
          range,
          message: format!("variable {} not found", var),
        })
      },
      |&index| {
        Ok(Exp {
          kind: ExpKind::Var(IdxVar {
            name: var.clone(),
            index,
          }),
          range,
          ty,
        })
      },
    ),
    ExpKind::Prim { op, args } => Ok(Exp {
      kind: ExpKind::Prim {
        op,
        args: args
          .into_iter()
          .map(|exp| uniquify_exp(exp, env, counter))
          .collect::<Result<_, _>>()?,
      },
      range,
      ty,
    }),
    ExpKind::Let {
      var: var @ (var_range, _),
      init: box init,
      body: box body,
    } => {
      let init = uniquify_exp(init, env, counter)?;
      let index = *counter;
      *counter += 1;
      let old_value = env.insert(var.1.clone(), index);
      let body = uniquify_exp(body, env, counter)?;
      if let Some(v) = old_value {
        env.insert(var.1.clone(), v);
      }
      Ok(Exp {
        kind: ExpKind::Let {
          var: (var_range, IdxVar { name: var.1, index }),
          init: box init,
          body: box body,
        },
        range,
        ty,
      })
    }
    // ch4
    ExpKind::Bool(b) => Ok(Exp {
      kind: ExpKind::Bool(b),
      range,
      ty,
    }),
    ExpKind::If { cond, conseq, alt } => Ok(Exp {
      kind: ExpKind::If {
        cond: box uniquify_exp(*cond, env, counter)?,
        conseq: box uniquify_exp(*conseq, env, counter)?,
        alt: box uniquify_exp(*alt, env, counter)?,
      },
      range,
      ty,
    }),
    ExpKind::Str(s) => Ok(Exp {
      kind: ExpKind::Str(s),
      range,
      ty,
    }),
    // ch5
    ExpKind::Set { var, exp } => Ok(Exp {
      kind: ExpKind::Set {
        var: (
          var.0,
          IdxVar {
            name: var.1.clone(),
            index: env[&var.1],
          },
        ),
        exp: box uniquify_exp(*exp, env, counter)?,
      },
      range,
      ty,
    }),
    ExpKind::Begin { seq, last } => Ok(Exp {
      kind: ExpKind::Begin {
        seq: seq
          .into_iter()
          .map(|exp| uniquify_exp(exp, env, counter))
          .collect::<Result<_, _>>()?,
        last: box uniquify_exp(*last, env, counter)?,
      },
      range,
      ty,
    }),
    ExpKind::While { cond, body } => Ok(Exp {
      kind: ExpKind::While {
        cond: box uniquify_exp(*cond, env, counter)?,
        body: box uniquify_exp(*body, env, counter)?,
      },
      range,
      ty,
    }),
    ExpKind::Void => Ok(Exp {
      kind: ExpKind::Void,
      range,
      ty,
    }),
    ExpKind::Print(args) => Ok(Exp {
      kind: ExpKind::Print(
        args
          .into_iter()
          .map(|exp| uniquify_exp(exp, env, counter))
          .collect::<Result<_, _>>()?,
      ),
      range,
      ty,
    }),
    ExpKind::NewLine => Ok(Exp {
      kind: ExpKind::NewLine,
      range,
      ty,
    }),
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
