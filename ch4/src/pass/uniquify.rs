use ast::{Exp, IdxVar, Program};
use std::collections::HashMap;

pub fn uniquify(prog: Program) -> Program<IdxVar> {
  let mut counter = 0;
  let mut env = HashMap::new();
  Program {
    body: prog
      .body
      .into_iter()
      .map(|(range, exp)| (range, uniquify_exp(exp, &mut env, &mut counter)))
      .collect(),
  }
}

fn uniquify_exp(
  exp: Exp,
  env: &mut HashMap<String, usize>,
  counter: &mut usize,
) -> Exp<IdxVar> {
  match exp {
    Exp::Int(n) => Exp::Int(n),
    Exp::Var(var) => Exp::Var(IdxVar {
      name: var.clone(),
      index: env[&var],
    }),
    Exp::Prim { op, args } => Exp::Prim {
      op,
      args: args
        .into_iter()
        .map(|(range, exp)| (range, uniquify_exp(exp, env, counter)))
        .collect(),
    },
    Exp::Let {
      var: var @ (var_range, _),
      init,
      body,
    } => {
      let init = (init.0, uniquify_exp(init.1, env, counter));
      let index = *counter;
      *counter += 1;
      let old_value = env.insert(var.1.clone(), index);
      let body = (body.0, uniquify_exp(body.1, env, counter));
      if let Some(v) = old_value {
        env.insert(var.1.clone(), v);
      }
      Exp::Let {
        var: (var_range, IdxVar { name: var.1, index }),
        init: box init,
        body: box body,
      }
    }
    Exp::Bool(b) => Exp::Bool(b),
    Exp::If { cond, conseq, alt } => Exp::If {
      cond: box (cond.0, uniquify_exp(cond.1, env, counter)),
      conseq: box (conseq.0, uniquify_exp(conseq.1, env, counter)),
      alt: box (alt.0, uniquify_exp(alt.1, env, counter)),
    },
    Exp::Str(_) => unimplemented!("string is not supported"),
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
