use ast::{Exp, Program, Range};
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};
use super::PassError;

#[derive(Clone, PartialEq, Eq)]
pub struct IdxVar {
  pub name: String,
  pub index: usize,
}

impl Debug for IdxVar {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}.{}", self.name, self.index)
  }
}

pub fn uniquify(prog: Program) -> Result<Program<IdxVar>, PassError> {
  let mut counter = 0;
  let mut env = HashMap::new();
  Ok(Program {
    body: prog.body.into_iter()
      .map(|exp| Ok((exp.0, uniquify_exp(exp, &mut env, &mut counter)?)))
      .collect::<Result<_, _>>()?,
  })
}

fn uniquify_exp(
  (range, exp): (Range, Exp),
  env: &mut HashMap<String, usize>,
  counter: &mut usize,
) -> Result<Exp<IdxVar>, PassError> {
  match exp {
    Exp::Int(n) => Ok(Exp::Int(n)),
    Exp::Var(var) => {
      env.get(&var).map_or_else(
        || Err(PassError { range, message: format!("variable {} not found", var) }),
        |&index| Ok(Exp::Var(IdxVar { name: var.clone(), index })),
      )
    }
    Exp::Prim { op, args } => {
      Ok(Exp::Prim {
        op,
        args: args.into_iter()
          .map(|exp| Ok((exp.0, uniquify_exp(exp, env, counter)?)))
          .collect::<Result<_, _>>()?,
      })
    }
    Exp::Let {
      var: var@(var_range, _),
      init: box init@(init_range, _),
      body: box body@(body_range, _),
    } => {
      let init = uniquify_exp(init, env, counter)?;
      let index = *counter;
      *counter += 1;
      let old_value = env.insert(var.1.clone(), index);
      let body = uniquify_exp(body, env, counter)?;
      if let Some(v) = old_value {
        env.insert(var.1.clone(), v);
      }
      Ok(Exp::Let {
        var: (var_range, IdxVar { name: var.1, index }),
        init: box (init_range, init),
        body: box (body_range, body),
      })
    }
    Exp::Str(_) => unimplemented!("string is not supported"),
    exp => unimplemented!("unsupported form {:?}", exp),
  }
}

#[cfg(test)]
mod tests {
  use ast::*;
  use super::*;
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
    let prog = parse(r#"(let ([x (let ([x 3]) x)]) (+ (let ([x 3] [y (read)]) (+ x y)) x))"#).unwrap();
    let result = uniquify(prog).unwrap();
    assert_snapshot!(result.to_string_pretty());
  }
}