use ast::{Error, Exp, ExpKind, IdxVar, Program};
use std::collections::HashMap;
use support::CompileError;

/// Make variable names unique.
pub fn uniquify(prog: Program) -> Result<Program<IdxVar>, CompileError> {
  let mut uniq = Uniq::new();
  Ok(Program {
    body: prog
      .body
      .into_iter()
      .map(|exp| uniq.uniquify_exp(exp))
      .collect::<Result<_, _>>()?,
    ..prog
  })
}

#[derive(Default)]
pub struct Uniq {
  env: HashMap<String, usize>,
  counter: usize,
}

impl Uniq {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn uniquify_exp<TYPE>(
    &mut self,
    exp: Exp<String, TYPE>,
  ) -> Result<Exp<IdxVar, TYPE>, CompileError> {
    let range = exp.range;
    let ty = exp.ty;
    match exp.kind {
      ExpKind::Int(n) => Ok(Exp {
        kind: ExpKind::Int(n),
        range,
        ty,
      }),
      ExpKind::Var(var) | ExpKind::Get(var) => self.env.get(&var).map_or_else(
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
            .map(|exp| self.uniquify_exp(exp))
            .collect::<Result<_, _>>()?,
        },
        range,
        ty,
      }),
      ExpKind::Call { name, args } => Ok(Exp {
        kind: ExpKind::Call {
          name,
          args: args
            .into_iter()
            .map(|exp| self.uniquify_exp(exp))
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
        let init = self.uniquify_exp(init)?;
        let index = self.counter;
        self.counter += 1;
        let old_value = self.env.insert(var.1.clone(), index);
        let body = self.uniquify_exp(body)?;
        if let Some(v) = old_value {
          self.env.insert(var.1.clone(), v);
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
          cond: box self.uniquify_exp(*cond)?,
          conseq: box self.uniquify_exp(*conseq)?,
          alt: box self.uniquify_exp(*alt)?,
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
              index: self.env[&var.1],
            },
          ),
          exp: box self.uniquify_exp(*exp)?,
        },
        range,
        ty,
      }),
      ExpKind::Begin { seq, last } => Ok(Exp {
        kind: ExpKind::Begin {
          seq: seq
            .into_iter()
            .map(|exp| self.uniquify_exp(exp))
            .collect::<Result<_, _>>()?,
          last: box self.uniquify_exp(*last)?,
        },
        range,
        ty,
      }),
      ExpKind::While { cond, body } => Ok(Exp {
        kind: ExpKind::While {
          cond: box self.uniquify_exp(*cond)?,
          body: box self.uniquify_exp(*body)?,
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
            .map(|exp| self.uniquify_exp(exp))
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
      // ch6
      ExpKind::Error(Error::Length(len)) => Ok(Exp {
        kind: ExpKind::Error(Error::Length(box self.uniquify_exp(*len)?)),
        range,
        ty,
      }),
      ExpKind::Error(Error::OutOfBounds { index, len }) => Ok(Exp {
        kind: ExpKind::Error(Error::OutOfBounds {
          index: box self.uniquify_exp(*index)?,
          len: box self.uniquify_exp(*len)?,
        }),
        range,
        ty,
      }),
      ExpKind::Error(Error::DivByZero) => Ok(Exp {
        kind: ExpKind::Error(Error::DivByZero),
        range,
        ty,
      }),
    }
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
