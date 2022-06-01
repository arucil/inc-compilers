use ast::{Error, Exp, ExpKind, FuncDef, IdxVar, Program};
use std::collections::HashMap;

/// Make variable names unique.
pub fn uniquify(prog: Program) -> Program<IdxVar> {
  let mut uniq = Uniq::new();
  Program {
    func_defs: prog
      .func_defs
      .into_iter()
      .map(|(name, fun)| (name, uniq.uniquify_func(fun)))
      .collect(),
    body: prog
      .body
      .into_iter()
      .map(|exp| uniq.uniquify_exp(exp))
      .collect(),
    ..prog
  }
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

  pub fn uniquify_func<TYPE>(
    &mut self,
    func: FuncDef<String, TYPE>,
  ) -> FuncDef<IdxVar, TYPE> {
    let params1 = func
      .params
      .into_iter()
      .map(|(param, ty)| {
        let index = self.counter;
        self.counter += 1;
        let old_value = self.env.insert(param.clone(), index);
        ((IdxVar { name: param, index }, ty), old_value)
      })
      .collect::<Vec<_>>();
    let body = self.uniquify_exp(func.body);
    let mut params = Vec::with_capacity(params1.len());
    for (param, old_value) in params1 {
      if let Some(v) = old_value {
        self.env.insert(param.0.name.clone(), v);
      }
      params.push(param);
    }
    FuncDef {
      params,
      body,
      ..func
    }
  }

  pub fn uniquify_exp<TYPE>(
    &mut self,
    exp: Exp<String, TYPE>,
  ) -> Exp<IdxVar, TYPE> {
    let range = exp.range;
    let ty = exp.ty;
    match exp.kind {
      ExpKind::Int(n) => Exp {
        kind: ExpKind::Int(n),
        range,
        ty,
      },
      ExpKind::Var(var) | ExpKind::Get(var) => Exp {
        kind: ExpKind::Var(IdxVar {
          name: var.clone(),
          index: self.env[&var],
        }),
        range,
        ty,
      },
      ExpKind::Prim { op, args } => Exp {
        kind: ExpKind::Prim {
          op,
          args: args.into_iter().map(|exp| self.uniquify_exp(exp)).collect(),
        },
        range,
        ty,
      },
      ExpKind::Apply {
        func,
        args,
        r#struct,
      } => Exp {
        kind: ExpKind::Apply {
          func: box self.uniquify_exp(*func),
          args: args.into_iter().map(|exp| self.uniquify_exp(exp)).collect(),
          r#struct,
        },
        range,
        ty,
      },
      ExpKind::Let {
        var: var @ (var_range, _),
        init: box init,
        body: box body,
      } => {
        let init = self.uniquify_exp(init);
        let index = self.counter;
        self.counter += 1;
        let old_value = self.env.insert(var.1.clone(), index);
        let body = self.uniquify_exp(body);
        if let Some(v) = old_value {
          self.env.insert(var.1.clone(), v);
        }
        Exp {
          kind: ExpKind::Let {
            var: (var_range, IdxVar { name: var.1, index }),
            init: box init,
            body: box body,
          },
          range,
          ty,
        }
      }
      // ch4
      ExpKind::Bool(b) => Exp {
        kind: ExpKind::Bool(b),
        range,
        ty,
      },
      ExpKind::If { cond, conseq, alt } => Exp {
        kind: ExpKind::If {
          cond: box self.uniquify_exp(*cond),
          conseq: box self.uniquify_exp(*conseq),
          alt: box self.uniquify_exp(*alt),
        },
        range,
        ty,
      },
      ExpKind::Str(s) => Exp {
        kind: ExpKind::Str(s),
        range,
        ty,
      },
      // ch5
      ExpKind::Set { var, exp } => Exp {
        kind: ExpKind::Set {
          var: (
            var.0,
            IdxVar {
              name: var.1.clone(),
              index: self.env[&var.1],
            },
          ),
          exp: box self.uniquify_exp(*exp),
        },
        range,
        ty,
      },
      ExpKind::Begin { seq, last } => Exp {
        kind: ExpKind::Begin {
          seq: seq.into_iter().map(|exp| self.uniquify_exp(exp)).collect(),
          last: box self.uniquify_exp(*last),
        },
        range,
        ty,
      },
      ExpKind::While { cond, body } => Exp {
        kind: ExpKind::While {
          cond: box self.uniquify_exp(*cond),
          body: box self.uniquify_exp(*body),
        },
        range,
        ty,
      },
      ExpKind::Void => Exp {
        kind: ExpKind::Void,
        range,
        ty,
      },
      ExpKind::Print(args) => Exp {
        kind: ExpKind::Print(
          args.into_iter().map(|exp| self.uniquify_exp(exp)).collect(),
        ),
        range,
        ty,
      },
      ExpKind::NewLine => Exp {
        kind: ExpKind::NewLine,
        range,
        ty,
      },
      // ch6
      ExpKind::Error(Error::Length(len)) => Exp {
        kind: ExpKind::Error(Error::Length(box self.uniquify_exp(*len))),
        range,
        ty,
      },
      ExpKind::Error(Error::OutOfBounds { index, len }) => Exp {
        kind: ExpKind::Error(Error::OutOfBounds {
          index: box self.uniquify_exp(*index),
          len: box self.uniquify_exp(*len),
        }),
        range,
        ty,
      },
      ExpKind::Error(Error::DivByZero) => Exp {
        kind: ExpKind::Error(Error::DivByZero),
        range,
        ty,
      },
      // ch7
      ExpKind::FunRef { name, arity } => Exp {
        kind: ExpKind::FunRef { name, arity },
        range,
        ty,
      },
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
}
