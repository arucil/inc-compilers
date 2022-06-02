use ast::{Error, Exp, ExpKind, FunDef, IdxVar, Program, Type};
use std::collections::HashMap;
use support::Range;

pub fn limit_arity(prog: Program<IdxVar, Type>) -> Program<IdxVar, Type> {
  let mut state = State {
    env: HashMap::new(),
  };
  Program {
    fun_defs: prog
      .fun_defs
      .into_iter()
      .map(|(name, fun)| (name, state.fun_limit(fun)))
      .collect(),
    body: prog
      .body
      .into_iter()
      .map(|exp| state.exp_limit(exp))
      .collect(),
    ..prog
  }
}

struct State {
  env: HashMap<IdxVar, Param>,
}

struct Param {
  vec_type: Type,
  index: u32,
}

impl State {
  fn fun_limit(&mut self, fun: FunDef<IdxVar, Type>) -> FunDef<IdxVar, Type> {
    let mut params = fun.params;
    if params.len() > 6 {
      let vec_type =
        Type::Tuple(params.iter().skip(5).map(|p| p.1.clone()).collect());
      for (i, (var, _)) in params.drain(5..).enumerate() {
        self.env.insert(
          var,
          Param {
            vec_type: vec_type.clone(),
            index: i as u32,
          },
        );
      }
      params.push((
        IdxVar {
          name: "(arity-tmp)".to_owned(),
          index: 0,
        },
        vec_type,
      ));
    }
    let body = self.exp_limit(fun.body);
    self.env.clear();
    FunDef {
      params,
      body,
      ..fun
    }
  }

  fn exp_limit(&mut self, exp: Exp<IdxVar, Type>) -> Exp<IdxVar, Type> {
    match exp.kind {
      ExpKind::Int(_)
      | ExpKind::Str(_)
      | ExpKind::Bool(_)
      | ExpKind::Void
      | ExpKind::NewLine => exp,
      ExpKind::Var(var) | ExpKind::Get(var) => {
        if let Some(param) = self.env.get(&var) {
          make_param_ref(exp.range, exp.ty, param, None)
        } else {
          Exp {
            kind: ExpKind::Var(var),
            ..exp
          }
        }
      }
      ExpKind::Prim { op, args } => Exp {
        kind: ExpKind::Prim {
          op,
          args: args.into_iter().map(|exp| self.exp_limit(exp)).collect(),
        },
        ..exp
      },
      ExpKind::Apply {
        fun,
        args,
        r#struct,
      } => {
        if args.len() > 6 {
          let mut args: Vec<_> =
            args.into_iter().map(|exp| self.exp_limit(exp)).collect();
          let vec: Vec<_> = args.drain(5..).collect();
          let vec_range =
            (vec[0].range.start, vec.last().unwrap().range.end).into();
          let last = Exp {
            ty: Type::Tuple(vec.iter().map(|v| v.ty.clone()).collect()),
            kind: ExpKind::Prim {
              op: (vec_range, "vector"),
              args: vec,
            },
            range: vec_range,
          };
          args.push(last);
          Exp {
            kind: ExpKind::Apply {
              fun: box self.exp_limit(*fun),
              r#struct,
              args,
            },
            ..exp
          }
        } else {
          Exp {
            kind: ExpKind::Apply {
              fun: box self.exp_limit(*fun),
              args: args.into_iter().map(|exp| self.exp_limit(exp)).collect(),
              r#struct,
            },
            ..exp
          }
        }
      }
      ExpKind::Let { var, init, body } => Exp {
        kind: ExpKind::Let {
          var,
          init: box self.exp_limit(*init),
          body: box self.exp_limit(*body),
        },
        ..exp
      },
      ExpKind::If { cond, conseq, alt } => Exp {
        kind: ExpKind::If {
          cond: box self.exp_limit(*cond),
          conseq: box self.exp_limit(*conseq),
          alt: box self.exp_limit(*alt),
        },
        ..exp
      },
      ExpKind::Set { var, exp: exp1 } => {
        let val = self.exp_limit(*exp1);
        if let Some(param) = self.env.get(&var.1) {
          make_param_ref(exp.range, exp.ty, param, Some(val))
        } else {
          Exp {
            kind: ExpKind::Set { var, exp: box val },
            ..exp
          }
        }
      }
      ExpKind::Begin { seq, last } => Exp {
        kind: ExpKind::Begin {
          seq: seq.into_iter().map(|exp| self.exp_limit(exp)).collect(),
          last: box self.exp_limit(*last),
        },
        ..exp
      },
      ExpKind::While { cond, body } => Exp {
        kind: ExpKind::While {
          cond: box self.exp_limit(*cond),
          body: box self.exp_limit(*body),
        },
        ..exp
      },
      ExpKind::Print(args) => Exp {
        kind: ExpKind::Print(
          args.into_iter().map(|exp| self.exp_limit(exp)).collect(),
        ),
        ..exp
      },
      ExpKind::Error(Error::Length(arg)) => Exp {
        kind: ExpKind::Error(Error::Length(box self.exp_limit(*arg))),
        ..exp
      },
      ExpKind::Error(Error::OutOfBounds { index, len }) => Exp {
        kind: ExpKind::Error(Error::OutOfBounds {
          index: box self.exp_limit(*index),
          len: box self.exp_limit(*len),
        }),
        ..exp
      },
      ExpKind::Error(Error::DivByZero) => exp,
      ExpKind::FunRef { .. } => exp,
    }
  }
}

fn make_param_ref(
  range: Range,
  ty: Type,
  param: &Param,
  set_value: Option<Exp<IdxVar, Type>>,
) -> Exp<IdxVar, Type> {
  let name = if set_value.is_some() {
    "vector-set!"
  } else {
    "vector-ref"
  };
  let mut args = vec![
    Exp {
      kind: ExpKind::Var(IdxVar {
        name: "(arity-tmp)".to_owned(),
        index: 0,
      }),
      range,
      ty: param.vec_type.clone(),
    },
    Exp {
      kind: ExpKind::Int(param.index as i64),
      range,
      ty: Type::Int,
    },
  ];
  if let Some(v) = set_value {
    args.push(v);
  }
  Exp {
    kind: ExpKind::Prim {
      op: (range, name),
      args,
    },
    range,
    ty,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use ch4::pass::uniquify;
  use ch5::pass::typecheck::typecheck;
  use insta::assert_snapshot;

  #[test]
  fn arity() {
    let prog = parse(
      r#"
(define (foo [a Int] [b Bool]) : Void
  (print (bar a))
  (set! b (not b))
  (qux 1 2 3 4 5 6)
  (baz 1 "a" (vector) (void) b (* a 2) (void) (not b)))
(define (bar [x Int]) : Bool
  (> x 0))
(define (qux [a Int] [b Int] [c Int] [d Int] [e Int] [f Int]) : Bool
  (> a (+ b c)))
(define (baz [a Int] [b Str] [c ()] [d Void] [e Bool] [f Int] [g Void] [h Bool]) : Void
  (foo (+ a 1) (not h))
  (set! g (void))
  (print b))
      "#,
    )
    .unwrap();
    let prog = typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let result = limit_arity(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
