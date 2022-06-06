use ast::{Error, Exp, ExpKind, FunDef, IdxVar, Program, Type};

pub fn expose_string_concat(
  prog: Program<IdxVar, Type>,
) -> Program<IdxVar, Type> {
  let mut state = State { tmp_counter: 0 };
  Program {
    fun_defs: prog
      .fun_defs
      .into_iter()
      .map(|(name, fun)| {
        (
          name,
          FunDef {
            body: state.exp_insert(fun.body),
            ..fun
          },
        )
      })
      .collect(),
    body: state.exp_insert(prog.body),
    ..prog
  }
}

struct State {
  tmp_counter: usize,
}

impl State {
  /// (string-append e1 e2)
  /// =>
  /// (let ([tmp1 e1]
  ///       [tmp2 e2]
  ///       [len1 (string-length tmp1)]
  ///       [result (alloc-string
  ///                 (+ len1 (string-length tmp2)))])
  ///   (copy-string! result 0 tmp1)
  ///   (copy-string! result len1 tmp2))
  fn exp_insert(&mut self, exp: Exp<IdxVar, Type>) -> Exp<IdxVar, Type> {
    match exp.kind {
      ExpKind::Int(_)
      | ExpKind::Var(_)
      | ExpKind::Str(_)
      | ExpKind::Bool(_)
      | ExpKind::Get(_)
      | ExpKind::Void
      | ExpKind::NewLine => exp,
      ExpKind::Prim {
        op: (_, "string-append"),
        mut args,
      } => {
        let range = exp.range;
        let arg2 = self.exp_insert(args.pop().unwrap());
        let arg1 = self.exp_insert(args.pop().unwrap());
        let tmp1 = self.new_tmp();
        let arg1_var = Exp {
          kind: ExpKind::Var(tmp1.clone()),
          range: exp.range,
          ty: Type::Int,
        };
        let tmp2 = self.new_tmp();
        let arg2_var = Exp {
          kind: ExpKind::Var(tmp2.clone()),
          range: exp.range,
          ty: Type::Int,
        };
        let len1 = self.new_tmp();
        let len1_var = Exp {
          kind: ExpKind::Var(len1.clone()),
          range: exp.range,
          ty: Type::Int,
        };
        let tmp3 = self.new_tmp();
        let result_var = Exp {
          kind: ExpKind::Var(tmp3.clone()),
          range: exp.range,
          ty: Type::Int,
        };
        let body = Exp {
          kind: ExpKind::Begin {
            seq: vec![
              Exp {
                kind: ExpKind::Prim {
                  op: (range, "copy-string!"),
                  args: vec![
                    result_var.clone(),
                    Exp {
                      kind: ExpKind::Int(0),
                      range,
                      ty: Type::Int,
                    },
                    arg1_var.clone(),
                  ],
                },
                range,
                ty: Type::Void,
              },
              Exp {
                kind: ExpKind::Prim {
                  op: (range, "copy-string!"),
                  args: vec![
                    result_var.clone(),
                    len1_var.clone(),
                    arg2_var.clone(),
                  ],
                },
                range,
                ty: Type::Void,
              },
            ],
            last: box result_var,
          },
          range,
          ty: Type::Str,
        };
        vec![
          (tmp1, box arg1),
          (tmp2, box arg2),
          (
            len1,
            box Exp {
              kind: ExpKind::Prim {
                op: (range, "string-length"),
                args: vec![arg1_var],
              },
              range,
              ty: Type::Int,
            },
          ),
          (
            tmp3,
            box Exp {
              kind: ExpKind::Prim {
                op: (exp.range, "alloc-string"),
                args: vec![Exp {
                  kind: ExpKind::Prim {
                    op: (exp.range, "+"),
                    args: vec![
                      len1_var,
                      Exp {
                        kind: ExpKind::Prim {
                          op: (range, "string-length"),
                          args: vec![arg2_var],
                        },
                        range,
                        ty: Type::Int,
                      },
                    ],
                  },
                  range: exp.range,
                  ty: Type::Int,
                }],
              },
              range,
              ty: Type::Str,
            },
          ),
        ]
        .into_iter()
        .rfold(body, |body, (var, init)| Exp {
          kind: ExpKind::Let {
            var: (range, var),
            init,
            body: box body,
          },
          range,
          ty: Type::Str,
        })
      }
      ExpKind::Prim { op, args } => Exp {
        kind: ExpKind::Prim {
          op,
          args: args.into_iter().map(|exp| self.exp_insert(exp)).collect(),
        },
        ..exp
      },
      ExpKind::Apply {
        fun,
        args,
        r#struct,
      } => Exp {
        kind: ExpKind::Apply {
          fun: box self.exp_insert(*fun),
          args: args.into_iter().map(|exp| self.exp_insert(exp)).collect(),
          r#struct,
        },
        ..exp
      },
      ExpKind::Let { var, init, body } => Exp {
        kind: ExpKind::Let {
          var,
          init: box self.exp_insert(*init),
          body: box self.exp_insert(*body),
        },
        ..exp
      },
      ExpKind::If { cond, conseq, alt } => Exp {
        kind: ExpKind::If {
          cond: box self.exp_insert(*cond),
          conseq: box self.exp_insert(*conseq),
          alt: box self.exp_insert(*alt),
        },
        ..exp
      },
      ExpKind::Set { var, exp: exp1 } => Exp {
        kind: ExpKind::Set {
          var,
          exp: box self.exp_insert(*exp1),
        },
        ..exp
      },
      ExpKind::Begin { seq, last } => Exp {
        kind: ExpKind::Begin {
          seq: seq.into_iter().map(|exp| self.exp_insert(exp)).collect(),
          last: box self.exp_insert(*last),
        },
        ..exp
      },
      ExpKind::While { cond, body } => Exp {
        kind: ExpKind::While {
          cond: box self.exp_insert(*cond),
          body: box self.exp_insert(*body),
        },
        ..exp
      },
      ExpKind::Print(args) => Exp {
        kind: ExpKind::Print(
          args.into_iter().map(|exp| self.exp_insert(exp)).collect(),
        ),
        ..exp
      },
      ExpKind::Error(Error::Length(arg)) => Exp {
        kind: ExpKind::Error(Error::Length(box self.exp_insert(*arg))),
        ..exp
      },
      ExpKind::Error(Error::OutOfBounds { index, len }) => Exp {
        kind: ExpKind::Error(Error::OutOfBounds {
          index: box self.exp_insert(*index),
          len: box self.exp_insert(*len),
        }),
        ..exp
      },
      ExpKind::Error(Error::DivByZero) => exp,
      ExpKind::FunRef { .. } => exp,
    }
  }

  fn new_tmp(&mut self) -> IdxVar {
    self.tmp_counter += 1;
    IdxVar {
      name: "(str-tmp)".to_owned(),
      index: self.tmp_counter,
    }
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
  fn string_append() {
    let prog = parse(
      r#"
(let ([x "\n"])
  (print (string-append "abc" (string-append "+" x))))
      "#,
    )
    .unwrap();
    let prog = typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let result = expose_string_concat(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
