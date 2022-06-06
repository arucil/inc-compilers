use ast::{Error, Exp, ExpKind, FunDef, IdxVar, Program, Type};

/// (remainder e1 e2) / (quotient e1 e2)
///
/// =>
///
/// (let ([x e1] [y e2])
///   (if (eq? y 0)
///     (div-by-zero)
///     (remainder/qoutient x y)))
pub fn insert_division_check(
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
        op: op @ (_, "quotient" | "remainder"),
        mut args,
      } => {
        let arg2 = self.exp_insert(args.pop().unwrap());
        let arg1 = self.exp_insert(args.pop().unwrap());
        let mut vars = vec![];
        let arg1_var;
        if arg1.kind.is_atomic() {
          arg1_var = arg1;
        } else {
          let tmp1 = self.new_tmp();
          arg1_var = Exp {
            kind: ExpKind::Var(tmp1.clone()),
            range: arg1.range,
            ty: Type::Int,
          };
          vars.push(((arg1.range, tmp1), box arg1));
        }
        let arg2_var;
        if arg2.kind.is_atomic() {
          arg2_var = arg2;
        } else {
          let tmp2 = self.new_tmp();
          arg2_var = Exp {
            kind: ExpKind::Var(tmp2.clone()),
            range: arg2.range,
            ty: Type::Int,
          };
          vars.push(((arg2.range, tmp2), box arg2));
        }
        let body = Exp {
          kind: ExpKind::If {
            cond: box Exp {
              kind: ExpKind::Prim {
                op: (exp.range, "eq?"),
                args: vec![
                  arg2_var.clone(),
                  Exp {
                    kind: ExpKind::Int(0),
                    range: exp.range,
                    ty: Type::Int,
                  },
                ],
              },
              range: exp.range,
              ty: Type::Bool,
            },
            conseq: box Exp {
              kind: ExpKind::Error(Error::DivByZero),
              range: exp.range,
              ty: Type::Int,
            },
            alt: box Exp {
              kind: ExpKind::Prim {
                op,
                args: vec![arg1_var, arg2_var],
              },
              range: exp.range,
              ty: Type::Int,
            },
          },
          range: exp.range,
          ty: Type::Int,
        };
        vars.into_iter().rfold(body, |body, (var, init)| Exp {
          kind: ExpKind::Let {
            var,
            init,
            body: box body,
          },
          range: exp.range,
          ty: Type::Int,
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
      ExpKind::Error(Error::DivByZero) => unreachable!(),
      ExpKind::FunRef { .. } => exp,
    }
  }

  fn new_tmp(&mut self) -> IdxVar {
    self.tmp_counter += 1;
    IdxVar {
      name: "(div-tmp)".to_owned(),
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
  fn division() {
    let prog = parse(
      r#"
(let ([x 3])
  (print (quotient 17 (remainder (- x) x))))
      "#,
    )
    .unwrap();
    let prog = typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let result = insert_division_check(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
