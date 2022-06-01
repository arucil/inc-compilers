use ast::{Error, Exp, ExpKind, FuncDef, Program, Type, IdxVar};

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
  Program {
    func_defs: prog
      .func_defs
      .into_iter()
      .map(|(name, func)| {
        (
          name,
          FuncDef {
            body: exp_insert(func.body),
            ..func
          },
        )
      })
      .collect(),
    body: prog.body.into_iter().map(exp_insert).collect(),
    ..prog
  }
}

fn exp_insert(exp: Exp<IdxVar, Type>) -> Exp<IdxVar, Type> {
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
      let arg2 = exp_insert(args.pop().unwrap());
      let arg1 = exp_insert(args.pop().unwrap());
      let mut vars = vec![];
      let arg1_var;
      if arg1.kind.is_atomic() {
        arg1_var = arg1;
      } else {
        let tmp1 = "(dividend-tmp)".to_owned();
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
        let tmp2 = "(divisor-tmp)".to_owned();
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
        args: args.into_iter().map(exp_insert).collect(),
      },
      ..exp
    },
    ExpKind::Apply {
      func,
      args,
      r#struct,
    } => Exp {
      kind: ExpKind::Apply {
        func: box exp_insert(*func),
        args: args.into_iter().map(exp_insert).collect(),
        r#struct,
      },
      ..exp
    },
    ExpKind::Let { var, init, body } => Exp {
      kind: ExpKind::Let {
        var,
        init: box exp_insert(*init),
        body: box exp_insert(*body),
      },
      ..exp
    },
    ExpKind::If { cond, conseq, alt } => Exp {
      kind: ExpKind::If {
        cond: box exp_insert(*cond),
        conseq: box exp_insert(*conseq),
        alt: box exp_insert(*alt),
      },
      ..exp
    },
    ExpKind::Set { var, exp: exp1 } => Exp {
      kind: ExpKind::Set {
        var,
        exp: box exp_insert(*exp1),
      },
      ..exp
    },
    ExpKind::Begin { seq, last } => Exp {
      kind: ExpKind::Begin {
        seq: seq.into_iter().map(exp_insert).collect(),
        last: box exp_insert(*last),
      },
      ..exp
    },
    ExpKind::While { cond, body } => Exp {
      kind: ExpKind::While {
        cond: box exp_insert(*cond),
        body: box exp_insert(*body),
      },
      ..exp
    },
    ExpKind::Print(args) => Exp {
      kind: ExpKind::Print(args.into_iter().map(exp_insert).collect()),
      ..exp
    },
    ExpKind::Error(Error::Length(arg)) => Exp {
      kind: ExpKind::Error(Error::Length(box exp_insert(*arg))),
      ..exp
    },
    ExpKind::Error(Error::OutOfBounds { index, len }) => Exp {
      kind: ExpKind::Error(Error::OutOfBounds {
        index: box exp_insert(*index),
        len: box exp_insert(*len),
      }),
      ..exp
    },
    ExpKind::Error(Error::DivByZero) => unreachable!(),
    ExpKind::FunRef { .. } => exp,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
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
    let result = insert_division_check(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
