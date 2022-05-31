use ast::{Error, Exp, ExpKind, Program, Type};

pub fn insert_division_check(
  prog: Program<String, Type>,
) -> Program<String, Type> {
  Program {
    body: prog.body.into_iter().map(exp_insert).collect(),
    ..prog
  }
}

fn exp_insert(exp: Exp<String, Type>) -> Exp<String, Type> {
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
      let tmp = "(division-divisor-tmp)".to_owned();
      let arg2_var = Exp {
        kind: ExpKind::Var(tmp.clone()),
        range: exp.range,
        ty: Type::Int,
      };
      Exp {
        kind: ExpKind::Let {
          var: (arg2.range, tmp),
          init: box arg2,
          body: box Exp {
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
                  args: vec![arg1, arg2_var],
                },
                range: exp.range,
                ty: Type::Int,
              },
            },
            range: exp.range,
            ty: Type::Int,
          },
        },
        range: exp.range,
        ty: Type::Int,
      }
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
  (print (quotient 17 (remainder 30 x))))
      "#,
    )
    .unwrap();
    let prog = typecheck(prog).unwrap();
    let result = insert_division_check(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
