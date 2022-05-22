use ast::{Exp, ExpKind, Program, Type};

pub fn shrink(prog: Program<String, Type>) -> Program<String, Type> {
  let body = prog.body.into_iter().map(shrink_exp).collect();
  Program { body, ..prog }
}

fn shrink_exp(exp: Exp<String, Type>) -> Exp<String, Type> {
  let range = exp.range;
  let ty = exp.ty.clone();
  match exp.kind {
    ExpKind::If { cond, conseq, alt } => Exp {
      kind: ExpKind::If {
        cond: box shrink_exp(*cond),
        conseq: box shrink_exp(*conseq),
        alt: box shrink_exp(*alt),
      },
      range,
      ty,
    },
    ExpKind::Let { var, init, body } => Exp {
      kind: ExpKind::Let {
        var,
        init: box shrink_exp(*init),
        body: box shrink_exp(*body),
      },
      range,
      ty,
    },
    ExpKind::Set { var, exp } => Exp {
      kind: ExpKind::Set {
        var,
        exp: box shrink_exp(*exp),
      },
      range,
      ty,
    },
    ExpKind::Begin { seq, last } => Exp {
      kind: ExpKind::Begin {
        seq: seq.into_iter().map(shrink_exp).collect(),
        last: box shrink_exp(*last),
      },
      range,
      ty,
    },
    ExpKind::While { cond, body } => Exp {
      kind: ExpKind::While {
        cond: box shrink_exp(*cond),
        body: box shrink_exp(*body),
      },
      range,
      ty,
    },
    ExpKind::Print(args) => Exp {
      kind: ExpKind::Print(args.into_iter().map(shrink_exp).collect()),
      range,
      ty,
    },
    ExpKind::Prim { op, args } => {
      let args: Vec<_> = args.into_iter().map(shrink_exp).collect();
      match op.1 {
        "and" => Exp {
          kind: ExpKind::If {
            cond: box args[0].clone(),
            conseq: box args[1].clone(),
            alt: box Exp {
              kind: ExpKind::Bool(false),
              range,
              ty: Type::Bool,
            },
          },
          range,
          ty,
        },
        "or" => Exp {
          kind: ExpKind::If {
            cond: box args[0].clone(),
            conseq: box Exp {
              kind: ExpKind::Bool(true),
              range,
              ty: Type::Bool,
            },
            alt: box args[1].clone(),
          },
          range,
          ty,
        },
        _ => Exp {
          kind: ExpKind::Prim { op, args },
          range,
          ty,
        },
      }
    }
    _ => exp,
  }
}
