use ast::{Exp, Program};
use support::Range;

pub fn shrink(prog: Program) -> Program {
  let body = prog.body.into_iter().map(shrink_exp).collect();
  Program { body }
}

fn shrink_exp((range, exp): (Range, Exp)) -> (Range, Exp) {
  match exp {
    Exp::If { cond, conseq, alt } => (
      range,
      Exp::If {
        cond: box shrink_exp(*cond),
        conseq: box shrink_exp(*conseq),
        alt: box shrink_exp(*alt),
      },
    ),
    Exp::Let { var, init, body } => (
      range,
      Exp::Let {
        var,
        init: box shrink_exp(*init),
        body: box shrink_exp(*body),
      },
    ),
    Exp::Set { var, exp } => (
      range,
      Exp::Set {
        var,
        exp: box shrink_exp(*exp),
      },
    ),
    Exp::Begin { seq, last } => (
      range,
      Exp::Begin {
        seq: seq.into_iter().map(shrink_exp).collect(),
        last: box shrink_exp(*last),
      },
    ),
    Exp::While { cond, body } => (
      range,
      Exp::While {
        cond: box shrink_exp(*cond),
        body: box shrink_exp(*body),
      },
    ),
    Exp::Print { args, types } => (
      range,
      Exp::Print {
        args: args.into_iter().map(shrink_exp).collect(),
        types,
      },
    ),
    Exp::Prim { op, args } => {
      let args: Vec<_> = args.into_iter().map(shrink_exp).collect();
      match op.1 {
        "and" => (
          range,
          Exp::If {
            cond: box args[0].clone(),
            conseq: box args[1].clone(),
            alt: box (range, Exp::Bool(false)),
          },
        ),
        "or" => (
          range,
          Exp::If {
            cond: box args[0].clone(),
            conseq: box (range, Exp::Bool(true)),
            alt: box args[1].clone(),
          },
        ),
        _ => (range, Exp::Prim { op, args }),
      }
    }
    _ => (range, exp),
  }
}
