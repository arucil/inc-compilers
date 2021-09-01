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
    Exp::Prim { op, args } => {
      let args: Vec<_> = args.into_iter().map(shrink_exp).collect();
      match op.1 {
        "and" => (
          range,
          Exp::If {
            cond: box args[0].clone(),
            conseq: box args[1].clone(),
            alt: box (range.clone(), Exp::Bool(false)),
          },
        ),
        "or" => (
          range,
          Exp::If {
            cond: box args[0].clone(),
            conseq: box (range.clone(), Exp::Bool(true)),
            alt: box args[1].clone(),
          },
        ),
        _ => (range, Exp::Prim { op, args }),
      }
    }
    _ => (range, exp),
  }
}
