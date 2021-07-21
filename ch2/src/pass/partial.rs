use ast::{Exp, Program};
use support::Range;

pub fn partial_evaluate(prog: Program) -> Program {
  Program {
    body: prog.body.into_iter()
      .map(|(range, exp)| (range, pe_exp(exp)))
      .collect()
  }
}

fn pe_exp(exp: Exp) -> Exp {
  match exp {
    Exp::Int(n) => Exp::Int(n),
    Exp::Str(s) => Exp::Str(s),
    Exp::Var(var) => Exp::Var(var),
    Exp::Let { var, init, body } => {
      Exp::Let {
        var,
        init: box (init.0, pe_exp(init.1)),
        body: box (body.0, pe_exp(body.1)),
      }
    }
    Exp::Prim { op, args } => {
      pe_prim(op, args)
    }
    e => unimplemented!("{:?}", e),
  }
}

fn pe_prim(
  op: (Range, &'static str),
  args: Vec<(Range, Exp)>,
) -> Exp {
  let mut args = args.into_iter()
    .map(|(range, exp)| (range, pe_exp(exp)))
    .collect::<Vec<_>>();
  match (op.1, &mut args[..]) {
    ("+", [(_, Exp::Int(a)), (_, Exp::Int(b))]) => {
      Exp::Int(*a + *b)
    }
    ("+", [(range1, Exp::Int(a)), (_, Exp::Prim { op: (_, "+"), args: subargs })])
    => {
      match subargs[0].1 {
        Exp::Int(b) => {
          Exp::Prim {
            op: (op.0, "+"),
            args: vec![
              (*range1, Exp::Int(*a + b)),
              subargs.pop().unwrap(),
            ],
          }
        }
        _ => {
          Exp::Prim {
            op: (op.0, "+"),
            args,
          }
        }
      }
    }
    ("+", [_, (_, Exp::Int(_))]) => {
      pe_exp(Exp::Prim {
        op: (op.0, "+"),
        args: args.into_iter()
          .rev()
          .collect(),
      })
    }
    ("-", [(_, Exp::Int(a))]) => {
      Exp::Int(-*a)
    }
    _ => Exp::Prim { op, args }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog = parse(r#"(+ 1 (+ (read) 2))"#).unwrap();
    let result = partial_evaluate(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn nested_prims2() {
    let prog = parse(r#"(+ (+ 7 (read)) 3)"#).unwrap();
    let result = partial_evaluate(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn fold_constant() {
    let prog = parse(r#"(+ 1 (+ (+ (- 7) 2) (read)))"#).unwrap();
    let result = partial_evaluate(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}