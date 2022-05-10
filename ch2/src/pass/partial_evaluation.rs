use ast::{Exp, Program};
use support::Range;

pub fn partial_evaluate(prog: Program) -> Program {
  Program {
    body: prog.body.into_iter().map(pe_exp).collect(),
    types: prog.types,
  }
}

fn pe_exp(exp: Exp) -> Exp {
  match exp {
    Exp::Int { .. } | Exp::Str { .. } | Exp::Var { .. } => exp,
    Exp::Let {
      var,
      init,
      body,
      range,
    } => Exp::Let {
      var,
      init: box pe_exp(*init),
      body: box pe_exp(*body),
      range,
    },
    Exp::Prim { op, args, range } => pe_prim(op, args, range),
    e => unimplemented!("{:?}", e),
  }
}

fn pe_prim(op: (Range, &'static str), args: Vec<Exp>, range: Range) -> Exp {
  let mut args = args.into_iter().map(pe_exp).collect::<Vec<_>>();
  match (op.1, &mut args[..]) {
    ("+", [Exp::Int { value: a, .. }, Exp::Int { value: b, .. }]) => Exp::Int {
      value: *a + *b,
      range,
    },
    ("-", [Exp::Int { value: a, .. }, Exp::Int { value: b, .. }]) => Exp::Int {
      value: *a - *b,
      range,
    },
    (
      "+",
      [Exp::Int {
        value: a,
        range: range1,
      }, rhs]
      | [rhs, Exp::Int {
        value: a,
        range: range1,
      }],
    ) => pe_add(op.0, (*range1, *a), rhs.clone()),
    (
      "-",
      [Exp::Int {
        value: a,
        range: range1,
        ..
      }, rhs],
    ) => pe_sub(op.0, (*range1, *a), rhs.clone()),
    ("-", [Exp::Int { value: a, .. }]) => Exp::Int { value: -*a, range },
    _ => Exp::Prim { op, args, range },
  }
}

fn pe_add(range: Range, lhs: (Range, i64), mut rhs: Exp) -> Exp {
  match &mut rhs {
    Exp::Prim {
      op: (_, op @ ("+" | "-")),
      args: subargs,
      ..
    } => match subargs[0] {
      Exp::Int { value: b, .. } => Exp::Prim {
        op: (range, op),
        args: vec![
          Exp::Int {
            value: lhs.1 + b,
            range: lhs.0,
          },
          subargs.pop().unwrap(),
        ],
        range,
      },
      _ => Exp::Prim {
        op: (range, "+"),
        args: vec![
          Exp::Int {
            value: lhs.1,
            range: lhs.0,
          },
          rhs,
        ],
        range,
      },
    },
    _ => Exp::Prim {
      op: (range, "+"),
      args: vec![
        Exp::Int {
          value: lhs.1,
          range: lhs.0,
        },
        rhs,
      ],
      range,
    },
  }
}

fn pe_sub(range: Range, lhs: (Range, i64), mut rhs: Exp) -> Exp {
  match &mut rhs {
    Exp::Prim {
      op: (_, op @ ("+" | "-")),
      args: subargs,
      ..
    } => match subargs[0] {
      Exp::Int { value: b, .. } => Exp::Prim {
        op: (range, op),
        args: vec![
          Exp::Int {
            value: lhs.1 - b,
            range,
          },
          subargs.pop().unwrap(),
        ],
        range,
      },
      _ => Exp::Prim {
        op: (range, "-"),
        args: vec![
          Exp::Int {
            value: lhs.1,
            range,
          },
          rhs,
        ],
        range,
      },
    },
    _ => Exp::Prim {
      op: (range, "-"),
      args: vec![
        Exp::Int {
          value: lhs.1,
          range: lhs.0,
        },
        rhs,
      ],
      range,
    },
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
    let prog = parse(r#"(+ (- 7 (read)) 3)"#).unwrap();
    let result = partial_evaluate(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn fold_constant() {
    let prog = parse(r#"(+ (- (- (- 7) 2) (read)) 1)"#).unwrap();
    let result = partial_evaluate(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn cannot_fold_sub_rhs() {
    let prog = parse(r#"(+ 1 (- (read) (- (- 7) 2)))"#).unwrap();
    let result = partial_evaluate(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
