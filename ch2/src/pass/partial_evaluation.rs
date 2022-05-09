use ast::{Exp, ExpKind, Program};
use support::Range;

pub fn partial_evaluate(prog: Program) -> Program {
  Program {
    body: prog.body.into_iter().map(pe_exp).collect(),
    types: prog.types,
  }
}

fn pe_exp(exp: Exp) -> Exp {
  match exp.kind {
    ExpKind::Int(_) | ExpKind::Str(_) | ExpKind::Var(_) => exp,
    ExpKind::Let {
      var,
      box init,
      box body,
    } => Exp {
      kind: ExpKind::Let {
        var,
        init: box pe_exp(init),
        body: box pe_exp(body),
      },
      range: exp.range,
      ty: exp.ty,
    },
    ExpKind::Prim { op, args } => pe_prim(op, args, exp.range),
    e => unimplemented!("{:?}", e),
  }
}

fn pe_prim(op: (Range, &'static str), args: Vec<Exp>, range: Range) -> Exp {
  let mut args = args.into_iter().map(pe_exp).collect::<Vec<_>>();
  match (op.1, &mut args[..]) {
    (
      "+",
      [Exp {
        kind: ExpKind::Int(a),
        ..
      }, Exp {
        kind: ExpKind::Int(b),
        ..
      }],
    ) => Exp {
      kind: ExpKind::Int(*a + *b),
      range,
      ty: (),
    },
    (
      "-",
      [Exp {
        kind: ExpKind::Int(a),
        ..
      }, Exp {
        kind: ExpKind::Int(b),
        ..
      }],
    ) => Exp {
      kind: ExpKind::Int(*a - *b),
      range,
      ty: (),
    },
    (
      "+",
      [Exp {
        kind: ExpKind::Int(a),
        range: range1,
        ..
      }, rhs]
      | [rhs, Exp {
        kind: ExpKind::Int(a),
        range: range1,
        ..
      }],
    ) => pe_add(op.0, (*range1, *a), rhs.clone()),
    (
      "-",
      [Exp {
        kind: ExpKind::Int(a),
        range: range1,
        ..
      }, rhs],
    ) => pe_sub(op.0, (*range1, *a), rhs.clone()),
    (
      "-",
      [Exp {
        kind: ExpKind::Int(a),
        ..
      }],
    ) => Exp {
      kind: ExpKind::Int(-*a),
      range,
      ty: (),
    },
    _ => Exp {
      kind: ExpKind::Prim { op, args },
      range,
      ty: (),
    },
  }
}

fn pe_add(range: Range, lhs: (Range, i64), mut rhs: Exp) -> Exp {
  match &mut rhs.kind {
    ExpKind::Prim {
      op: (_, op @ ("+" | "-")),
      args: subargs,
    } => match subargs[0].kind {
      ExpKind::Int(b) => Exp {
        kind: ExpKind::Prim {
          op: (range, op),
          args: vec![
            Exp {
              kind: ExpKind::Int(lhs.1 + b),
              range: lhs.0,
              ty: (),
            },
            subargs.pop().unwrap(),
          ],
        },
        range,
        ty: (),
      },
      _ => Exp {
        kind: ExpKind::Prim {
          op: (range, "+"),
          args: vec![
            Exp {
              kind: ExpKind::Int(lhs.1),
              range: lhs.0,
              ty: (),
            },
            rhs,
          ],
        },
        range,
        ty: (),
      },
    },
    _ => Exp {
      kind: ExpKind::Prim {
        op: (range, "+"),
        args: vec![
          Exp {
            kind: ExpKind::Int(lhs.1),
            range: lhs.0,
            ty: (),
          },
          rhs,
        ],
      },
      range,
      ty: (),
    },
  }
}

fn pe_sub(range: Range, lhs: (Range, i64), mut rhs: Exp) -> Exp {
  match &mut rhs.kind {
    ExpKind::Prim {
      op: (_, op @ ("+" | "-")),
      args: subargs,
    } => match subargs[0].kind {
      ExpKind::Int(b) => Exp {
        kind: ExpKind::Prim {
          op: (range, op),
          args: vec![
            Exp {
              kind: ExpKind::Int(lhs.1 - b),
              range,
              ty: (),
            },
            subargs.pop().unwrap(),
          ],
        },
        range,
        ty: (),
      },
      _ => Exp {
        kind: ExpKind::Prim {
          op: (range, "-"),
          args: vec![
            Exp {
              kind: ExpKind::Int(lhs.1),
              range,
              ty: (),
            },
            rhs,
          ],
        },
        range,
        ty: (),
      },
    },
    _ => Exp {
      kind: ExpKind::Prim {
        op: (range, "-"),
        args: vec![
          Exp {
            kind: ExpKind::Int(lhs.1),
            range: lhs.0,
            ty: (),
          },
          rhs,
        ],
      },
      range,
      ty: (),
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
