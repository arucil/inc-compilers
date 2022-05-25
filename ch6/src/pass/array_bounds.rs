use ast::{Error, Exp, ExpKind, Program, Type};
use id_arena::Arena;
use support::Range;

pub fn insert_bounds_check(
  prog: Program<String, Type>,
) -> Program<String, Type> {
  let types = &prog.types;
  Program {
    body: prog
      .body
      .into_iter()
      .map(|exp| exp_insert(exp, types))
      .collect(),
    ..prog
  }
}

fn exp_insert(
  exp: Exp<String, Type>,
  types: &Arena<Type>,
) -> Exp<String, Type> {
  match exp.kind {
    ExpKind::Int(_)
    | ExpKind::Var(_)
    | ExpKind::Str(_)
    | ExpKind::Bool(_)
    | ExpKind::Get(_)
    | ExpKind::Void
    | ExpKind::NewLine => exp,
    ExpKind::Prim {
      op: op @ (_, "make-vector"),
      mut args,
    } => {
      let range = exp.range;
      let ty = exp.ty;
      let val = exp_insert(args.pop().unwrap(), types);
      let len = exp_insert(args.pop().unwrap(), types);
      make_check_exp(
        range,
        ty.clone(),
        len,
        |len_var| Exp {
          kind: ExpKind::Prim {
            op: (range, ">="),
            args: vec![
              len_var,
              Exp {
                kind: ExpKind::Int(0),
                range,
                ty: Type::Int,
              },
            ],
          },
          range,
          ty: Type::Bool,
        },
        |len_var| Exp {
          kind: ExpKind::Prim {
            op,
            args: vec![len_var, val],
          },
          range,
          ty,
        },
        |len_var| Error::Length(box len_var),
      )
    }
    ExpKind::Prim {
      op: op @ (_, "vector-ref"),
      mut args,
    } if matches!(args[0].ty.resolved(types), Type::Array(_)) => {
      let range = exp.range;
      let ty = exp.ty;
      let idx = exp_insert(args.pop().unwrap(), types);
      let vec = exp_insert(args.pop().unwrap(), types);
      let vec_range = vec.range;
      let tmp = "(bounds-tmp-vec)".to_owned();
      let vec_var = Exp {
        kind: ExpKind::Var(tmp.clone()),
        range: vec_range,
        ty: vec.ty.clone(),
      };
      let check_exp = make_check_exp(
        range,
        ty.clone(),
        idx,
        |idx_var| Exp {
          kind: ExpKind::Prim {
            op: (range, "and"),
            args: vec![
              Exp {
                kind: ExpKind::Prim {
                  op: (range, ">="),
                  args: vec![
                    idx_var.clone(),
                    Exp {
                      kind: ExpKind::Int(0),
                      range,
                      ty: Type::Int,
                    },
                  ],
                },
                range,
                ty: Type::Bool,
              },
              Exp {
                kind: ExpKind::Prim {
                  op: (range, "<"),
                  args: vec![
                    idx_var,
                    Exp {
                      kind: ExpKind::Prim {
                        op: (range, "vector-length"),
                        args: vec![vec_var.clone()],
                      },
                      range,
                      ty: Type::Int,
                    },
                  ],
                },
                range,
                ty: Type::Bool,
              },
            ],
          },
          range,
          ty: Type::Bool,
        },
        |idx_var| Exp {
          kind: ExpKind::Prim {
            op,
            args: vec![vec_var.clone(), idx_var],
          },
          range,
          ty: ty.clone(),
        },
        |idx_var| Error::OutOfBounds {
          index: box idx_var,
          len: box Exp {
            kind: ExpKind::Prim {
              op: (range, "vector-length"),
              args: vec![vec_var.clone()],
            },
            range,
            ty: Type::Int,
          },
        },
      );
      Exp {
        kind: ExpKind::Let {
          var: (vec_range, tmp),
          init: box vec,
          body: box check_exp,
        },
        range,
        ty,
      }
    }
    ExpKind::Prim {
      op: op @ (_, "vector-set!"),
      mut args,
    } if matches!(args[0].ty.resolved(types), Type::Array(_)) => {
      let range = exp.range;
      let ty = exp.ty;
      let val = exp_insert(args.pop().unwrap(), types);
      let idx = exp_insert(args.pop().unwrap(), types);
      let vec = exp_insert(args.pop().unwrap(), types);
      let vec_range = vec.range;
      let tmp = "(bounds-tmp-vec)".to_owned();
      let vec_var = Exp {
        kind: ExpKind::Var(tmp.clone()),
        range: vec_range,
        ty: vec.ty.clone(),
      };
      let check_exp = make_check_exp(
        range,
        ty.clone(),
        idx,
        |idx_var| Exp {
          kind: ExpKind::Prim {
            op: (range, "and"),
            args: vec![
              Exp {
                kind: ExpKind::Prim {
                  op: (range, ">="),
                  args: vec![
                    idx_var.clone(),
                    Exp {
                      kind: ExpKind::Int(0),
                      range,
                      ty: Type::Int,
                    },
                  ],
                },
                range,
                ty: Type::Bool,
              },
              Exp {
                kind: ExpKind::Prim {
                  op: (range, "<"),
                  args: vec![
                    idx_var,
                    Exp {
                      kind: ExpKind::Prim {
                        op: (range, "vector-length"),
                        args: vec![vec_var.clone()],
                      },
                      range,
                      ty: Type::Int,
                    },
                  ],
                },
                range,
                ty: Type::Bool,
              },
            ],
          },
          range,
          ty: Type::Bool,
        },
        |idx_var| Exp {
          kind: ExpKind::Prim {
            op,
            args: vec![vec_var.clone(), idx_var, val],
          },
          range,
          ty: ty.clone(),
        },
        |idx_var| Error::OutOfBounds {
          index: box idx_var,
          len: box Exp {
            kind: ExpKind::Prim {
              op: (range, "vector-length"),
              args: vec![vec_var.clone()],
            },
            range,
            ty: Type::Int,
          },
        },
      );
      Exp {
        kind: ExpKind::Let {
          var: (vec_range, tmp),
          init: box vec,
          body: box check_exp,
        },
        range,
        ty,
      }
    }
    ExpKind::Prim { op, args } => Exp {
      kind: ExpKind::Prim {
        op,
        args: args.into_iter().map(|exp| exp_insert(exp, types)).collect(),
      },
      ..exp
    },
    ExpKind::Call { name, args } => Exp {
      kind: ExpKind::Call {
        name,
        args: args.into_iter().map(|exp| exp_insert(exp, types)).collect(),
      },
      ..exp
    },
    ExpKind::Let { var, init, body } => Exp {
      kind: ExpKind::Let {
        var,
        init: box exp_insert(*init, types),
        body: box exp_insert(*body, types),
      },
      ..exp
    },
    ExpKind::If { cond, conseq, alt } => Exp {
      kind: ExpKind::If {
        cond: box exp_insert(*cond, types),
        conseq: box exp_insert(*conseq, types),
        alt: box exp_insert(*alt, types),
      },
      ..exp
    },
    ExpKind::Set { var, exp: exp1 } => Exp {
      kind: ExpKind::Set {
        var,
        exp: box exp_insert(*exp1, types),
      },
      ..exp
    },
    ExpKind::Begin { seq, last } => Exp {
      kind: ExpKind::Begin {
        seq: seq.into_iter().map(|exp| exp_insert(exp, types)).collect(),
        last: box exp_insert(*last, types),
      },
      ..exp
    },
    ExpKind::While { cond, body } => Exp {
      kind: ExpKind::While {
        cond: box exp_insert(*cond, types),
        body: box exp_insert(*body, types),
      },
      ..exp
    },
    ExpKind::Print(args) => Exp {
      kind: ExpKind::Print(
        args.into_iter().map(|exp| exp_insert(exp, types)).collect(),
      ),
      ..exp
    },
    ExpKind::Error(_) => unreachable!()
  }
}

fn make_check_exp<F, G, E>(
  range: Range,
  ty: Type,
  index: Exp<String, Type>,
  cond: F,
  conseq: G,
  err: E,
) -> Exp<String, Type>
where
  F: FnOnce(Exp<String, Type>) -> Exp<String, Type>,
  G: FnOnce(Exp<String, Type>) -> Exp<String, Type>,
  E: FnOnce(Exp<String, Type>) -> Error<String, Type>,
{
  let tmp = "(bounds-tmp-index)".to_owned();
  let idx_var = Exp {
    kind: ExpKind::Var(tmp.clone()),
    range: index.range,
    ty: Type::Int,
  };
  Exp {
    kind: ExpKind::Let {
      var: (index.range, tmp),
      init: box index,
      body: box Exp {
        kind: ExpKind::If {
          cond: box cond(idx_var.clone()),
          conseq: box conseq(idx_var.clone()),
          alt: box Exp {
            kind: ExpKind::Error(err(idx_var)),
            range,
            ty: ty.clone(),
          },
        },
        range,
        ty: ty.clone(),
      },
    },
    range,
    ty,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use ch5::pass::typecheck::typecheck;
  use insta::assert_snapshot;

  #[test]
  fn array() {
    let prog = parse(
      r#"
(let ([x (make-vector 3 #t)]
      [i 1]
      [y (vector-ref x i)]
      [z (vector 1 #t "abc")])
  (vector-set! x i (vector-ref z 1))
  (vector-set! z 1 (not y)))
      "#,
    )
    .unwrap();
    let prog = typecheck(prog).unwrap();
    let result = insert_bounds_check(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
