use ast::{Error, Exp, ExpKind, FuncDef, IdxVar, Program, Type};
use id_arena::Arena;
use support::Range;

/// (make-vector e1 e2)
/// =>
/// (let ([len e1])
///   (if (>= len 0)
///     (make-vector len e2)
///     (length-error)))
///
/// (vector-ref e1 e2)
/// =>
/// (let ([vec e1] [idx e2])
///   (if (and (>= idx 0)
///            (< idx (vector-length vec)))
///     (vector-ref vec idx)
///     (out-of-bounds idx (vector-length vec))))
///
/// (vector-set! e1 e2 e3)
/// =>
/// (let ([vec e1] [idx e2])
///   (if (and (>= idx 0) (< idx (vector-length vec)))
///     (vector-set! vec idx e3)
///     (out-of-bounds idx (vector-length vec))))
pub fn insert_bounds_check(
  prog: Program<IdxVar, Type>,
) -> Program<IdxVar, Type> {
  let mut state = State {
    types: &prog.types,
    tmp_counter: 0,
  };
  Program {
    func_defs: prog
      .func_defs
      .into_iter()
      .map(|(name, func)| {
        (
          name,
          FuncDef {
            body: state.exp_insert(func.body),
            ..func
          },
        )
      })
      .collect(),
    body: prog
      .body
      .into_iter()
      .map(|exp| state.exp_insert(exp))
      .collect(),
    ..prog
  }
}

struct State<'a> {
  types: &'a Arena<Type>,
  tmp_counter: usize,
}

impl<'a> State<'a> {
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
        op: op @ (_, "make-vector"),
        mut args,
      } => {
        let range = exp.range;
        let ty = exp.ty;
        let val = self.exp_insert(args.pop().unwrap());
        let len = self.exp_insert(args.pop().unwrap());
        self.make_check_exp(
          range,
          ty.clone(),
          vec![],
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
      } if matches!(self.resolve_type(&args[0].ty), Type::Array(_)) => {
        let range = exp.range;
        let ty = exp.ty;
        let idx = self.exp_insert(args.pop().unwrap());
        let vec = self.exp_insert(args.pop().unwrap());
        let mut tmps = vec![];
        let vec_var;
        if vec.kind.is_atomic() {
          vec_var = vec;
        } else {
          let tmp = self.new_tmp();
          vec_var = Exp {
            kind: ExpKind::Var(tmp.clone()),
            range: vec.range,
            ty: vec.ty.clone(),
          };
          tmps.push(((vec.range, tmp), box vec));
        }
        self.make_check_exp(
          range,
          ty.clone(),
          tmps,
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
        )
      }
      ExpKind::Prim {
        op: op @ (_, "vector-set!"),
        mut args,
      } if matches!(self.resolve_type(&args[0].ty), Type::Array(_)) => {
        let range = exp.range;
        let ty = exp.ty;
        let val = self.exp_insert(args.pop().unwrap());
        let idx = self.exp_insert(args.pop().unwrap());
        let vec = self.exp_insert(args.pop().unwrap());
        let mut tmps = vec![];
        let vec_var;
        if vec.kind.is_atomic() {
          vec_var = vec;
        } else {
          let tmp = self.new_tmp();
          vec_var = Exp {
            kind: ExpKind::Var(tmp.clone()),
            range: vec.range,
            ty: vec.ty.clone(),
          };
          tmps.push(((vec.range, tmp), box vec));
        }
        self.make_check_exp(
          range,
          ty.clone(),
          tmps,
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
        )
      }
      ExpKind::Prim { op, args } => Exp {
        kind: ExpKind::Prim {
          op,
          args: args.into_iter().map(|exp| self.exp_insert(exp)).collect(),
        },
        ..exp
      },
      ExpKind::Apply {
        func,
        args,
        r#struct,
      } => Exp {
        kind: ExpKind::Apply {
          func: box self.exp_insert(*func),
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
      ExpKind::Error(_) => unreachable!(),
      ExpKind::FunRef { .. } => exp,
    }
  }

  fn make_check_exp<F, G, E>(
    &mut self,
    range: Range,
    ty: Type,
    mut tmps: Vec<Tmp>,
    index: Exp<IdxVar, Type>,
    cond: F,
    conseq: G,
    err: E,
  ) -> Exp<IdxVar, Type>
  where
    F: FnOnce(Exp<IdxVar, Type>) -> Exp<IdxVar, Type>,
    G: FnOnce(Exp<IdxVar, Type>) -> Exp<IdxVar, Type>,
    E: FnOnce(Exp<IdxVar, Type>) -> Error<IdxVar, Type>,
  {
    let idx_var;
    if index.kind.is_atomic() {
      idx_var = index;
    } else {
      let tmp = self.new_tmp();
      idx_var = Exp {
        kind: ExpKind::Var(tmp.clone()),
        range: index.range,
        ty: Type::Int,
      };
      tmps.push(((index.range, tmp), box index));
    }
    let body = Exp {
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
    };
    tmps.into_iter().rfold(body, |body, (var, init)| Exp {
      kind: ExpKind::Let {
        var,
        init,
        body: box body,
      },
      range,
      ty: ty.clone(),
    })
  }

  fn resolve_type(&self, ty: &Type) -> Type {
    ty.resolved(self.types)
  }

  fn new_tmp(&mut self) -> IdxVar {
    self.tmp_counter += 1;
    IdxVar {
      name: "(bounds-tmp)".to_owned(),
      index: self.tmp_counter,
    }
  }
}

type Tmp = ((Range, IdxVar), Box<Exp<IdxVar, Type>>);

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use ch4::pass::uniquify;
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
    let prog = uniquify::uniquify(prog);
    let result = insert_bounds_check(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
