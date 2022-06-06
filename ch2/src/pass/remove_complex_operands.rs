use ast::{Error, Exp, ExpKind, FunDef, IdxVar, Program};
use support::Range;

pub fn remove_complex_operands<TYPE: Clone>(
  prog: Program<IdxVar, TYPE>,
) -> Program<IdxVar, TYPE> {
  let mut state = State { counter: 0 };
  Program {
    fun_defs: prog
      .fun_defs
      .into_iter()
      .map(|(name, fun)| {
        (
          name,
          FunDef {
            body: state.mon_exp(fun.body),
            ..fun
          },
        )
      })
      .collect(),
    body: state.mon_exp(prog.body),
    ..prog
  }
}

struct State {
  counter: usize,
}

struct TmpVar<TYPE> {
  range: Range,
  name: IdxVar,
  /// May be not atomic.
  init: Exp<IdxVar, TYPE>,
}

impl State {
  /// process expressions that do not need to be atomic.
  ///
  /// `mon` stands for Monadic Normal Form.
  fn mon_exp<TYPE: Clone>(
    &mut self,
    exp: Exp<IdxVar, TYPE>,
  ) -> Exp<IdxVar, TYPE> {
    let range = exp.range;
    let ty = exp.ty.clone();
    match exp.kind {
      ExpKind::Int(..) => exp,
      ExpKind::Var(..) => exp,
      ExpKind::Let { var, init, body } => Exp {
        kind: ExpKind::Let {
          var,
          init: box self.mon_exp(*init),
          body: box self.mon_exp(*body),
        },
        range,
        ty,
      },
      ExpKind::Prim { op, args } => {
        let mut tmps = vec![];
        // TODO rearrange args to make atom exps come after complex ones, but make
        // sure their results are in original order.
        let args = args
          .into_iter()
          .map(|arg| self.atom_exp(arg, &mut tmps))
          .collect();
        make_tmps_let(
          Exp {
            kind: ExpKind::Prim { op, args },
            range,
            ty,
          },
          tmps,
        )
      }
      ExpKind::Apply {
        fun,
        args,
        r#struct,
      } => {
        let mut tmps = vec![];
        // TODO rearrange args to make atom exps come after complex ones, but make
        // sure their results are in original order.
        let fun = if let ExpKind::FunRef { .. } = fun.kind {
          *fun
        } else {
          self.atom_exp(*fun, &mut tmps)
        };
        let args = args
          .into_iter()
          .map(|arg| self.atom_exp(arg, &mut tmps))
          .collect();
        make_tmps_let(
          Exp {
            kind: ExpKind::Apply {
              fun: box fun,
              args,
              r#struct,
            },
            range,
            ty,
          },
          tmps,
        )
      }
      // ch4
      ExpKind::If { cond, conseq, alt } => {
        if let ExpKind::Prim {
          op: (_, "vector-ref"),
          ..
        }
        | ExpKind::Apply { .. } = cond.kind
        {
          let mut tmps = vec![];
          let cond = self.atom_exp(*cond, &mut tmps);
          let conseq = box self.mon_exp(*conseq);
          let alt = box self.mon_exp(*alt);
          make_tmps_let(
            Exp {
              kind: ExpKind::If {
                cond: box cond,
                conseq,
                alt,
              },
              range,
              ty,
            },
            tmps,
          )
        } else {
          let cond = box self.mon_exp(*cond);
          let conseq = box self.mon_exp(*conseq);
          let alt = box self.mon_exp(*alt);
          Exp {
            kind: ExpKind::If { cond, conseq, alt },
            range,
            ty,
          }
        }
      }
      ExpKind::Bool(..) => exp,
      // ch5
      ExpKind::Get(..) => exp,
      ExpKind::Set { var, exp } => Exp {
        kind: ExpKind::Set {
          var,
          exp: box self.mon_exp(*exp),
        },
        range,
        ty,
      },
      ExpKind::Begin { seq, last } => Exp {
        kind: ExpKind::Begin {
          seq: seq.into_iter().map(|exp| self.mon_exp(exp)).collect(),
          last: box self.mon_exp(*last),
        },
        range,
        ty,
      },
      ExpKind::While { cond, body } => Exp {
        kind: ExpKind::While {
          cond: box self.mon_exp(*cond),
          body: box self.mon_exp(*body),
        },
        range,
        ty,
      },
      ExpKind::Void => exp,
      ExpKind::Print(args) => {
        let mut tmps = vec![];
        // TODO rearrange args to make atom exps come after complex ones, but make
        // sure their results are in original order.
        let args = args
          .into_iter()
          .map(|arg| self.atom_exp(arg, &mut tmps))
          .collect();
        make_tmps_let(
          Exp {
            kind: ExpKind::Print(args),
            range,
            ty,
          },
          tmps,
        )
      }
      ExpKind::Str(..) => exp,
      ExpKind::NewLine => exp,
      ExpKind::Error(Error::Length(len)) => {
        let mut tmps = vec![];
        let len = self.atom_exp(*len, &mut tmps);
        make_tmps_let(
          Exp {
            kind: ExpKind::Error(Error::Length(box len)),
            range,
            ty,
          },
          tmps,
        )
      }
      ExpKind::Error(Error::OutOfBounds { index, len }) => {
        let mut tmps = vec![];
        let len = self.atom_exp(*len, &mut tmps);
        let index = self.atom_exp(*index, &mut tmps);
        make_tmps_let(
          Exp {
            kind: ExpKind::Error(Error::OutOfBounds {
              len: box len,
              index: box index,
            }),
            range,
            ty,
          },
          tmps,
        )
      }
      ExpKind::Error(Error::DivByZero) => exp,
      // ch7
      ExpKind::FunRef { .. } => exp,
    }
  }

  /// Process expressions that need to be atomic.
  fn atom_exp<TYPE: Clone>(
    &mut self,
    exp: Exp<IdxVar, TYPE>,
    tmps: &mut Vec<TmpVar<TYPE>>,
  ) -> Exp<IdxVar, TYPE> {
    match exp.kind {
      ExpKind::Let { var, init, body } => {
        tmps.push(TmpVar {
          range: var.0,
          name: var.1,
          init: self.mon_exp(*init),
        });
        self.atom_exp(*body, tmps)
      }
      ExpKind::Prim { op, args } => {
        let args = args
          .into_iter()
          .map(|arg| self.atom_exp(arg, tmps))
          .collect();
        let exp = Exp {
          kind: ExpKind::Prim { op, args },
          range: exp.range,
          ty: exp.ty,
        };
        self.assign_var(exp, tmps)
      }
      ExpKind::Apply {
        fun,
        args,
        r#struct,
      } => {
        let fun = if let ExpKind::FunRef { .. } = fun.kind {
          *fun
        } else {
          self.atom_exp(*fun, tmps)
        };
        let args = args
          .into_iter()
          .map(|arg| self.atom_exp(arg, tmps))
          .collect();
        let exp = Exp {
          kind: ExpKind::Apply {
            fun: box fun,
            args,
            r#struct,
          },
          range: exp.range,
          ty: exp.ty,
        };
        self.assign_var(exp, tmps)
      }
      ExpKind::Bool(..) | ExpKind::Int(..) | ExpKind::Var(..) => exp,
      // ch4
      ExpKind::If { .. } => {
        let exp = self.mon_exp(exp);
        self.assign_var(exp, tmps)
      }
      // ch5
      ExpKind::Get(..)
      | ExpKind::Begin { .. }
      | ExpKind::While { .. }
      | ExpKind::Set { .. }
      | ExpKind::Print { .. }
      | ExpKind::NewLine
      | ExpKind::Str(..) => {
        let exp = self.mon_exp(exp);
        self.assign_var(exp, tmps)
      }
      ExpKind::Void => exp,
      ExpKind::Error(_) => {
        let exp = self.mon_exp(exp);
        self.assign_var(exp, tmps)
      }
      // ch7
      ExpKind::FunRef { .. } => {
        let exp = self.mon_exp(exp);
        self.assign_var(exp, tmps)
      }
    }
  }

  fn assign_var<TYPE: Clone>(
    &mut self,
    exp: Exp<IdxVar, TYPE>,
    tmps: &mut Vec<TmpVar<TYPE>>,
  ) -> Exp<IdxVar, TYPE> {
    let range = exp.range;
    let ty = exp.ty.clone();
    let tmp = IdxVar {
      name: "(tmp)".to_owned(),
      index: self.counter,
    };
    self.counter += 1;
    tmps.push(TmpVar {
      range,
      name: tmp.clone(),
      init: exp,
    });
    Exp {
      kind: ExpKind::Var(tmp),
      range,
      ty,
    }
  }
}

fn make_tmps_let<TYPE: Clone>(
  body: Exp<IdxVar, TYPE>,
  tmps: Vec<TmpVar<TYPE>>,
) -> Exp<IdxVar, TYPE> {
  tmps.into_iter().rfold(body, |body, tmp| Exp {
    range: body.range,
    ty: body.ty.clone(),
    kind: ExpKind::Let {
      var: (tmp.range, tmp.name),
      init: box tmp.init,
      body: box body,
    },
  })
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (- y (- 2))))"#)
        .unwrap();
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn init_with_var() {
    let prog = parse(r#"(let ([a 42]) (let ([b a]) b))"#).unwrap();
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_in_init() {
    let prog = parse(
      r#"
      (let
        ([a
          (+ (let
               ([x (let
                     ([x (read)])
                     (+ x 13))]
                [y (- x)])
               (+ x (- y)))
             7)])
        (- a))
      "#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog);
    let result = remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
