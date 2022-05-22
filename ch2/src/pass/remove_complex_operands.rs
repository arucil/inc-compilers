use ast::{Exp, ExpKind, IdxVar, Program};
use support::Range;

pub fn remove_complex_operands<TYPE: Clone>(
  prog: Program<IdxVar, TYPE>,
) -> Program<IdxVar, TYPE> {
  let mut counter = 0;
  Program {
    body: prog
      .body
      .into_iter()
      .map(|exp| mon_exp(exp, &mut counter))
      .collect(),
    ..prog
  }
}

struct TmpVar<TYPE> {
  range: Range,
  name: IdxVar,
  /// May be not atomic.
  init: Exp<IdxVar, TYPE>,
}

/// process expressions that do not need to be atomic.
///
/// `mon` stands for Monadic Normal Form.
fn mon_exp<TYPE: Clone>(
  exp: Exp<IdxVar, TYPE>,
  counter: &mut usize,
) -> Exp<IdxVar, TYPE> {
  let range = exp.range;
  let ty = exp.ty.clone();
  match exp.kind {
    ExpKind::Int(..) => exp,
    ExpKind::Var(..) => exp,
    ExpKind::Let { var, init, body } => Exp {
      kind: ExpKind::Let {
        var,
        init: box mon_exp(*init, counter),
        body: box mon_exp(*body, counter),
      },
      range,
      ty,
    },
    ExpKind::Prim { op, args } => mon_prim(
      args,
      |args| Exp {
        kind: ExpKind::Prim { op, args },
        range,
        ty,
      },
      counter,
    ),
    // ch4
    ExpKind::If { cond, conseq, alt } => Exp {
      kind: ExpKind::If {
        cond: box mon_exp(*cond, counter),
        conseq: box mon_exp(*conseq, counter),
        alt: box mon_exp(*alt, counter),
      },
      range,
      ty,
    },
    ExpKind::Bool(..) => exp,
    // ch5
    ExpKind::Get(..) => exp,
    ExpKind::Set { var, exp } => Exp {
      kind: ExpKind::Set {
        var,
        exp: box mon_exp(*exp, counter),
      },
      range,
      ty,
    },
    ExpKind::Begin { seq, last } => Exp {
      kind: ExpKind::Begin {
        seq: seq.into_iter().map(|exp| mon_exp(exp, counter)).collect(),
        last: box mon_exp(*last, counter),
      },
      range,
      ty,
    },
    ExpKind::While { cond, body } => Exp {
      kind: ExpKind::While {
        cond: box mon_exp(*cond, counter),
        body: box mon_exp(*body, counter),
      },
      range,
      ty,
    },
    ExpKind::Void => exp,
    ExpKind::Print(args) => mon_prim(
      args,
      |args| Exp {
        kind: ExpKind::Print(args),
        range,
        ty,
      },
      counter,
    ),
    ExpKind::Str(..) => exp,
    ExpKind::NewLine => exp,
  }
}

fn mon_prim<TYPE: Clone>(
  args: Vec<Exp<IdxVar, TYPE>>,
  build_exp: impl FnOnce(Vec<Exp<IdxVar, TYPE>>) -> Exp<IdxVar, TYPE>,
  counter: &mut usize,
) -> Exp<IdxVar, TYPE> {
  let mut tmps = vec![];
  // TODO rearrange args to make atom exps come after complex ones, but make
  // sure their results in original order.
  let args = args
    .into_iter()
    .map(|arg| atom_exp(arg, &mut tmps, counter))
    .collect();
  tmps.into_iter().rfold(build_exp(args), |body, tmp| Exp {
    range: body.range,
    ty: body.ty.clone(),
    kind: ExpKind::Let {
      var: (tmp.range, tmp.name),
      init: box tmp.init,
      body: box body,
    },
  })
}

/// Process expressions that need to be atomic.
fn atom_exp<TYPE: Clone>(
  exp: Exp<IdxVar, TYPE>,
  tmps: &mut Vec<TmpVar<TYPE>>,
  counter: &mut usize,
) -> Exp<IdxVar, TYPE> {
  match exp.kind {
    ExpKind::Let { var, init, body } => {
      tmps.push(TmpVar {
        range: var.0,
        name: var.1,
        init: mon_exp(*init, counter),
      });
      atom_exp(*body, tmps, counter)
    }
    ExpKind::Prim { op, args } => {
      let args = args
        .into_iter()
        .map(|arg| atom_exp(arg, tmps, counter))
        .collect();
      let exp = Exp {
        kind: ExpKind::Prim { op, args },
        range: exp.range,
        ty: exp.ty,
      };
      assign_var(exp, tmps, counter)
    }
    ExpKind::Bool(..) | ExpKind::Int(..) | ExpKind::Var(..) => exp,
    // ch4
    ExpKind::If { .. } => {
      let exp = mon_exp(exp, counter);
      assign_var(exp, tmps, counter)
    }
    // ch5
    ExpKind::Get(..)
    | ExpKind::Begin { .. }
    | ExpKind::While { .. }
    | ExpKind::Set { .. }
    | ExpKind::Print { .. }
    | ExpKind::Str(..) => {
      let exp = mon_exp(exp, counter);
      assign_var(exp, tmps, counter)
    }
    ExpKind::Void => exp,
    // ExpKind::Str(..) => exp,
    ExpKind::NewLine => exp,
  }
}

fn assign_var<TYPE: Clone>(
  exp: Exp<IdxVar, TYPE>,
  tmps: &mut Vec<TmpVar<TYPE>>,
  counter: &mut usize,
) -> Exp<IdxVar, TYPE> {
  let range = exp.range;
  let ty = exp.ty.clone();
  let tmp = IdxVar {
    name: "tmp".to_owned(),
    index: *counter,
  };
  *counter += 1;
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
    let prog = uniquify::uniquify(prog).unwrap();
    let result = remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn init_with_var() {
    let prog = parse(r#"(let ([a 42]) (let ([b a]) b))"#).unwrap();
    let prog = uniquify::uniquify(prog).unwrap();
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
    let prog = uniquify::uniquify(prog).unwrap();
    let result = remove_complex_operands(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
