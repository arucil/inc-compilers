use ast::{Exp, IdxVar, Program};
use support::Range;

pub fn remove_complex_operands(prog: Program<IdxVar>) -> Program<IdxVar> {
  let mut counter = 0;
  Program {
    body: prog
      .body
      .into_iter()
      .map(|(range, exp)| (range, mon_exp(range, exp, &mut counter)))
      .collect(),
  }
}

struct TmpVar {
  range: Range,
  name: IdxVar,
  /// May be not atomic.
  init: Exp<IdxVar>,
}

/// process expressions that do not need to be atomic.
///
/// `mon` stands for Monadic Normal Form.
fn mon_exp(range: Range, exp: Exp<IdxVar>, counter: &mut usize) -> Exp<IdxVar> {
  match exp {
    Exp::Int(..) => exp,
    Exp::Var(..) => exp,
    Exp::Let { var, init, body } => Exp::Let {
      var,
      init: box (init.0, mon_exp(init.0, init.1, counter)),
      body: box (body.0, mon_exp(body.0, body.1, counter)),
    },
    Exp::Prim { op, args } => {
      let mut tmps = vec![];
      let args = args
        .into_iter()
        .map(|(range, arg)| (range, atom_exp(range, arg, &mut tmps, counter)))
        .collect();
      tmps
        .into_iter()
        .rfold(
          (range, Exp::Prim { op, args }),
          |(body_range, body), tmp| {
            (
              body_range,
              Exp::Let {
                var: (tmp.range, tmp.name),
                init: box (range, tmp.init),
                body: box (body_range, body),
              },
            )
          },
        )
        .1
    }
    // ch4
    Exp::If { cond, conseq, alt } => Exp::If {
      cond: box (cond.0, mon_exp(cond.0, cond.1, counter)),
      conseq: box (conseq.0, mon_exp(conseq.0, conseq.1, counter)),
      alt: box (alt.0, mon_exp(alt.0, alt.1, counter)),
    },
    Exp::Bool(..) => exp,
    // ch5
    Exp::Set { var, exp } => Exp::Set {
      var,
      exp: box (exp.0, mon_exp(exp.0, exp.1, counter)),
    },
    Exp::Begin { seq, last } => Exp::Begin {
      seq: seq
        .into_iter()
        .map(|exp| (exp.0, mon_exp(exp.0, exp.1, counter)))
        .collect(),
      last: box (last.0, mon_exp(last.0, last.1, counter)),
    },
    Exp::While { cond, body } => Exp::While {
      cond: box (cond.0, mon_exp(cond.0, cond.1, counter)),
      body: box (body.0, mon_exp(body.0, body.1, counter)),
    },
    Exp::Void => exp,
    Exp::Print { val, ty } => Exp::Print {
      val: box (val.0, mon_exp(val.0, val.1, counter)),
      ty,
    },
    Exp::Str(..) => exp,
    Exp::NewLine => exp,
  }
}

/// Process expressions that need to be atomic.
fn atom_exp(
  range: Range,
  exp: Exp<IdxVar>,
  tmps: &mut Vec<TmpVar>,
  counter: &mut usize,
) -> Exp<IdxVar> {
  match exp {
    Exp::Let { var, init, body } => {
      tmps.push(TmpVar {
        range: var.0,
        name: var.1,
        init: mon_exp(init.0, init.1, counter),
      });
      atom_exp(body.0, body.1, tmps, counter)
    }
    Exp::Prim { op, args } => {
      let args = args
        .into_iter()
        .map(|(range, arg)| (range, atom_exp(range, arg, tmps, counter)))
        .collect();
      let exp = Exp::Prim { op, args };
      assign_var(range, exp, tmps, counter)
    }
    Exp::Bool(..) | Exp::Int(..) | Exp::Var(..) => exp,
    // ch4
    Exp::If { .. } | Exp::Begin { .. } => {
      let exp = mon_exp(range, exp, counter);
      assign_var(range, exp, tmps, counter)
    }
    Exp::Str(..) => exp,
    _ => unimplemented!("{:?}", exp),
  }
}

fn assign_var(
  range: Range,
  exp: Exp<IdxVar>,
  tmps: &mut Vec<TmpVar>,
  counter: &mut usize,
) -> Exp<IdxVar> {
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
  Exp::Var(tmp)
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
