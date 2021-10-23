use ast::{Exp, IdxVar, Program};
use support::Range;

pub fn anf(prog: Program<IdxVar>) -> Program<IdxVar> {
  let mut counter = 0;
  Program {
    body: prog
      .body
      .into_iter()
      .map(|(range, exp)| (range, anf_exp(range, exp, &mut counter)))
      .collect(),
  }
}

fn anf_exp(range: Range, exp: Exp<IdxVar>, counter: &mut usize) -> Exp<IdxVar> {
  match exp {
    Exp::Let { var, init, body } => Exp::Let {
      var,
      init: box (init.0, anf_exp(init.0, init.1, counter)),
      body: box (body.0, anf_exp(body.0, body.1, counter)),
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
          |(body_range, body), (range, var, init)| {
            (
              body_range,
              Exp::Let {
                var: (range, var),
                init: box (range, init),
                body: box (body_range, body),
              },
            )
          },
        )
        .1
    }
    Exp::If { cond, conseq, alt } => Exp::If {
      cond: box (cond.0, anf_exp(cond.0, cond.1, counter)),
      conseq: box (conseq.0, anf_exp(conseq.0, conseq.1, counter)),
      alt: box (alt.0, anf_exp(alt.0, alt.1, counter)),
    },
    Exp::Set { var, exp } => Exp::Set {
      var,
      exp: box (exp.0, anf_exp(exp.0, exp.1, counter)),
    },
    Exp::Begin { seq, last } => Exp::Begin {
      seq: seq
        .into_iter()
        .map(|exp| (exp.0, anf_exp(exp.0, exp.1, counter)))
        .collect(),
      last: box (last.0, anf_exp(last.0, last.1, counter)),
    },
    Exp::While { cond, body } => Exp::While {
      cond: box (cond.0, anf_exp(cond.0, cond.1, counter)),
      body: box (body.0, anf_exp(body.0, body.1, counter)),
    },
    Exp::Print { val, ty } => Exp::Print {
      val: box (val.0, anf_exp(val.0, val.1, counter)),
      ty,
    },
    exp => exp,
  }
}

fn atom_exp(
  range: Range,
  exp: Exp<IdxVar>,
  tmps: &mut Vec<(Range, IdxVar, Exp<IdxVar>)>,
  counter: &mut usize,
) -> Exp<IdxVar> {
  match exp {
    Exp::Prim { op, args } => {
      let args = args
        .into_iter()
        .map(|(range, arg)| (range, atom_exp(range, arg, tmps, counter)))
        .collect();
      let exp = Exp::Prim { op, args };
      assign_var(range, exp, tmps, counter)
    }
    Exp::Let { .. }
    | Exp::If { .. }
    | Exp::Set { .. }
    | Exp::Begin { .. }
    | Exp::While { .. } => {
      let exp = anf_exp(range, exp, counter);
      assign_var(range, exp, tmps, counter)
    }
    exp => exp,
  }
}

fn assign_var(
  range: Range,
  exp: Exp<IdxVar>,
  tmps: &mut Vec<(Range, IdxVar, Exp<IdxVar>)>,
  counter: &mut usize,
) -> Exp<IdxVar> {
  let tmp = IdxVar {
    name: "tmp".to_owned(),
    index: *counter,
  };
  *counter += 1;
  tmps.push((range, tmp.clone(), exp));
  Exp::Var(tmp)
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let result = anf(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn init_with_var() {
    let prog = parse(r#"(let ([a 42]) (let ([b a]) b))"#).unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let result = anf(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_in_init() {
    let prog = parse(
      r#"(let ([a (+ (let ([x (read)] [y (- x)]) (+ x (- y))) 7)]) (- a))"#,
    )
    .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let result = anf(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn if_form() {
    let prog = parse(
      r#"(+ (- 3 2) (if (not (eq? 2 3)) (- (read)) (> (read) (not #t))))"#,
    )
    .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let result = anf(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn begin() {
    let prog = parse(
      r#"
(let ([x0 10])
  (let ([y1 0])
    (+ (+ (begin (set! y1 (read)) x0)
       (begin (set! x0 (read)) y1))
    x0)))"#,
    )
    .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let result = anf(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
