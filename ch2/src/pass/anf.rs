use ast::{Program, Exp, IdxVar};
use support::Range;

pub fn anf(prog: Program<IdxVar>) -> Program<IdxVar> {
  let mut counter = 0;
  Program {
    body: prog.body.into_iter()
      .map(|(range, exp)| (range, anf_exp(range, exp, &mut counter)))
      .collect()
  }
}

fn anf_exp(
  range: Range,
  exp: Exp<IdxVar>,
  counter: &mut usize,
) -> Exp<IdxVar> {
  match exp {
    Exp::Let { var, init, body } => {
      Exp::Let {
        var,
        init: box (init.0, anf_exp(init.0, init.1, counter)),
        body: box (body.0, anf_exp(body.0, body.1, counter)),
      }
    }
    Exp::Prim { op, args } => {
      let mut tmps = vec![];
      let args = args.into_iter()
        .map(|(range, arg)| (range, atom_exp(range, arg, &mut tmps, counter)))
        .collect();
      tmps.into_iter().rfold(
        (range, Exp::Prim { op, args }),
        |(body_range, body), (range, var, init)|
          (body_range, Exp::Let {
            var: (range, var),
            init: box (range, init),
            body: box (body_range, body)
          })).1
    }
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
    exp@Exp::Let { .. } => {
      let exp = anf_exp(range, exp, counter);
      let tmp = IdxVar {
        name: "tmp".to_owned(),
        index: *counter,
      };
      *counter += 1;
      tmps.push((range, tmp.clone(), exp));
      Exp::Var(tmp)
    }
    Exp::Prim { op, args } => {
      let args = args.into_iter()
        .map(|(range, arg)| (range, atom_exp(range, arg, tmps, counter)))
        .collect();
      let exp = Exp::Prim { op, args };
      let tmp = IdxVar {
        name: "tmp".to_owned(),
        index: *counter,
      };
      *counter += 1;
      tmps.push((range, tmp.clone(), exp));
      Exp::Var(tmp)
    }
    exp => exp
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog = parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#).unwrap();
    let prog = super::super::uniquify::uniquify(prog).unwrap();
    let result = anf(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn init_with_var() {
    let prog = parse(r#"(let ([a 42]) (let ([b a]) b))"#).unwrap();
    let prog = super::super::uniquify::uniquify(prog).unwrap();
    let result = anf(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn let_in_init() {
    let prog = parse(r#"(let ([a (+ (let ([x (read)] [y (- x)]) (+ x (- y))) 7)]) (- a))"#).unwrap();
    let prog = super::super::uniquify::uniquify(prog).unwrap();
    let result = anf(prog);
    assert_snapshot!(result.to_string_pretty());
  }

}