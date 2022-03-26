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
    exp => exp,
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
    exp => exp,
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
