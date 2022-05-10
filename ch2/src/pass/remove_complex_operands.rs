use ast::{Exp, IdxVar, Program};
use support::Range;

pub fn remove_complex_operands(prog: Program<IdxVar>) -> Program<IdxVar> {
  let mut counter = 0;
  Program {
    body: prog
      .body
      .into_iter()
      .map(|exp| mon_exp(exp, &mut counter))
      .collect(),
    types: prog.types,
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
fn mon_exp(exp: Exp<IdxVar>, counter: &mut usize) -> Exp<IdxVar> {
  match exp {
    Exp::Int { .. } => exp,
    Exp::Var { .. } => exp,
    Exp::Let {
      var,
      init,
      body,
      range,
    } => Exp::Let {
      var,
      init: box mon_exp(*init, counter),
      body: box mon_exp(*body, counter),
      range,
    },
    Exp::Prim { op, args, range } => {
      mon_prim(args, |args| Exp::Prim { op, args, range }, counter)
    }
    // ch4
    Exp::If {
      cond,
      conseq,
      alt,
      range,
    } => Exp::If {
      cond: box mon_exp(*cond, counter),
      conseq: box mon_exp(*conseq, counter),
      alt: box mon_exp(*alt, counter),
      range,
    },
    Exp::Bool { .. } => exp,
    // ch5
    Exp::Get { .. } => exp,
    Exp::Set { var, exp, range } => Exp::Set {
      var,
      exp: box mon_exp(*exp, counter),
      range,
    },
    Exp::Begin { seq, last, range } => Exp::Begin {
      seq: seq.into_iter().map(|exp| mon_exp(exp, counter)).collect(),
      last: box mon_exp(*last, counter),
      range,
    },
    Exp::While { cond, body, range } => Exp::While {
      cond: box mon_exp(*cond, counter),
      body: box mon_exp(*body, counter),
      range,
    },
    Exp::Void(_) => exp,
    Exp::Print { args, range } => {
      mon_prim(args, |args| Exp::Print { args, range }, counter)
    }
    Exp::Str { .. } => exp,
    Exp::NewLine(_) => exp,
    Exp::HasType { exp, ty } => Exp::HasType {
      exp: box mon_exp(*exp, counter),
      ty,
    },
  }
}

fn mon_prim(
  args: Vec<Exp<IdxVar>>,
  build_exp: impl FnOnce(Vec<Exp<IdxVar>>) -> Exp<IdxVar>,
  counter: &mut usize,
) -> Exp<IdxVar> {
  let mut tmps = vec![];
  let args = args
    .into_iter()
    .map(|arg| atom_exp(arg, &mut tmps, counter))
    .collect();
  tmps
    .into_iter()
    .rfold(build_exp(args), |body, tmp| Exp::Let {
      range: body.range(),
      var: (tmp.range, tmp.name),
      init: box tmp.init,
      body: box body,
    })
}

/// Process expressions that need to be atomic.
fn atom_exp(
  exp: Exp<IdxVar>,
  tmps: &mut Vec<TmpVar>,
  counter: &mut usize,
) -> Exp<IdxVar> {
  match exp {
    Exp::Let {
      var, init, body, ..
    } => {
      tmps.push(TmpVar {
        range: var.0,
        name: var.1,
        init: mon_exp(*init, counter),
      });
      atom_exp(*body, tmps, counter)
    }
    Exp::Prim { op, args, range } => {
      let args = args
        .into_iter()
        .map(|arg| atom_exp(arg, tmps, counter))
        .collect();
      let exp = Exp::Prim { op, args, range };
      assign_var(exp, tmps, counter)
    }
    Exp::Bool { .. } | Exp::Int { .. } | Exp::Var { .. } => exp,
    // ch4
    Exp::If { .. } => {
      let exp = mon_exp(exp, counter);
      assign_var(exp, tmps, counter)
    }
    // ch5
    Exp::Get { .. }
    | Exp::Begin { .. }
    | Exp::While { .. }
    | Exp::Set { .. }
    | Exp::Print { .. } => {
      let exp = mon_exp(exp, counter);
      assign_var(exp, tmps, counter)
    }
    Exp::Void(_) => exp,
    Exp::Str { .. } => exp,
    Exp::NewLine(_) => exp,
    Exp::HasType { exp, ty } => Exp::HasType {
      exp: box atom_exp(*exp, tmps, counter),
      ty,
    },
  }
}

fn assign_var(
  exp: Exp<IdxVar>,
  tmps: &mut Vec<TmpVar>,
  counter: &mut usize,
) -> Exp<IdxVar> {
  let range = exp.range();
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
  Exp::Var { var: tmp, range }
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
