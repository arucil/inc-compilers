use ast::{Exp, ExpKind, Program, Type};
use support::Range;

pub fn insert_bounds_check(
  prog: Program<String, Type>,
) -> Program<String, Type> {
  Program {
    body: prog.body.into_iter().map(exp_insert).collect(),
    ..prog
  }
}

fn exp_insert(exp: Exp<String, Type>) -> Exp<String, Type> {
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
      let val = args.pop().unwrap();
      let len = args.pop().unwrap();
      let len_range = len.range;
    }
    ExpKind::Prim {
      op: op @ (_, "vector-ref"),
      mut args,
    } => {}
    ExpKind::Call { name, args } => Exp {
      kind: ExpKind::Call {
        name,
        args: args.into_iter().map(|exp| self.exp_insert(exp)).collect(),
      },
      ..exp
    },
    ExpKind::Let { var, init, body } => Exp {
      kind: ExpKind::Let {
        var,
        init: box exp_insert(*init),
        body: box exp_insert(*body),
      },
      ..exp
    },
    ExpKind::If { cond, conseq, alt } => Exp {
      kind: ExpKind::If {
        cond: box exp_insert(*cond),
        conseq: box exp_insert(*conseq),
        alt: box exp_insert(*alt),
      },
      ..exp
    },
    ExpKind::Set { var, exp: exp1 } => Exp {
      kind: ExpKind::Set {
        var,
        exp: box exp_insert(*exp1),
      },
      ..exp
    },
    ExpKind::Begin { seq, last } => Exp {
      kind: ExpKind::Begin {
        seq: seq.into_iter().map(exp_insert).collect(),
        last: box exp_insert(*last),
      },
      ..exp
    },
    ExpKind::While { cond, body } => Exp {
      kind: ExpKind::While {
        cond: box exp_insert(*cond),
        body: box exp_insert(*body),
      },
      ..exp
    },
    ExpKind::Print(args) => Exp {
      kind: ExpKind::Print(args.into_iter().map(exp_insert).collect()),
      ..exp
    },
  }
}

fn make_check_exp(
  range: Range,
  op: (Range, &'static str),
  len: Exp<String, Type>,
) -> Exp<String, Type> {
  let tmp = "(bounds-tmp)".to_owned();
  let len_var = Exp {
    kind: ExpKind::Var(tmp.clone()),
    range: len.range,
    ty: Type::Int,
  };
  Exp {
    kind: ExpKind::Let {
      var: (len.range, tmp.clone()),
      init: box len,
      body: box Exp {
        kind: ExpKind::If {
          cond: box Exp {
            kind: ExpKind::Prim {
              op: (range, ">="),
              args: vec![
                len_var.clone(),
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
          conseq: box Exp {
            kind: ExpKind::Prim {
              op,
              args: vec![len_var, val],
            },
            range,
            ty: ty.clone(),
          },
          alt: box Exp {
            kind: ExpKind::Begin {
              seq: vec![
                Exp {
                  kind: ExpKind::Print(vec![]),
                  range: exp.range,
                  ty: Type::Void,
                },
                Exp {
                  kind: ExpKind::NewLine,
                  range: exp.range,
                  ty: Type::Void,
                },
              ],
              last: box Exp {
                kind: ExpKind::Prim {
                  op: (exp.range, "exit"),
                  args: vec![Exp {
                    kind: ExpKind::Int(1),
                    range: exp.range,
                    ty: Type::Void,
                  }],
                },
                range: exp.range,
                ty: Type::Void,
              },
            },
            range: exp.range,
            ty: Type::Void,
          },
        },
        range: exp.range,
        ty: exp.ty.clone(),
      },
    },
    range: exp.range,
    ty: exp.ty,
  }
}
