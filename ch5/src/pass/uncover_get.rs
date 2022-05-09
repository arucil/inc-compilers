use ast::{Exp, ExpKind, IdxVar, Program};
use std::collections::HashSet;

pub fn uncover_get<TYPE>(prog: Program<IdxVar, TYPE>) -> Program<IdxVar, TYPE> {
  Program {
    body: prog.body.into_iter().map(uncover_exp).collect(),
    types: prog.types,
  }
}

fn uncover_exp<TYPE>(exp: Exp<IdxVar, TYPE>) -> Exp<IdxVar, TYPE> {
  let mut set_vars = HashSet::new();
  collect_set_vars(&exp, &mut set_vars);
  mark_mutable_vars(exp, &set_vars)
}

fn mark_mutable_vars<TYPE>(
  exp: Exp<IdxVar, TYPE>,
  set_vars: &HashSet<IdxVar>,
) -> Exp<IdxVar, TYPE> {
  match exp.kind {
    ExpKind::Var(var) => {
      if set_vars.contains(&var) {
        Exp {
          kind: ExpKind::Get(var),
          range: exp.range,
          ty: exp.ty,
        }
      } else {
        Exp {
          kind: ExpKind::Var(var),
          range: exp.range,
          ty: exp.ty,
        }
      }
    }
    ExpKind::Int(_) | ExpKind::Str(_) | ExpKind::Bool(_) | ExpKind::Void => exp,
    ExpKind::Let { var, init, body } => Exp {
      kind: ExpKind::Let {
        var,
        init: box mark_mutable_vars(*init, set_vars),
        body: box mark_mutable_vars(*body, set_vars),
      },
      range: exp.range,
      ty: exp.ty,
    },
    ExpKind::Begin { seq, last } => Exp {
      kind: ExpKind::Begin {
        seq: seq
          .into_iter()
          .map(|exp| mark_mutable_vars(exp, set_vars))
          .collect(),
        last: box mark_mutable_vars(*last, set_vars),
      },
      range: exp.range,
      ty: exp.ty,
    },
    ExpKind::If { cond, conseq, alt } => Exp {
      kind: ExpKind::If {
        cond: box mark_mutable_vars(*cond, set_vars),
        conseq: box mark_mutable_vars(*conseq, set_vars),
        alt: box mark_mutable_vars(*alt, set_vars),
      },
      range: exp.range,
      ty: exp.ty,
    },
    ExpKind::Prim { op, args } => Exp {
      kind: ExpKind::Prim {
        op,
        args: args
          .into_iter()
          .map(|exp| mark_mutable_vars(exp, set_vars))
          .collect(),
      },
      range: exp.range,
      ty: exp.ty,
    },
    ExpKind::Print(args) => Exp {
      kind: ExpKind::Print(
        args
          .into_iter()
          .map(|exp| mark_mutable_vars(exp, set_vars))
          .collect(),
      ),
      range: exp.range,
      ty: exp.ty,
    },
    ExpKind::Set { var, exp: exp1 } => Exp {
      kind: ExpKind::Set {
        var,
        exp: box mark_mutable_vars(*exp1, set_vars),
      },
      range: exp.range,
      ty: exp.ty,
    },
    ExpKind::NewLine => exp,
    ExpKind::While { cond, body } => Exp {
      kind: ExpKind::While {
        cond: box mark_mutable_vars(*cond, set_vars),
        body: box mark_mutable_vars(*body, set_vars),
      },
      range: exp.range,
      ty: exp.ty,
    },
    ExpKind::Get(_) => unreachable!(),
  }
}

fn collect_set_vars<TYPE>(
  exp: &Exp<IdxVar, TYPE>,
  set_vars: &mut HashSet<IdxVar>,
) {
  match &exp.kind {
    ExpKind::Int(_)
    | ExpKind::Str(_)
    | ExpKind::Bool(_)
    | ExpKind::Void
    | ExpKind::Var(_) => {}
    ExpKind::Let { init, body, .. } => {
      collect_set_vars(init, set_vars);
      collect_set_vars(body, set_vars);
    }
    ExpKind::Begin { seq, last } => {
      for exp in seq {
        collect_set_vars(exp, set_vars);
      }
      collect_set_vars(last, set_vars);
    }
    ExpKind::If { cond, conseq, alt } => {
      collect_set_vars(cond, set_vars);
      collect_set_vars(conseq, set_vars);
      collect_set_vars(alt, set_vars);
    }
    ExpKind::Prim { args, .. } => {
      for exp in args {
        collect_set_vars(exp, set_vars);
      }
    }
    ExpKind::Print(args) => {
      for exp in args {
        collect_set_vars(exp, set_vars);
      }
    }
    ExpKind::Set { var, exp } => {
      set_vars.insert(var.1.clone());
      collect_set_vars(exp, set_vars);
    }
    ExpKind::NewLine => {}
    ExpKind::While { cond, body } => {
      collect_set_vars(cond, set_vars);
      collect_set_vars(body, set_vars);
    }
    ExpKind::Get(_) => unreachable!(),
  }
}
