use ast::{Exp, IdxVar, Program};
use std::collections::HashSet;
use support::Range;

pub fn uncover_get(prog: Program<IdxVar>) -> Program<IdxVar> {
  Program {
    body: prog.body.into_iter().map(uncover_exp).collect(),
  }
}

fn uncover_exp(exp: (Range, Exp<IdxVar>)) -> (Range, Exp<IdxVar>) {
  let mut set_vars = HashSet::new();
  collect_set_vars(&exp, &mut set_vars);
  mark_mutable_vars(exp, &set_vars)
}

fn mark_mutable_vars(
  (range, exp): (Range, Exp<IdxVar>),
  set_vars: &HashSet<IdxVar>,
) -> (Range, Exp<IdxVar>) {
  match exp {
    Exp::Var(var) => {
      if set_vars.contains(&var) {
        (range, Exp::Get(var))
      } else {
        (range, Exp::Var(var))
      }
    }
    Exp::Int(_) | Exp::Str(_) | Exp::Bool(_) | Exp::Void => (range, exp),
    Exp::Let { var, init, body } => (
      range,
      Exp::Let {
        var,
        init: box mark_mutable_vars(*init, set_vars),
        body: box mark_mutable_vars(*body, set_vars),
      },
    ),
    Exp::Begin { seq, last } => (
      range,
      Exp::Begin {
        seq: seq
          .into_iter()
          .map(|exp| mark_mutable_vars(exp, set_vars))
          .collect(),
        last: box mark_mutable_vars(*last, set_vars),
      },
    ),
    Exp::If { cond, conseq, alt } => (
      range,
      Exp::If {
        cond: box mark_mutable_vars(*cond, set_vars),
        conseq: box mark_mutable_vars(*conseq, set_vars),
        alt: box mark_mutable_vars(*alt, set_vars),
      },
    ),
    Exp::Prim { op, args } => (
      range,
      Exp::Prim {
        op,
        args: args
          .into_iter()
          .map(|exp| mark_mutable_vars(exp, set_vars))
          .collect(),
      },
    ),
    Exp::Print { args, types } => (
      range,
      Exp::Print {
        args: args
          .into_iter()
          .map(|exp| mark_mutable_vars(exp, set_vars))
          .collect(),
        types,
      },
    ),
    Exp::Set { var, exp } => (
      range,
      Exp::Set {
        var,
        exp: box mark_mutable_vars(*exp, set_vars),
      },
    ),
    Exp::NewLine => (range, exp),
    Exp::While { cond, body } => (
      range,
      Exp::While {
        cond: box mark_mutable_vars(*cond, set_vars),
        body: box mark_mutable_vars(*body, set_vars),
      },
    ),
    Exp::Get(_) => unreachable!(),
  }
}

fn collect_set_vars(
  (_, exp): &(Range, Exp<IdxVar>),
  set_vars: &mut HashSet<IdxVar>,
) {
  match exp {
    Exp::Int(_) | Exp::Str(_) | Exp::Bool(_) | Exp::Void | Exp::Var(_) => {}
    Exp::Let { init, body, .. } => {
      collect_set_vars(init, set_vars);
      collect_set_vars(body, set_vars);
    }
    Exp::Begin { seq, last } => {
      for exp in seq {
        collect_set_vars(exp, set_vars);
      }
      collect_set_vars(last, set_vars);
    }
    Exp::If { cond, conseq, alt } => {
      collect_set_vars(cond, set_vars);
      collect_set_vars(conseq, set_vars);
      collect_set_vars(alt, set_vars);
    }
    Exp::Prim { args, .. } => {
      for exp in args {
        collect_set_vars(exp, set_vars);
      }
    }
    Exp::Print { args, .. } => {
      for exp in args {
        collect_set_vars(exp, set_vars);
      }
    }
    Exp::Set { var, exp } => {
      set_vars.insert(var.1.clone());
      collect_set_vars(exp, set_vars);
    }
    Exp::NewLine => {}
    Exp::While { cond, body } => {
      collect_set_vars(cond, set_vars);
      collect_set_vars(body, set_vars);
    }
    Exp::Get(_) => unreachable!(),
  }
}
