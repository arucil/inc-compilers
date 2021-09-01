use ast::{Exp, IdxVar, Program};
use control::{CAtom, CBlock, CExp, CPrim, CProgram, CStmt, CTail};
use indexmap::IndexSet;
use std::fmt::{self, Debug, Formatter};
use support::Range;

pub struct CInfo {
  pub locals: IndexSet<IdxVar>,
}

impl Debug for CInfo {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}\n", self.locals)
  }
}

pub fn explicate_control(mut prog: Program<IdxVar>) -> CProgram<CInfo> {
  let locals = collect_locals(&prog);
  CProgram {
    info: CInfo { locals },
    body: vec![(
      "start".to_owned(),
      explicate_block(prog.body.pop().unwrap().1),
    )],
  }
}

fn collect_locals(prog: &Program<IdxVar>) -> IndexSet<IdxVar> {
  let mut locals = IndexSet::new();
  for (_, exp) in &prog.body {
    collect_exp_locals(exp, &mut locals);
  }
  locals
}

fn collect_exp_locals(exp: &Exp<IdxVar>, locals: &mut IndexSet<IdxVar>) {
  match exp {
    Exp::Int(_) => {}
    Exp::Var(var) => {
      locals.insert(var.clone());
    }
    Exp::Str(_) => {}
    Exp::Let { var, init, body } => {
      locals.insert(var.1.clone());
      collect_exp_locals(&init.1, locals);
      collect_exp_locals(&body.1, locals);
    }
    Exp::Prim { op: _, args } => {
      for (_, arg) in args {
        collect_exp_locals(arg, locals);
      }
    }
    _ => unimplemented!(),
  }
}

fn explicate_block(exp: Exp<IdxVar>) -> CBlock {
  let mut stmts: Vec<CStmt> = vec![];
  let tail = explicate_tail(exp, &mut stmts);
  CBlock { stmts, tail }
}

fn explicate_tail(mut exp: Exp<IdxVar>, stmts: &mut Vec<CStmt>) -> CTail {
  loop {
    match exp {
      exp @ (Exp::Int(_) | Exp::Var(_)) => {
        return CTail::Return(CExp::Atom(atom(exp)))
      }
      Exp::Prim { op: (_, op), args } => {
        return CTail::Return(CExp::Prim(prim(op, args)))
      }
      Exp::Let { var, init, body } => {
        explicate_assign(var.1, init.1, stmts);
        exp = body.1;
      }
      exp => unimplemented!("unsupported form {:?}", exp),
    }
  }
}

fn explicate_assign(
  var: IdxVar,
  mut init: Exp<IdxVar>,
  stmts: &mut Vec<CStmt>,
) {
  loop {
    match init {
      exp @ (Exp::Int(_) | Exp::Var(_)) => {
        let assign = CStmt::Assign {
          var,
          exp: CExp::Atom(atom(exp)),
        };
        stmts.push(assign);
        return;
      }
      Exp::Prim { op: (_, op), args } => {
        let prim = prim(op, args);
        let assign = CStmt::Assign {
          var,
          exp: CExp::Prim(prim),
        };
        stmts.push(assign);
        return;
      }
      Exp::Let {
        var: (_, var1),
        init: box (_, init1),
        body,
      } => {
        explicate_assign(var1, init1, stmts);
        init = body.1;
      }
      exp => unimplemented!("unsupported form {:?}", exp),
    }
  }
}

fn atom(exp: Exp<IdxVar>) -> CAtom {
  match exp {
    Exp::Int(n) => CAtom::Int(n),
    Exp::Var(var) => CAtom::Var(var),
    exp => unreachable!("{:?}", exp),
  }
}

fn prim(op: &str, mut args: Vec<(Range, Exp<IdxVar>)>) -> CPrim {
  match op {
    "read" => CPrim::Read,
    "-" => CPrim::Neg(atom(args.pop().unwrap().1)),
    "+" => CPrim::Add(atom(args[0].1.clone()), atom(args.pop().unwrap().1)),
    _ => unreachable!("{}", op),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_snapshot;

  #[test]
  fn let_in_init() {
    let prog =
      ast::parse(r#"(let ([y (let ([x 20]) (+ x (let ([x 22]) x)))]) y)"#)
        .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn nested_prims() {
    let prog = ast::parse(
      r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#,
    )
    .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}