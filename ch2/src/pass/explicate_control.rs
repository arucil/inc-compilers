use asm::Label;
use ast::{Exp, ExpKind, IdxVar, Program};
use control::{CAtom, CExp, CPrim, CProgram, CStmt, CTail};
use indexmap::IndexSet;
use std::fmt::{self, Debug, Formatter};

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
    body: vec![(Label::Start, explicate_tail(prog.body.pop().unwrap()))],
  }
}

fn explicate_tail(exp: Exp<IdxVar>) -> CTail {
  match exp.kind {
    ExpKind::Int(_) | ExpKind::Var(_) => CTail::Return(CExp::Atom(atom(exp))),
    ExpKind::Prim { op: (_, op), args } => {
      CTail::Return(CExp::Prim(prim(op, args)))
    }
    ExpKind::Let { var, init, body } => {
      explicate_assign(var.1, *init, explicate_tail(*body))
    }
    exp => unimplemented!("unsupported form {:?}", exp),
  }
}

fn explicate_assign(var: IdxVar, init: ast::Exp<IdxVar>, cont: CTail) -> CTail {
  match init.kind {
    ExpKind::Int(_) | ExpKind::Var(_) => {
      let assign = CStmt::Assign {
        var,
        exp: CExp::Atom(atom(init)),
      };
      CTail::Seq(assign, box cont)
    }
    ExpKind::Prim { op: (_, op), args } => {
      let prim = prim(op, args);
      let assign = CStmt::Assign {
        var,
        exp: CExp::Prim(prim),
      };
      CTail::Seq(assign, box cont)
    }
    ExpKind::Let {
      var: (_, var1),
      init: box init1,
      body,
    } => explicate_assign(var1, init1, explicate_assign(var, *body, cont)),
    exp => unimplemented!("unsupported form {:?}", exp),
  }
}

fn atom(exp: Exp<IdxVar>) -> CAtom {
  match exp.kind {
    ExpKind::Int(n) => CAtom::Int(n),
    ExpKind::Var(var) => CAtom::Var(var),
    exp => unreachable!("{:?}", exp),
  }
}

fn prim(op: &str, mut args: Vec<Exp<IdxVar>>) -> CPrim {
  match op {
    "read" => CPrim::Read,
    "-" => {
      if args.len() == 1 {
        CPrim::Neg(atom(args.pop().unwrap()))
      } else {
        CPrim::Sub(atom(args[0].clone()), atom(args.pop().unwrap()))
      }
    }
    "+" => CPrim::Add(atom(args[0].clone()), atom(args.pop().unwrap())),
    _ => unreachable!("{}", op),
  }
}

fn collect_locals<TYPE>(prog: &Program<IdxVar, TYPE>) -> IndexSet<IdxVar> {
  let mut locals = IndexSet::new();
  for exp in &prog.body {
    collect_exp_locals(exp, &mut locals);
  }
  locals
}

fn collect_exp_locals<TYPE>(
  exp: &Exp<IdxVar, TYPE>,
  locals: &mut IndexSet<IdxVar>,
) {
  match &exp.kind {
    ExpKind::Int(_)
    | ExpKind::Var(_)
    | ExpKind::Str(_)
    | ExpKind::Bool(_)
    | ExpKind::Void
    | ExpKind::Get(_) => {}
    ExpKind::Let { var, init, body } => {
      locals.insert(var.1.clone());
      collect_exp_locals(&*init, locals);
      collect_exp_locals(&*body, locals);
    }
    ExpKind::Prim { op: _, args } => {
      for arg in args {
        collect_exp_locals(arg, locals);
      }
    }
    ExpKind::If { cond, conseq, alt } => {
      collect_exp_locals(&*cond, locals);
      collect_exp_locals(&*conseq, locals);
      collect_exp_locals(&*alt, locals);
    }
    ExpKind::Set { var: _, exp } => {
      collect_exp_locals(&*exp, locals);
    }
    ExpKind::Begin { seq, last } => {
      for exp in seq {
        collect_exp_locals(&*exp, locals);
      }
      collect_exp_locals(&*last, locals);
    }
    ExpKind::While { cond, body } => {
      collect_exp_locals(&*cond, locals);
      collect_exp_locals(&*body, locals);
    }
    ExpKind::Print(args) => {
      for arg in args {
        collect_exp_locals(arg, locals);
      }
    }
    ExpKind::NewLine => {}
  }
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use insta::assert_snapshot;

  #[test]
  fn let_in_init() {
    let prog = ast::parse(
      r#"
        (let
          ([y (let ([x 20])
                (+ x
                   (let ([x 22]) x)))])
          y)
        "#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog).unwrap();
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn nested_prims() {
    let prog = ast::parse(
      r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog).unwrap();
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
