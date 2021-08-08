use ast::{Exp, IdxVar, Program};
use indexmap::IndexSet;
use std::fmt::{self, Debug, Formatter, Write};
use support::Range;

pub struct CProgram {
  pub info: CInfo,
  pub body: Vec<(String, CTail)>,
}

pub struct CInfo {
  pub locals: IndexSet<IdxVar>,
}

pub enum CTail {
  Return(CExp),
  Seq(CStmt, Box<CTail>),
}

pub enum CStmt {
  Assign { var: IdxVar, exp: CExp },
}

pub enum CExp {
  Atom(CAtom),
  Prim(CPrim),
}

pub enum CPrim {
  Read,
  Neg(CAtom),
  Add(CAtom, CAtom),
}

pub enum CAtom {
  Int(i64),
  Var(IdxVar),
}

impl CProgram {
  #[allow(unused)]
  pub fn to_string_pretty(&self) -> String {
    let mut buf = format!("{:?}", self.info);
    for (label, tail) in &self.body {
      writeln!(&mut buf, "{}:", label).unwrap();
      buf += &format!("{:?}", tail);
    }
    buf
  }
}

impl Debug for CInfo {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}\n", self.locals)
  }
}

impl Debug for CTail {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Return(exp) => {
        write!(f, "    return {:?}", exp)
      }
      Self::Seq(stmt, tail) => {
        write!(f, "    {:?}\n{:?}", stmt, tail)
      }
    }
  }
}

impl Debug for CStmt {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Assign { var, exp } => {
        write!(f, "{:?} = {:?}", var, exp)
      }
    }
  }
}

impl Debug for CExp {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Atom(atom) => atom.fmt(f),
      Self::Prim(prim) => prim.fmt(f),
    }
  }
}

impl Debug for CPrim {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Add(arg1, arg2) => {
        write!(f, "(+ {:?} {:?})", arg1, arg2)
      }
      Self::Neg(arg) => {
        write!(f, "(- {:?})", arg)
      }
      Self::Read => {
        write!(f, "(read)")
      }
    }
  }
}

impl Debug for CAtom {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Self::Int(n) => write!(f, "{}", n),
      Self::Var(n) => write!(f, "{:?}", n),
    }
  }
}

pub fn explicate_control(mut prog: Program<IdxVar>) -> CProgram {
  let locals = collect_locals(&prog);
  CProgram {
    info: CInfo { locals },
    body: vec![(
      "start".to_owned(),
      explicate_tail(prog.body.pop().unwrap().1),
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

fn explicate_tail(exp: Exp<IdxVar>) -> CTail {
  match exp {
    exp @ (Exp::Int(_) | Exp::Var(_)) => CTail::Return(CExp::Atom(atom(exp))),
    ast::Exp::Prim { op: (_, op), args } => {
      CTail::Return(CExp::Prim(prim(op, args)))
    }
    ast::Exp::Let { var, init, body } => {
      explicate_assign(var.1, init.1, explicate_tail(body.1))
    }
    exp => unimplemented!("unsupported form {:?}", exp),
  }
}

fn explicate_assign(var: IdxVar, init: ast::Exp<IdxVar>, cont: CTail) -> CTail {
  match init {
    exp @ (Exp::Int(_) | Exp::Var(_)) => {
      let assign = CStmt::Assign {
        var,
        exp: CExp::Atom(atom(exp)),
      };
      CTail::Seq(assign, box cont)
    }
    Exp::Prim { op: (_, op), args } => {
      let prim = prim(op, args);
      let assign = CStmt::Assign {
        var,
        exp: CExp::Prim(prim),
      };
      CTail::Seq(assign, box cont)
    }
    Exp::Let {
      var: (_, var1),
      init: box (_, init1),
      body,
    } => explicate_assign(var1, init1, explicate_assign(var, body.1, cont)),
    exp => unimplemented!("unsupported form {:?}", exp),
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
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn let_in_init() {
    let prog =
      parse(r#"(let ([y (let ([x 20]) (+ x (let ([x 22]) x)))]) y)"#).unwrap();
    let prog = super::super::uniquify::uniquify(prog).unwrap();
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = super::super::uniquify::uniquify(prog).unwrap();
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}