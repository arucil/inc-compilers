use asm::Label;
use ast::{Exp, IdxVar, Program};
use control::{CAtom, CCmpOp, CExp, CPrim, CProgram, CStmt, CTail};
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

struct Cfg {
  blocks: Vec<(Label, CTail)>,
  label_index: u32,
}

impl Cfg {
  fn add_block(&mut self, block: CTail) -> Label {
    let label = Label::Tmp(self.label_index);
    self.label_index += 1;
    self.blocks.push((label, block));
    label
  }
}

pub fn explicate_control(mut prog: Program<IdxVar>) -> CProgram<CInfo> {
  let locals = collect_locals(&prog);
  let mut cfg = Cfg {
    blocks: vec![],
    label_index: 0,
  };
  let start = explicate_tail(&mut cfg, prog.body.pop().unwrap().1);
  cfg.blocks.insert(0, (Label::Start, start));

  CProgram {
    info: CInfo { locals },
    body: cfg.blocks,
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
    Exp::Bool(_) => {}
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
    Exp::If { cond, conseq, alt } => {
      collect_exp_locals(&cond.1, locals);
      collect_exp_locals(&conseq.1, locals);
      collect_exp_locals(&alt.1, locals);
    }
    _ => unimplemented!(),
  }
}

fn explicate_tail(cfg: &mut Cfg, exp: Exp<IdxVar>) -> CTail {
  match exp {
    Exp::Int(_) | Exp::Var(_) | Exp::Bool(_) => {
      CTail::Return(CExp::Atom(atom(exp)))
    }
    Exp::Prim { op: (_, op), args } => {
      CTail::Return(CExp::Prim(prim(op, args)))
    }
    Exp::Let { var, init, body } => {
      let cont = explicate_tail(cfg, body.1);
      explicate_assign(cfg, var.1, init.1, cont)
    }
    Exp::If { cond, conseq, alt } => {
      let conseq = explicate_tail(cfg, conseq.1);
      let alt = explicate_tail(cfg, alt.1);
      explicate_pred(cfg, cond.1, conseq, alt)
    }
    exp => unimplemented!("unsupported form {:?}", exp),
  }
}

fn explicate_assign(
  cfg: &mut Cfg,
  var: IdxVar,
  init: Exp<IdxVar>,
  cont: CTail,
) -> CTail {
  match init {
    Exp::Int(_) | Exp::Var(_) | Exp::Bool(_) => {
      let assign = CStmt::Assign {
        var,
        exp: CExp::Atom(atom(init)),
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
    } => {
      let cont = explicate_assign(cfg, var, body.1, cont);
      explicate_assign(cfg, var1, init1, cont)
    }
    Exp::If { cond, conseq, alt } => {
      let cont = tail_to_goto(cont, cfg);
      let conseq = explicate_assign(cfg, var.clone(), conseq.1, cont.clone());
      let alt = explicate_assign(cfg, var, alt.1, cont);
      explicate_pred(cfg, cond.1, conseq, alt)
    }
    _ => unimplemented!("unsupported form {:?}", init),
  }
}

fn explicate_pred(
  cfg: &mut Cfg,
  cond: Exp<IdxVar>,
  conseq: CTail,
  alt: CTail,
) -> CTail {
  match cond {
    Exp::Bool(v) => {
      if v {
        conseq
      } else {
        alt
      }
    }
    Exp::Var(_) => {
      let conseq = tail_to_label(conseq, cfg);
      let alt = tail_to_label(alt, cfg);
      CTail::If(
        CExp::Prim(CPrim::Cmp(CCmpOp::Eq, atom(cond), CAtom::Bool(false))),
        alt,
        conseq,
      )
    }
    Exp::Let { var, init, body } => {
      let cont = explicate_pred(cfg, body.1, conseq, alt);
      explicate_assign(cfg, var.1, init.1, cont)
    }
    Exp::Prim {
      op: (_, "not"),
      mut args,
    } => explicate_pred(cfg, args.pop().unwrap().1, alt, conseq),
    Exp::Prim { op, mut args } => {
      let conseq = tail_to_label(conseq, cfg);
      let alt = tail_to_label(alt, cfg);
      let cmp = CCmpOp::from_str(op.1).unwrap();
      let arg2 = args.pop().unwrap().1;
      let arg1 = args.pop().unwrap().1;
      CTail::If(
        CExp::Prim(CPrim::Cmp(cmp, atom(arg1), atom(arg2))),
        conseq,
        alt,
      )
    }
    Exp::If {
      cond: cond1,
      conseq: conseq1,
      alt: alt1,
    } => {
      let conseq = tail_to_goto(conseq, cfg);
      let alt = tail_to_goto(alt, cfg);
      let conseq1 = explicate_pred(cfg, conseq1.1, conseq.clone(), alt.clone());
      let alt1 = explicate_pred(cfg, alt1.1, conseq, alt);
      explicate_pred(cfg, cond1.1, conseq1, alt1)
    }
    _ => unimplemented!(),
  }
}

fn tail_to_label(tail: CTail, cfg: &mut Cfg) -> Label {
  match tail {
    CTail::Goto(label) => label,
    _ => cfg.add_block(tail),
  }
}

fn tail_to_goto(tail: CTail, cfg: &mut Cfg) -> CTail {
  match tail {
    CTail::Goto(_) => tail,
    _ => CTail::Goto(cfg.add_block(tail)),
  }
}

fn atom(exp: Exp<IdxVar>) -> CAtom {
  match exp {
    Exp::Int(n) => CAtom::Int(n),
    Exp::Var(var) => CAtom::Var(var),
    Exp::Bool(v) => CAtom::Bool(v),
    _ => unreachable!("{:?}", exp),
  }
}

fn prim(op: &str, mut args: Vec<(Range, Exp<IdxVar>)>) -> CPrim {
  match op {
    "read" => CPrim::Read,
    "-" => {
      if args.len() == 1 {
        let arg = args.pop().unwrap().1;
        CPrim::Neg(atom(arg))
      } else {
        let arg2 = args.pop().unwrap().1;
        let arg1 = args.pop().unwrap().1;
        CPrim::Sub(atom(arg1), atom(arg2))
      }
    }
    "+" => {
      let arg2 = args.pop().unwrap().1;
      let arg1 = args.pop().unwrap().1;
      CPrim::Add(atom(arg1), atom(arg2))
    }
    "not" => {
      let arg = args.pop().unwrap().1;
      CPrim::Not(atom(arg))
    }
    "eq?" | ">" | ">=" | "<" | "<=" => {
      let arg2 = args.pop().unwrap().1;
      let arg1 = args.pop().unwrap().1;
      CPrim::Cmp(CCmpOp::from_str(op).unwrap(), atom(arg1), atom(arg2))
    }
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

  #[test]
  fn if_form() {
    let prog =
      ast::parse(r#"(if (> 3 (read)) (- 7 2) (- (let ([x 3]) (+ x (read)))))"#)
        .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn nested_if() {
    let prog = ast::parse(
      r#"(let ([x (read)] [y (read)]) (if (if (< x 1) (eq? x 0) (eq? x 2)) (+ y 2) (+ y 10)))"#,
    )
    .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn const_cond() {
    let prog = ast::parse(
      r#"(if (not #t) 1 2)"#,
    )
    .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn if_in_init() {
    let prog = ast::parse(
      r#"(let ([x (if (>= (read) 3) 10 77)]) (if (not (eq? x 10)) 41 2))"#,
    )
    .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn var_cond() {
    let prog = ast::parse(
      r#"(let ([x (eq? (read) 20)]) (if x 42 89))"#,
    )
    .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn shrink_and() {
    let prog = ast::parse(
      r#"(if (and (eq? (read) 0) (eq? (read) 2)) 0 42)"#,
    )
    .unwrap();
    let prog = super::super::shrink::shrink(prog);
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
