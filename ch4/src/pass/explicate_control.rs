use asm::Label;
use ast::{Exp, IdxVar, PrintType, Program};
use control::{CAtom, CCmpOp, CExp, CPrim, CProgram, CStmt, CTail, CType};
use indexmap::{IndexMap, IndexSet};
use std::collections::VecDeque;
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

struct State {
  blocks: Vec<(Label, CTail)>,
  label_index: u32,
}

impl State {
  fn new_label(&mut self) -> Label {
    let label = Label::Tmp(self.label_index);
    self.label_index += 1;
    label
  }

  fn add_label_block(&mut self, label: Label, block: CTail) {
    self.blocks.push((label, block));
  }

  fn add_block(&mut self, block: CTail) -> Label {
    let label = self.new_label();
    self.add_label_block(label, block);
    label
  }

  fn tail_to_label(&mut self, tail: CTail) -> Label {
    match tail {
      CTail::Goto(label) => label,
      _ => self.add_block(tail),
    }
  }

  fn tail_to_goto(&mut self, tail: CTail) -> CTail {
    match tail {
      CTail::Goto(_) => tail,
      _ => CTail::Goto(self.add_block(tail)),
    }
  }
}

pub fn explicate_control(mut prog: Program<IdxVar>) -> CProgram<CInfo> {
  let locals = ch2::pass::explicate_control::collect_locals(&prog);
  let mut state = State {
    blocks: vec![],
    label_index: 0,
  };
  let start = explicate_tail(&mut state, prog.body.pop().unwrap().1);
  state.blocks.insert(0, (Label::Start, start));

  let blocks = remove_unreachable_blocks(state.blocks);

  CProgram {
    info: CInfo { locals },
    body: blocks,
  }
}

fn remove_unreachable_blocks(
  blocks: Vec<(Label, CTail)>,
) -> Vec<(Label, CTail)> {
  let mut blocks: IndexMap<_, _> = blocks.into_iter().collect();
  let mut reachable_blocks = IndexMap::<Label, CTail>::default();
  let mut worklist = VecDeque::new();
  worklist.push_back((Label::Start, blocks.remove(&Label::Start).unwrap()));

  while let Some((label, block)) = worklist.pop_front() {
    let mut tail = &block;
    loop {
      match tail {
        CTail::Seq(_, t) => {
          tail = &**t;
          continue;
        }
        CTail::Goto(goto_label) => {
          if let Some(goto_block) = blocks.remove(goto_label) {
            worklist.push_back((*goto_label, goto_block));
          }
        }
        CTail::If {
          conseq: goto_label1,
          alt: goto_label2,
          ..
        } => {
          if let Some(goto_block) = blocks.remove(goto_label1) {
            worklist.push_back((*goto_label1, goto_block));
          }
          if let Some(goto_block) = blocks.remove(goto_label2) {
            worklist.push_back((*goto_label2, goto_block));
          }
        }
        _ => {}
      }
      break;
    }
    reachable_blocks.insert(label, block);
  }

  reachable_blocks.into_iter().collect()
}

fn explicate_tail(state: &mut State, exp: Exp<IdxVar>) -> CTail {
  match exp {
    Exp::Int(_) | Exp::Var(_) | Exp::Bool(_) | Exp::Str(_) => {
      CTail::Return(CExp::Atom(atom(exp)))
    }
    Exp::Prim { op: (_, op), args } => {
      CTail::Return(CExp::Prim(prim(op, args)))
    }
    Exp::Let { var, init, body } => {
      let cont = explicate_tail(state, body.1);
      explicate_assign(state, var.1, init.1, cont)
    }
    Exp::If { cond, conseq, alt } => {
      let conseq = explicate_tail(state, conseq.1);
      let alt = explicate_tail(state, alt.1);
      explicate_pred(state, cond.1, conseq, alt)
    }
    Exp::Begin { seq, last } => {
      let cont = explicate_tail(state, last.1);
      explicate_effect(state, seq.into_iter(), cont)
    }
    Exp::While { .. } | Exp::Set { .. } | Exp::NewLine => unreachable!(),
    _ => unimplemented!("unsupported form {:?}", exp),
  }
}

fn explicate_assign(
  state: &mut State,
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
      let cont = explicate_assign(state, var, body.1, cont);
      explicate_assign(state, var1, init1, cont)
    }
    Exp::If { cond, conseq, alt } => {
      let cont = state.tail_to_goto(cont);
      let conseq = explicate_assign(state, var.clone(), conseq.1, cont.clone());
      let alt = explicate_assign(state, var, alt.1, cont);
      explicate_pred(state, cond.1, conseq, alt)
    }
    Exp::Begin { seq, last } => {
      let cont = explicate_assign(state, var, last.1, cont);
      explicate_effect(state, seq.into_iter(), cont)
    }
    Exp::While { .. } | Exp::Set { .. } | Exp::NewLine => unreachable!(),
    _ => unimplemented!("unsupported form {:?}", init),
  }
}

fn explicate_pred(
  state: &mut State,
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
      let conseq = state.tail_to_label(conseq);
      let alt = state.tail_to_label(alt);
      CTail::If {
        cmp: CCmpOp::Eq,
        lhs: atom(cond),
        rhs: CAtom::Bool(false),
        conseq: alt,
        alt: conseq,
      }
    }
    Exp::Let { var, init, body } => {
      let cont = explicate_pred(state, body.1, conseq, alt);
      explicate_assign(state, var.1, init.1, cont)
    }
    Exp::Prim {
      op: (_, "not"),
      mut args,
    } => explicate_pred(state, args.pop().unwrap().1, alt, conseq),
    Exp::Prim { op, mut args } => {
      let conseq = state.tail_to_label(conseq);
      let alt = state.tail_to_label(alt);
      let cmp = op.1.parse().unwrap();
      let rhs = args.pop().unwrap().1;
      let lhs = args.pop().unwrap().1;
      CTail::If {
        cmp,
        lhs: atom(lhs),
        rhs: atom(rhs),
        conseq,
        alt,
      }
    }
    Exp::If {
      cond: cond1,
      conseq: conseq1,
      alt: alt1,
    } => {
      let conseq = state.tail_to_goto(conseq);
      let alt = state.tail_to_goto(alt);
      let conseq1 =
        explicate_pred(state, conseq1.1, conseq.clone(), alt.clone());
      let alt1 = explicate_pred(state, alt1.1, conseq, alt);
      explicate_pred(state, cond1.1, conseq1, alt1)
    }
    Exp::Begin { seq, last } => {
      let cont = explicate_pred(state, last.1, conseq, alt);
      explicate_effect(state, seq.into_iter(), cont)
    }
    Exp::While { .. } | Exp::Set { .. } | Exp::NewLine => unreachable!(),
    _ => unimplemented!(),
  }
}

fn explicate_effect(
  state: &mut State,
  seq: impl DoubleEndedIterator<Item = (Range, Exp<IdxVar>)>,
  cont: CTail,
) -> CTail {
  seq.rfold(cont, |cont, exp| explicate_exp_effect(state, exp.1, cont))
}

fn explicate_exp_effect(
  state: &mut State,
  exp: Exp<IdxVar>,
  cont: CTail,
) -> CTail {
  match exp {
    Exp::Bool(_) | Exp::Int(_) | Exp::Var(_) | Exp::Str(_) | Exp::Void => cont,
    Exp::Prim {
      op: (_, "read"),
      args: _,
    } => CTail::Seq(CStmt::Read, box cont),
    Exp::Prim { op: _, args } => {
      explicate_effect(state, args.into_iter(), cont)
    }
    Exp::Let { var, init, body } => {
      let body = explicate_exp_effect(state, body.1, cont);
      explicate_assign(state, var.1, init.1, body)
    }
    Exp::If { cond, conseq, alt } => {
      let cont = state.tail_to_goto(cont);
      let conseq = explicate_exp_effect(state, conseq.1, cont.clone());
      let alt = explicate_exp_effect(state, alt.1, cont);
      explicate_pred(state, cond.1, conseq, alt)
    }
    Exp::Set { var, exp } => explicate_assign(state, var.1, exp.1, cont),
    Exp::Begin { seq, last } => {
      let cont = explicate_exp_effect(state, last.1, cont);
      explicate_effect(state, seq.into_iter(), cont)
    }
    Exp::While { cond, body } => {
      let loop_start = state.new_label();
      let body = explicate_exp_effect(state, body.1, CTail::Goto(loop_start));
      let block = explicate_pred(state, cond.1, body, cont);
      state.add_label_block(loop_start, block);
      CTail::Goto(loop_start)
    }
    Exp::NewLine => CTail::Seq(CStmt::NewLine, box cont),
    Exp::Print { val, ty } => {
      let ty = match ty {
        PrintType::Int => CType::Int,
        PrintType::Bool => CType::Bool,
        PrintType::Str => CType::Str,
      };
      explicate_print(state, val.1, ty, cont)
    }
  }
}

fn explicate_print(
  state: &mut State,
  exp: Exp<IdxVar>,
  ty: CType,
  cont: CTail,
) -> CTail {
  match exp {
    Exp::Int(_) | Exp::Var(_) | Exp::Bool(_) | Exp::Str(_) => CTail::Seq(
      CStmt::Print {
        val: CExp::Atom(atom(exp)),
        ty,
      },
      box cont,
    ),
    Exp::Prim { op: (_, op), args } => CTail::Seq(
      CStmt::Print {
        val: CExp::Prim(prim(op, args)),
        ty,
      },
      box cont,
    ),
    Exp::Let {
      var: (_, var1),
      init: box (_, init1),
      body,
    } => {
      let cont = explicate_print(state, body.1, ty, cont);
      explicate_assign(state, var1, init1, cont)
    }
    Exp::If { cond, conseq, alt } => {
      let cont = state.tail_to_goto(cont);
      let conseq = explicate_print(state, conseq.1, ty, cont.clone());
      let alt = explicate_print(state, alt.1, ty, cont);
      explicate_pred(state, cond.1, conseq, alt)
    }
    Exp::Begin { seq, last } => {
      let cont = explicate_print(state, last.1, ty, cont);
      explicate_effect(state, seq.into_iter(), cont)
    }
    Exp::While { .. } | Exp::Set { .. } | Exp::NewLine => unreachable!(),
    _ => unimplemented!("unsupported form {:?}", exp),
  }
}

fn atom(exp: Exp<IdxVar>) -> CAtom {
  match exp {
    Exp::Int(n) => CAtom::Int(n),
    Exp::Var(var) => CAtom::Var(var),
    Exp::Bool(v) => CAtom::Bool(v),
    Exp::Str(s) => CAtom::Str(s),
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
      CPrim::Cmp(op.parse().unwrap(), atom(arg1), atom(arg2))
    }
    _ => unreachable!("{}", op),
  }
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use ch2::pass::remove_complex_operands;
  use insta::assert_snapshot;

  #[test]
  fn let_in_init() {
    let prog =
      ast::parse(r#"(let ([y (let ([x 20]) (+ x (let ([x 22]) x)))]) y)"#)
        .unwrap();
    let prog = uniquify::uniquify(prog);
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
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn if_form() {
    let prog =
      ast::parse(r#"(if (> 3 (read)) (- 7 2) (- (let ([x 3]) (+ x (read)))))"#)
        .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn nested_if() {
    let prog = ast::parse(
      r#"
(let ([x (read)]
      [y (read)])
  (if (if (< x 1)
        (eq? x 0)
        (eq? x 2))
    (+ y 2)
    (+ y 10)))
"#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn const_cond() {
    let prog = ast::parse(r#"(if (not #t) 1 2)"#).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn if_in_init() {
    let prog = ast::parse(
      r#"(let ([x (if (>= (read) 3) 10 77)]) (if (not (eq? x 10)) 41 2))"#,
    )
    .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn var_cond() {
    let prog =
      ast::parse(r#"(let ([x (eq? (read) 20)]) (if x 42 89))"#).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn shrink_and() {
    let prog =
      ast::parse(r#"(if (and (eq? (read) 0) (eq? (read) 2)) 0 42)"#).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn begin() {
    let prog = ast::parse(
      r#"
(let ([x 1] [y (read)])
  (while (> x 10)
    (set! x (begin (print "x=" x) (+ x (- y 1))))
    (print (> x (- y 1))))
  (begin 1 2 3)
  (let ([k (+ x 1)])
    (print k)
    k)
  (+ 7 (+ (read) (if (begin (set! y 37) (not (eq? y x))) 11 23)))
  (+ x y))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
