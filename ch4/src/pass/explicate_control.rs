use asm::Label;
use ast::{Error, Exp, ExpKind, IdxVar, Program, Type};
use control::{
  CAtom, CCmpOp, CError, CExp, CFun, CPrim, CProgram, CStmt, CTail,
};
use id_arena::Arena;
use indexmap::IndexMap;
use std::collections::VecDeque;
use std::fmt::{self, Debug, Formatter};

pub struct CInfo {
  pub locals: IndexMap<IdxVar, Type>,
}

impl Debug for CInfo {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "locals: {:?}", self.locals)
  }
}

struct State<'a> {
  blocks: Vec<(Label, CTail)>,
  label_index: u32,
  types: &'a Arena<Type>,
}

impl<'a> State<'a> {
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

  fn resolve_type(&self, ty: &Type) -> Type {
    ty.resolved(self.types)
  }
}

pub fn explicate_control(prog: Program<IdxVar, Type>) -> CProgram<CInfo> {
  let mut state = State {
    blocks: vec![],
    label_index: 0,
    types: &prog.types,
  };
  let funs = prog
    .fun_defs
    .into_iter()
    .map(|(name, fun)| {
      let locals = collect_locals(&fun.body);
      state.blocks.clear();
      let body = explicate_tail(&mut state, fun.body);
      state.blocks.insert(0, (Label::Start, body));
      CFun {
        name,
        params: fun.params,
        info: CInfo { locals },
        ty: fun.ret,
        blocks: std::mem::take(&mut state.blocks),
      }
    })
    .collect();

  let locals = collect_locals(&prog.body);
  state.blocks.clear();
  let body = explicate_tail(&mut state, prog.body);
  state.blocks.insert(0, (Label::Start, body));

  let blocks = remove_unreachable_blocks(state.blocks);

  CProgram {
    info: CInfo { locals },
    funs,
    body: blocks,
    types: prog.types,
  }
}

pub fn collect_locals(exp: &Exp<IdxVar, Type>) -> IndexMap<IdxVar, Type> {
  let mut locals = IndexMap::new();
  collect_exp_locals(exp, &mut locals);
  locals
}

fn collect_exp_locals(
  exp: &Exp<IdxVar, Type>,
  locals: &mut IndexMap<IdxVar, Type>,
) {
  match &exp.kind {
    ExpKind::Int(_)
    | ExpKind::Var(_)
    | ExpKind::Str(_)
    | ExpKind::Bool(_)
    | ExpKind::Void
    | ExpKind::Get(_) => {}
    ExpKind::Let { var, init, body } => {
      if init.ty != Type::Void {
        locals.insert(var.1.clone(), init.ty.clone());
      }
      collect_exp_locals(init, locals);
      collect_exp_locals(body, locals);
    }
    ExpKind::Prim { op: _, args } => {
      for arg in args {
        collect_exp_locals(arg, locals);
      }
    }
    ExpKind::Apply {
      fun,
      args,
      r#struct: _,
    } => {
      collect_exp_locals(fun, locals);
      for arg in args {
        collect_exp_locals(arg, locals);
      }
    }
    ExpKind::If { cond, conseq, alt } => {
      collect_exp_locals(cond, locals);
      collect_exp_locals(conseq, locals);
      collect_exp_locals(alt, locals);
    }
    ExpKind::Set { var: _, exp } => {
      collect_exp_locals(exp, locals);
    }
    ExpKind::Begin { seq, last } => {
      for exp in seq {
        collect_exp_locals(exp, locals);
      }
      collect_exp_locals(last, locals);
    }
    ExpKind::While { cond, body } => {
      collect_exp_locals(cond, locals);
      collect_exp_locals(body, locals);
    }
    ExpKind::Print(args) => {
      for arg in args {
        collect_exp_locals(arg, locals);
      }
    }
    ExpKind::NewLine => {}
    ExpKind::Error(Error::Length(len)) => collect_exp_locals(len, locals),
    ExpKind::Error(Error::OutOfBounds { index, len }) => {
      collect_exp_locals(index, locals);
      collect_exp_locals(len, locals);
    }
    ExpKind::Error(Error::DivByZero) => {}
    ExpKind::FunRef { .. } => {}
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

fn explicate_tail(state: &mut State, exp: Exp<IdxVar, Type>) -> CTail {
  match exp.kind {
    ExpKind::Int(_)
    | ExpKind::Var(_)
    | ExpKind::Bool(_)
    | ExpKind::Str(_)
    | ExpKind::Void
    | ExpKind::Get(_) => CTail::Return(CExp::Atom(atom(exp))),
    ExpKind::Prim {
      op: (_, "vector-set!" | "copy-string!"),
      ..
    } => {
      explicate_exp_effect(state, exp, CTail::Return(CExp::Atom(CAtom::Void)))
    }
    // TODO this should be removed in chapter 9
    ExpKind::Prim {
      op: (_, op @ "vector-length"),
      args,
    } => match state.resolve_type(&args[0].ty) {
      Type::Tuple(fields) => {
        CTail::Return(CExp::Atom(CAtom::Int(fields.len() as i64)))
      }
      Type::Array(_) => CTail::Return(CExp::Prim(prim(state, op, args))),
      _ => unreachable!(),
    },
    ExpKind::Prim {
      op: (_, "length-error"),
      mut args,
    } => CTail::Error(CError::Length(atom(args.pop().unwrap()))),
    ExpKind::Prim { op: (_, op), args } => {
      if exp.ty == Type::Void {
        CTail::Return(CExp::Atom(CAtom::Void))
      } else {
        CTail::Return(CExp::Prim(prim(state, op, args)))
      }
    }
    ExpKind::Apply {
      fun,
      args,
      r#struct: _,
    } => CTail::TailCall(atom(*fun), args.into_iter().map(atom).collect()),
    ExpKind::Let { var, init, body } => {
      let cont = explicate_tail(state, *body);
      explicate_assign(state, var.1, *init, cont)
    }
    ExpKind::If { cond, conseq, alt } => {
      let conseq = explicate_tail(state, *conseq);
      let alt = explicate_tail(state, *alt);
      explicate_pred(state, *cond, conseq, alt)
    }
    ExpKind::Begin { seq, last } => {
      let cont = explicate_tail(state, *last);
      explicate_effect(state, seq.into_iter(), cont)
    }
    ExpKind::While { .. }
    | ExpKind::Set { .. }
    | ExpKind::Print { .. }
    | ExpKind::NewLine => {
      explicate_exp_effect(state, exp, CTail::Return(CExp::Atom(CAtom::Void)))
    }
    ExpKind::Error(err) => explicate_error(err),
    ExpKind::FunRef { name, arity } => {
      CTail::Return(CExp::Atom(CAtom::FunRef(name, arity)))
    }
  }
}

fn explicate_error(err: Error<IdxVar, Type>) -> CTail {
  match err {
    Error::Length(len) => CTail::Error(CError::Length(atom(*len))),
    Error::OutOfBounds { index, len } => CTail::Error(CError::OutOfBounds {
      index: atom(*index),
      len: atom(*len),
    }),
    Error::DivByZero => CTail::Error(CError::DivByZero),
  }
}

fn explicate_assign(
  state: &mut State,
  var: IdxVar,
  init: Exp<IdxVar, Type>,
  cont: CTail,
) -> CTail {
  match init.kind {
    ExpKind::Int(_) | ExpKind::Bool(_) | ExpKind::Str(_) => {
      let assign = CStmt::Assign {
        var,
        exp: CExp::Atom(atom(init)),
      };
      CTail::Seq(assign, box cont)
    }
    ExpKind::Var(_) | ExpKind::Get(_) => {
      if init.ty == Type::Void {
        cont
      } else {
        let assign = CStmt::Assign {
          var,
          exp: CExp::Atom(atom(init)),
        };
        CTail::Seq(assign, box cont)
      }
    }
    ExpKind::Void => cont,
    ExpKind::Prim {
      op: (_, "vector-set!" | "copy-string!"),
      ..
    } => explicate_exp_effect(state, init, cont),
    // TODO this should be removed in chapter 9
    ExpKind::Prim {
      op: (_, op @ "vector-length"),
      args,
    } => match state.resolve_type(&args[0].ty) {
      Type::Tuple(fields) => {
        let assign = CStmt::Assign {
          var,
          exp: CExp::Atom(CAtom::Int(fields.len() as i64)),
        };
        CTail::Seq(assign, box cont)
      }
      Type::Array(_) => {
        let prim = prim(state, op, args);
        let assign = CStmt::Assign {
          var,
          exp: CExp::Prim(prim),
        };
        CTail::Seq(assign, box cont)
      }
      _ => unreachable!(),
    },
    ExpKind::Prim { op: (_, op), args } => {
      if init.ty == Type::Void {
        cont
      } else {
        let prim = prim(state, op, args);
        let assign = CStmt::Assign {
          var,
          exp: CExp::Prim(prim),
        };
        CTail::Seq(assign, box cont)
      }
    }
    ExpKind::Apply {
      fun,
      args,
      r#struct: _,
    } => {
      let assign = CStmt::Assign {
        var,
        exp: CExp::Call(atom(*fun), args.into_iter().map(atom).collect()),
      };
      CTail::Seq(assign, box cont)
    }
    ExpKind::Let {
      var: (_, var1),
      init: init1,
      body,
    } => {
      let cont = explicate_assign(state, var, *body, cont);
      explicate_assign(state, var1, *init1, cont)
    }
    ExpKind::If { cond, conseq, alt } => {
      let cont = state.tail_to_goto(cont);
      let conseq = explicate_assign(state, var.clone(), *conseq, cont.clone());
      let alt = explicate_assign(state, var, *alt, cont);
      explicate_pred(state, *cond, conseq, alt)
    }
    ExpKind::Begin { seq, last } => {
      let cont = explicate_assign(state, var, *last, cont);
      explicate_effect(state, seq.into_iter(), cont)
    }
    ExpKind::While { .. }
    | ExpKind::Print { .. }
    | ExpKind::NewLine
    | ExpKind::Set { .. } => explicate_exp_effect(state, init, cont),
    ExpKind::Error(err) => explicate_error(err),
    ExpKind::FunRef { name, arity } => {
      let assign = CStmt::Assign {
        var,
        exp: CExp::Atom(CAtom::FunRef(name, arity)),
      };
      CTail::Seq(assign, box cont)
    }
  }
}

fn explicate_pred(
  state: &mut State,
  cond: Exp<IdxVar, Type>,
  conseq: CTail,
  alt: CTail,
) -> CTail {
  match cond.kind {
    ExpKind::Bool(v) => {
      if v {
        conseq
      } else {
        alt
      }
    }
    ExpKind::Var(_) | ExpKind::Get(_) => {
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
    ExpKind::Let { var, init, body } => {
      let cont = explicate_pred(state, *body, conseq, alt);
      explicate_assign(state, var.1, *init, cont)
    }
    ExpKind::Prim {
      op: (_, "not"),
      mut args,
    } => explicate_pred(state, args.pop().unwrap(), alt, conseq),
    ExpKind::Prim { op, mut args } => {
      let conseq = state.tail_to_label(conseq);
      let alt = state.tail_to_label(alt);
      let cmp = op.1.parse().unwrap();
      let rhs = args.pop().unwrap();
      let lhs = args.pop().unwrap();
      CTail::If {
        cmp,
        lhs: atom(lhs),
        rhs: atom(rhs),
        conseq,
        alt,
      }
    }
    ExpKind::If {
      cond: cond1,
      conseq: conseq1,
      alt: alt1,
    } => {
      let conseq = state.tail_to_goto(conseq);
      let alt = state.tail_to_goto(alt);
      let conseq1 =
        explicate_pred(state, *conseq1, conseq.clone(), alt.clone());
      let alt1 = explicate_pred(state, *alt1, conseq, alt);
      explicate_pred(state, *cond1, conseq1, alt1)
    }
    ExpKind::Begin { seq, last } => {
      let cont = explicate_pred(state, *last, conseq, alt);
      explicate_effect(state, seq.into_iter(), cont)
    }
    ExpKind::Void
    | ExpKind::Int(_)
    | ExpKind::Str(_)
    | ExpKind::While { .. }
    | ExpKind::Print { .. }
    | ExpKind::NewLine
    | ExpKind::Set { .. } => {
      unreachable!()
    }
    ExpKind::Error(err) => explicate_error(err),
    ExpKind::Apply { .. } => unreachable!(),
    ExpKind::FunRef { .. } => unreachable!(),
  }
}

fn explicate_effect(
  state: &mut State,
  seq: impl DoubleEndedIterator<Item = Exp<IdxVar, Type>>,
  cont: CTail,
) -> CTail {
  seq.rfold(cont, |cont, exp| explicate_exp_effect(state, exp, cont))
}

fn explicate_exp_effect(
  state: &mut State,
  exp: Exp<IdxVar, Type>,
  cont: CTail,
) -> CTail {
  match exp.kind {
    ExpKind::Bool(_)
    | ExpKind::Int(_)
    | ExpKind::Var(_)
    | ExpKind::Str(_)
    | ExpKind::Void
    | ExpKind::Get(_) => cont,
    ExpKind::Prim {
      op: (_, "read"),
      args: _,
    } => CTail::Seq(CStmt::Read, box cont),
    ExpKind::Prim {
      op: (_, "vector-set!"),
      mut args,
    } => {
      let val = args.pop().unwrap();
      let index = args.pop().unwrap();
      let vec = args.pop().unwrap();
      match state.resolve_type(&vec.ty) {
        Type::Tuple(mut fields) => match index.kind {
          ExpKind::Int(index) => {
            if val.ty == Type::Void {
              cont
            } else {
              fields.truncate(index as usize);
              CTail::Seq(
                CStmt::TupSet {
                  tup: atom(vec),
                  fields_before: fields,
                  val: atom(val),
                },
                box cont,
              )
            }
          }
          _ => unreachable!(),
        },
        Type::Array(_) => {
          if val.ty == Type::Void {
            cont
          } else {
            CTail::Seq(
              CStmt::ArrSet {
                vec: atom(vec),
                index: atom(index),
                val: atom(val),
              },
              box cont,
            )
          }
        }
        Type::Alias(_) => unreachable!(),
        _ => unreachable!(),
      }
    }
    ExpKind::Prim {
      op: (_, "copy-string!"),
      mut args,
    } => {
      let src = args.pop().unwrap();
      let offset = args.pop().unwrap();
      let dest = args.pop().unwrap();
      CTail::Seq(
        CStmt::CopyStr {
          dest: atom(dest),
          src: atom(src),
          offset: atom(offset),
        },
        box cont,
      )
    }
    ExpKind::Prim { op: _, args } => {
      explicate_effect(state, args.into_iter(), cont)
    }
    ExpKind::Apply {
      fun,
      args,
      r#struct: _,
    } => CTail::Seq(
      CStmt::Call(atom(*fun), args.into_iter().map(atom).collect()),
      box cont,
    ),
    ExpKind::Let { var, init, body } => {
      let body = explicate_exp_effect(state, *body, cont);
      explicate_assign(state, var.1, *init, body)
    }
    ExpKind::If { cond, conseq, alt } => {
      let cont = state.tail_to_goto(cont);
      let conseq = explicate_exp_effect(state, *conseq, cont.clone());
      let alt = explicate_exp_effect(state, *alt, cont);
      explicate_pred(state, *cond, conseq, alt)
    }
    ExpKind::Set { var, exp } => explicate_assign(state, var.1, *exp, cont),
    ExpKind::Begin { seq, last } => {
      let cont = explicate_exp_effect(state, *last, cont);
      explicate_effect(state, seq.into_iter(), cont)
    }
    ExpKind::While { cond, body } => {
      let loop_start = state.new_label();
      let body = explicate_exp_effect(state, *body, CTail::Goto(loop_start));
      let block = explicate_pred(state, *cond, body, cont);
      state.add_label_block(loop_start, block);
      CTail::Goto(loop_start)
    }
    ExpKind::NewLine => CTail::Seq(CStmt::NewLine, box cont),
    ExpKind::Print(args) => args.into_iter().rfold(
      CTail::Seq(CStmt::NewLine, box cont),
      |cont, exp| match exp.ty {
        Type::Bool => CTail::Seq(CStmt::PrintBool(atom(exp)), box cont),
        Type::Int => CTail::Seq(CStmt::PrintInt(atom(exp)), box cont),
        Type::Str => CTail::Seq(CStmt::PrintStr(atom(exp)), box cont),
        _ => unreachable!(),
      },
    ),
    ExpKind::Error(err) => explicate_error(err),
    ExpKind::FunRef { .. } => cont,
  }
}

fn atom(exp: Exp<IdxVar, Type>) -> CAtom {
  match exp.kind {
    ExpKind::Int(n) => CAtom::Int(n),
    ExpKind::Var(var) | ExpKind::Get(var) => CAtom::Var(var),
    ExpKind::Bool(v) => CAtom::Bool(v),
    ExpKind::Str(s) => CAtom::Str(s),
    ExpKind::Void => CAtom::Void,
    ExpKind::FunRef { name, arity } => CAtom::FunRef(name, arity),
    _ => unreachable!("{:?}", exp),
  }
}

fn prim(state: &State, op: &str, mut args: Vec<Exp<IdxVar, Type>>) -> CPrim {
  match op {
    "read" => CPrim::Read,
    "+" => {
      let arg2 = args.pop().unwrap();
      let arg1 = args.pop().unwrap();
      CPrim::Add(atom(arg1), atom(arg2))
    }
    "-" => {
      if args.len() == 1 {
        let arg = args.pop().unwrap();
        CPrim::Neg(atom(arg))
      } else {
        let arg2 = args.pop().unwrap();
        let arg1 = args.pop().unwrap();
        CPrim::Sub(atom(arg1), atom(arg2))
      }
    }
    "*" => {
      let arg2 = args.pop().unwrap();
      let arg1 = args.pop().unwrap();
      CPrim::Mul(atom(arg1), atom(arg2))
    }
    "quotient" => {
      let arg2 = args.pop().unwrap();
      let arg1 = args.pop().unwrap();
      CPrim::Div(atom(arg1), atom(arg2))
    }
    "remainder" => {
      let arg2 = args.pop().unwrap();
      let arg1 = args.pop().unwrap();
      CPrim::Rem(atom(arg1), atom(arg2))
    }
    "not" => {
      let arg = args.pop().unwrap();
      CPrim::Not(atom(arg))
    }
    "eq?" | ">" | ">=" | "<" | "<=" => {
      let arg2 = args.pop().unwrap();
      let arg1 = args.pop().unwrap();
      CPrim::Cmp(op.parse().unwrap(), atom(arg1), atom(arg2))
    }
    "make-vector" => {
      let val = args.pop().unwrap();
      let ty = val.ty.clone();
      let len = args.pop().unwrap();
      CPrim::MakeArr {
        len: atom(len),
        val: atom(val),
        ty,
      }
    }
    "vector" => CPrim::Tuple(
      args
        .into_iter()
        .map(|arg| {
          let ty = arg.ty.clone();
          (atom(arg), ty)
        })
        .collect(),
    ),
    "vector-ref" => {
      let index = args.pop().unwrap();
      let vec = args.pop().unwrap();
      match state.resolve_type(&vec.ty) {
        Type::Tuple(fields) => match index.kind {
          ExpKind::Int(index) => CPrim::TupRef {
            tup: atom(vec),
            fields_before: fields[..index as usize].to_vec(),
          },
          _ => unreachable!(),
        },
        Type::Array(ty) => {
          assert_ne!(*ty, Type::Void);
          CPrim::ArrRef {
            vec: atom(vec),
            index: atom(index),
          }
        }
        _ => unreachable!(),
      }
    }
    "vector-length" => {
      let arg = args.pop().unwrap();
      match state.resolve_type(&arg.ty) {
        Type::Tuple(_) => CPrim::TupLen(atom(arg)),
        Type::Array(_) => CPrim::ArrLen(atom(arg)),
        _ => unreachable!(),
      }
    }
    "alloc-string" => {
      let arg = args.pop().unwrap();
      CPrim::AllocStr(atom(arg))
    }
    "string-length" => {
      let arg = args.pop().unwrap();
      CPrim::StrLen(atom(arg))
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
    let prog = typecheck::typecheck(prog).unwrap();
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
    let prog = typecheck::typecheck(prog).unwrap();
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
    let prog = typecheck::typecheck(prog).unwrap();
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
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn const_cond() {
    let prog = ast::parse(r#"(if (not #t) 1 2)"#).unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
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
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn var_cond() {
    let prog =
      ast::parse(r#"(let ([x (eq? (read) 20)]) (if x 42 89))"#).unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn shrink_and() {
    let prog =
      ast::parse(r#"(if (and (eq? (read) 0) (eq? (read) 2)) 0 42)"#).unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let result = explicate_control(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
