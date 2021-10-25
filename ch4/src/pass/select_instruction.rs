use super::explicate_control::CInfo;
use asm::{Arg, Block, ByteReg, Instr, Label, Program, Reg};
use ast::IdxVar;
use ch2::pass::select_instruction::Info;
use control::*;
use indexmap::IndexMap;

pub fn select_instruction(prog: CProgram<CInfo>) -> Program<Info, IdxVar> {
  let mut constants = IndexMap::default();
  Program {
    info: Info {
      locals: prog.info.locals,
    },
    blocks: prog
      .body
      .into_iter()
      .map(|(label, tail)| (label, tail_block(tail, &mut constants)))
      .collect(),
    constants,
  }
}

fn tail_block(
  tail: CTail,
  constants: &mut IndexMap<String, String>,
) -> Block<IdxVar> {
  let mut code = vec![];
  tail_instructions(tail, &mut code, constants);
  Block {
    global: false,
    code,
  }
}

fn tail_instructions(
  mut tail: CTail,
  code: &mut Vec<Instr<IdxVar>>,
  constants: &mut IndexMap<String, String>,
) {
  loop {
    match tail {
      CTail::Return(exp) => {
        exp_instructions(Arg::Reg(Reg::Rax), exp, code);
        code.push(Instr::Jmp(Label::Conclusion));
        return;
      }
      CTail::Seq(stmt, new_tail) => {
        stmt_instructions(stmt, code, constants);
        tail = *new_tail;
      }
      CTail::Goto(label) => {
        code.push(Instr::Jmp(label));
        return;
      }
      CTail::If(cmp, arg1, arg2, conseq, alt) => {
        let arg1 = atom_to_arg(arg1);
        let arg2 = atom_to_arg(arg2);
        code.push(Instr::Cmp {
          dest: arg1,
          src: arg2,
        });
        code.push(Instr::JumpIf(cmp.into(), conseq));
        code.push(Instr::Jmp(alt));
        return;
      }
      _ => unimplemented!(),
    }
  }
}

fn stmt_instructions(
  stmt: CStmt,
  code: &mut Vec<Instr<IdxVar>>,
  constants: &mut IndexMap<String, String>,
) {
  match stmt {
    CStmt::Assign { var, exp } => exp_instructions(Arg::Var(var), exp, code),
    CStmt::Print {
      val,
      ty: CType::Int,
    } => {
      exp_instructions(Arg::Reg(Reg::Rax), val, code);
      code.push(Instr::Call("print_int".to_owned(), 0));
    }
    CStmt::Print {
      val,
      ty: CType::Bool,
    } => {
      exp_instructions(Arg::Reg(Reg::Rax), val, code);
      code.push(Instr::Call("print_bool".to_owned(), 0));
    }
    CStmt::Print {
      val,
      ty: CType::Str,
    } => match val {
      CExp::Atom(CAtom::Str(s)) => {
        let label = format!("const_{}", constants.len() + 1);
        let len = s.len();
        constants.insert(label.clone(), s);
        code.push(Instr::Mov {
          src: Arg::Label(label),
          dest: Arg::Reg(Reg::Rsi),
        });
        code.push(Instr::Mov {
          src: Arg::Imm(len as i64),
          dest: Arg::Reg(Reg::Rdx),
        });
        code.push(Instr::Call("print_str".to_owned(), 0));
      }
      _ => unreachable!(),
    },
    CStmt::NewLine => code.push(Instr::Call("print_newline".to_owned(), 0)),
    CStmt::Read => code.push(Instr::Call("read_int".to_owned(), 0)),
    _ => unimplemented!("{:?}", stmt),
  }
}

fn exp_instructions(
  target: Arg<IdxVar>,
  exp: CExp,
  code: &mut Vec<Instr<IdxVar>>,
) {
  match exp {
    CExp::Atom(atom) => atom_instructions(target, atom, code),
    CExp::Prim(prim) => prim_instructions(target, prim, code),
  }
}

fn prim_instructions(
  target: Arg<IdxVar>,
  prim: CPrim,
  code: &mut Vec<Instr<IdxVar>>,
) {
  match prim {
    CPrim::Read => {
      code.push(Instr::Call("read_int".to_owned(), 0));
      code.push(Instr::Mov {
        src: Arg::Reg(Reg::Rax),
        dest: target,
      });
    }
    CPrim::Neg(atom) => {
      atom_instructions(target.clone(), atom, code);
      code.push(Instr::Neg(target));
    }
    CPrim::Add(atom1, atom2) => {
      atom_instructions(target.clone(), atom1, code);
      let arg = atom_to_arg(atom2);
      code.push(Instr::Add {
        src: arg,
        dest: target,
      });
    }
    CPrim::Sub(atom1, atom2) => {
      atom_instructions(target.clone(), atom1, code);
      let arg = atom_to_arg(atom2);
      code.push(Instr::Sub {
        src: arg,
        dest: target,
      });
    }
    CPrim::Not(atom) => {
      atom_instructions(target.clone(), atom, code);
      code.push(Instr::Xor {
        src: Arg::Imm(1),
        dest: target,
      });
    }
    CPrim::Cmp(cmp, atom1, atom2) => {
      let arg1 = atom_to_arg(atom1);
      let arg2 = atom_to_arg(atom2);
      code.push(Instr::Cmp {
        dest: arg1,
        src: arg2,
      });
      code.push(Instr::SetIf(cmp.into(), Arg::ByteReg(ByteReg::Al)));
      code.push(Instr::Movzx {
        src: Arg::ByteReg(ByteReg::Al),
        dest: target,
      });
    }
    _ => unimplemented!(),
  }
}

fn atom_to_arg(atom: CAtom) -> Arg<IdxVar> {
  match atom {
    CAtom::Int(n) => Arg::Imm(n),
    CAtom::Var(var) => Arg::Var(var),
    CAtom::Bool(true) => Arg::Imm(1),
    CAtom::Bool(false) => Arg::Imm(0),
    _ => unimplemented!(),
  }
}

fn atom_instructions(
  target: Arg<IdxVar>,
  atom: CAtom,
  code: &mut Vec<Instr<IdxVar>>,
) {
  match atom {
    CAtom::Int(n) => {
      code.push(Instr::Mov {
        src: Arg::Imm(n),
        dest: target,
      });
    }
    CAtom::Var(var) => {
      code.push(Instr::Mov {
        src: Arg::Var(var),
        dest: target,
      });
    }
    CAtom::Bool(true) => {
      code.push(Instr::Mov {
        src: Arg::Imm(1),
        dest: target,
      });
    }
    CAtom::Bool(false) => {
      code.push(Instr::Mov {
        src: Arg::Imm(0),
        dest: target,
      });
    }
    _ => unimplemented!(),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let prog = super::super::explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
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
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let prog = super::super::explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
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
    let prog = super::super::explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn complex_if() {
    let prog = ast::parse(
      r#"
(let ([x (read)])
  (if (> x 100)
    (let ([y (read)])
      (if (or (and (>= x y) (< (- x y) 10))
              (and (< x y) (< (- y x) 10)))
        -1
        (- y)))
    (if (and (> x 40) (< x 60))
      5000
      x)))
      "#,
    )
    .unwrap();
    let prog = super::super::shrink::shrink(prog);
    let prog = super::super::uniquify::uniquify(prog);
    let prog = super::super::anf::anf(prog);
    let prog = super::super::explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
