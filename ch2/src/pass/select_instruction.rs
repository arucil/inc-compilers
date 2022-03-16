use super::explicate_control::CInfo;
use asm::{Arg, Block, Instr, Label, Program, Reg};
use ast::IdxVar;
use control::*;
use indexmap::IndexSet;
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}\n", self.locals)
  }
}

pub fn select_instruction(prog: CProgram<CInfo>) -> Program<Info, IdxVar> {
  Program {
    info: Info {
      locals: prog.info.locals,
    },
    constants: Default::default(),
    blocks: prog
      .body
      .into_iter()
      .map(|(label, tail)| (label, tail_block(tail)))
      .collect(),
  }
}

fn tail_block(tail: CTail) -> Block<IdxVar> {
  let mut code = vec![];
  tail_instructions(tail, &mut code);
  Block {
    global: false,
    code,
  }
}

fn tail_instructions(mut tail: CTail, code: &mut Vec<Instr<IdxVar>>) {
  loop {
    match tail {
      CTail::Return(exp) => {
        exp_instructions(Arg::Reg(Reg::Rax), exp, code);
        code.push(Instr::Jmp(Label::Conclusion));
        return;
      }
      CTail::Seq(stmt, new_tail) => {
        stmt_instructions(stmt, code);
        tail = *new_tail;
      }
      _ => unimplemented!(),
    }
  }
}

fn stmt_instructions(stmt: CStmt, code: &mut Vec<Instr<IdxVar>>) {
  match stmt {
    CStmt::Assign { var, exp } => exp_instructions(Arg::Var(var), exp, code),
    _ => unimplemented!(),
  }
}

fn exp_instructions(
  target: Arg<IdxVar>,
  exp: CExp,
  code: &mut Vec<Instr<IdxVar>>,
) {
  match exp {
    CExp::Atom(atom) => {
      atom_instructions(target, atom, code);
    }
    CExp::Prim(CPrim::Read) => {
      code.push(Instr::Call {
        label: "read_int".to_owned(),
        arity: 0,
      });
      code.push(Instr::Mov {
        src: Arg::Reg(Reg::Rax),
        dest: target,
      });
    }
    CExp::Prim(CPrim::Neg(atom)) => {
      atom_instructions(target.clone(), atom, code);
      code.push(Instr::Neg(target));
    }
    CExp::Prim(CPrim::Add(atom1, atom2)) => {
      atom_instructions(target.clone(), atom1, code);
      let arg = match atom2 {
        CAtom::Int(n) => Arg::Imm(n),
        CAtom::Var(var) => Arg::Var(var),
        _ => unimplemented!(),
      };
      code.push(Instr::Add {
        src: arg,
        dest: target,
      });
    }
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
    let prog = super::super::uniquify::uniquify(prog).unwrap();
    let prog =
      super::super::remove_complex_operands::remove_complex_operands(prog);
    let prog = super::super::explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
