use super::control::{CExp, CProgram, CStmt, CTail, CAtom, CPrim};
use super::uniquify::IdxVar;
use asm::{Program, Block, Instr, Arg, Reg};
use std::iter;

pub fn select_instruction(prog: CProgram) -> Program<IdxVar> {
  Program {
    blocks: prog.body.into_iter()
      .map(|(label, tail)| (label, tail_block(tail)))
      .collect()
  }
}

fn tail_block(tail: CTail) -> Block<IdxVar> {
  let mut code = vec![];
  tail_instructions(tail, &mut code);
  Block {
    code,
  }
}

fn tail_instructions(
  mut tail: CTail,
  code: &mut Vec<Instr<IdxVar>>,
) {
  loop {
    match tail {
      CTail::Return(exp) => {
        exp_instructions(Arg::Reg(Reg::Rax), exp, code);
        code.push(Instr::Jmp("conclusion".to_owned()));
        return;
      }
      CTail::Seq(stmt, new_tail) => {
        stmt_instructions(stmt, code);
        tail = *new_tail;
      }
    }
  }
}

fn stmt_instructions(
  stmt: CStmt,
  code: &mut Vec<Instr<IdxVar>>,
) {
  match stmt {
    CStmt::Assign { var, exp } => exp_instructions(Arg::Var(var), exp, code),
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
      code.push(Instr::Callq("read_int".to_owned(), 0));
      code.push(Instr::Movq(Arg::Reg(Reg::Rax), target));
    }
    CExp::Prim(CPrim::Neg(atom)) => {
      atom_instructions(target.clone(), atom, code);
      code.push(Instr::Negq(target));
    }
    CExp::Prim(CPrim::Add(atom1, atom2)) => {
      atom_instructions(target.clone(), atom1, code);
      let arg = match atom2 {
        CAtom::Int(n) => Arg::Imm(n),
        CAtom::Var(var) => Arg::Var(var),
      };
      code.push(Instr::Addq(arg, target));
    }
  }
}

fn atom_instructions(
  target: Arg<IdxVar>,
  atom: CAtom,
  code: &mut Vec<Instr<IdxVar>>,
) {
  match atom {
    CAtom::Int(n) => {
      code.push(Instr::Movq(Arg::Imm(n), target));
    }
    CAtom::Var(var) => {
      code.push(Instr::Movq(Arg::Var(var), target));
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog = parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#).unwrap();
    let prog = super::super::uniquify::uniquify(prog).unwrap();
    let prog = super::super::anf::anf(prog);
    let prog = super::super::control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}