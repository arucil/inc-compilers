use asm::{Arg, Block, Instr, Program, Reg};
use ast::IdxVar;
use indexmap::IndexSet;
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  /// in bytes
  pub stack_space: usize,
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}", self.locals)?;
    writeln!(f, "stack_space: {} bytes", self.stack_space)
  }
}

pub fn assign_home(
  prog: Program<super::instruction_selection::Info, IdxVar>,
) -> Program<Info> {
  let local_spaces = prog
    .info
    .locals
    .iter()
    .enumerate()
    .map(|(i, var)| (var.clone(), (i + 1) * 8))
    .collect();
  let blocks = prog
    .blocks
    .into_iter()
    .map(|block| assign_home_block(block, &local_spaces))
    .collect();
  Program {
    info: Info {
      locals: prog.info.locals,
      stack_space: local_spaces.len() * 8,
    },
    externs: prog.externs,
    constants: Default::default(),
    funs: vec![],
    blocks,
    types: prog.types,
  }
}

fn assign_home_block(
  block: Block<IdxVar>,
  local_spaces: &HashMap<IdxVar, usize>,
) -> Block {
  let code = block
    .code
    .into_iter()
    .map(|ins| assign_home_instr(ins, local_spaces))
    .collect();
  Block { code, ..block }
}

fn assign_home_instr(
  instr: Instr<IdxVar>,
  local_spaces: &HashMap<IdxVar, usize>,
) -> Instr {
  match instr {
    Instr::Add { src, dest } => {
      let src = assign_home_arg(src, local_spaces);
      let dest = assign_home_arg(dest, local_spaces);
      Instr::Add { src, dest }
    }
    Instr::Call { label, arity, gc } => Instr::Call { label, arity, gc },
    Instr::Jmp(arg) => {
      let arg = assign_home_arg(arg, local_spaces);
      Instr::Jmp(arg)
    }
    Instr::Mov { src, dest } => {
      let src = assign_home_arg(src, local_spaces);
      let dest = assign_home_arg(dest, local_spaces);
      Instr::Mov { src, dest }
    }
    Instr::Neg(dest) => Instr::Neg(assign_home_arg(dest, local_spaces)),
    Instr::Pop(dest) => Instr::Pop(assign_home_arg(dest, local_spaces)),
    Instr::Push(src) => Instr::Push(assign_home_arg(src, local_spaces)),
    Instr::Ret => Instr::Ret,
    instr => unimplemented!("{:?}", instr),
  }
}

fn assign_home_arg(
  arg: Arg<IdxVar>,
  local_spaces: &HashMap<IdxVar, usize>,
) -> Arg {
  match arg {
    Arg::Imm(n) => Arg::Imm(n),
    Arg::Var(var) => Arg::Deref(Reg::Rbp, -(local_spaces[&var] as i32)),
    Arg::Reg(r) => Arg::Reg(r),
    Arg::Deref(r, i) => Arg::Deref(r, i),
    Arg::ByteReg(_) => unimplemented!(),
    Arg::Label(label) => Arg::Label(label),
  }
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let prog = instruction_selection::select_instruction(prog, false);
    let result = assign_home(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
