use asm::{Arg, Block, Instr, Program, Reg};
use ast::IdxVar;
use indexmap::IndexSet;
use std::collections::HashMap;
use std::fmt::{self, Write};
use support::WritePretty;

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  // in bytes
  pub stack_space: usize,
}

impl WritePretty for Info {
  fn write(&self, f: &mut impl Write) -> fmt::Result {
    writeln!(f, "locals: {:?}", self.locals)?;
    writeln!(f, "stack_space: {} bytes", self.stack_space)
  }
}

pub fn assign_home(prog: Program<super::instruction::Info, IdxVar>) -> Program<Info> {
  let mut local_spaces = HashMap::new();
  let blocks = prog
    .blocks
    .into_iter()
    .map(|(label, block)| (label, assign_home_block(block, &mut local_spaces)))
    .collect();
  Program {
    info: Info {
      locals: prog.info.locals,
      stack_space: local_spaces.len() * 8,
    },
    blocks,
  }
}

fn assign_home_block(
  block: Block<IdxVar>,
  local_spaces: &mut HashMap<IdxVar, usize>,
) -> Block {
  let code = block
    .code
    .into_iter()
    .map(|ins| assign_home_instr(ins, local_spaces))
    .collect();
  Block { code }
}

fn assign_home_instr(
  instr: Instr<IdxVar>,
  local_spaces: &mut HashMap<IdxVar, usize>,
) -> Instr {
  let mut binary = |arg1, arg2, op: fn(_, _) -> _| -> Instr {
    let arg1 = assign_home_arg(arg1, local_spaces);
    let arg2 = assign_home_arg(arg2, local_spaces);
    op(arg1, arg2)
  };

  match instr {
    Instr::Addq(src, dest) => binary(src, dest, Instr::Addq),
    Instr::Callq(label, n) => Instr::Callq(label, n),
    Instr::Jmp(label) => Instr::Jmp(label),
    Instr::Movq(src, dest) => binary(src, dest, Instr::Movq),
    Instr::Negq(dest) => Instr::Negq(assign_home_arg(dest, local_spaces)),
    Instr::Popq(dest) => Instr::Popq(assign_home_arg(dest, local_spaces)),
    Instr::Pushq(src) => Instr::Pushq(assign_home_arg(src, local_spaces)),
    Instr::Retq => Instr::Retq,
    instr => unimplemented!("{:?}", instr),
  }
}

fn assign_home_arg(arg: Arg<IdxVar>, local_spaces: &mut HashMap<IdxVar, usize>) -> Arg {
  match arg {
    Arg::Imm(n) => Arg::Imm(n),
    Arg::Var(var) => {
      if let Some(&i) = local_spaces.get(&var) {
        return Arg::Deref(Reg::Rbp, -(i as i32));
      }
      let i = (local_spaces.len() + 1) * 8;
      local_spaces.insert(var, i);
      Arg::Deref(Reg::Rbp, -(i as i32))
    }
    Arg::Reg(r) => Arg::Reg(r),
    Arg::Deref(r, i) => Arg::Deref(r, i),
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
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#).unwrap();
    let prog = super::super::uniquify::uniquify(prog).unwrap();
    let prog = super::super::anf::anf(prog);
    let prog = super::super::control::explicate_control(prog);
    let prog = super::super::instruction::select_instruction(prog);
    let result = assign_home(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
