use super::assign::Info;
use asm::{Arg, Block, Instr, Program, Reg};

pub fn patch_instructions(prog: Program<Info>) -> Program<Info> {
  Program {
    info: prog.info,
    blocks: prog
      .blocks
      .into_iter()
      .map(|(label, block)| (label, patch_block(block)))
      .collect(),
  }
}

fn patch_block(block: Block) -> Block {
  let mut code = vec![];
  for instr in block.code {
    match instr {
      Instr::Addq(src @ Arg::Deref(..), dest @ Arg::Deref(..)) => {
        code.push(Instr::Movq(src, Arg::Reg(Reg::Rax)));
        code.push(Instr::Addq(Arg::Reg(Reg::Rax), dest));
      }
      Instr::Movq(src @ Arg::Deref(..), dest @ Arg::Deref(..)) => {
        code.push(Instr::Movq(src, Arg::Reg(Reg::Rax)));
        code.push(Instr::Movq(Arg::Reg(Reg::Rax), dest));
      }
      instr => {
        code.push(instr);
      }
    }
  }
  Block { code }
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
    let prog = super::super::assign::assign_home(prog);
    let result = patch_instructions(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
