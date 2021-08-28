use asm::{Arg, Block, Instr, Program, Reg};

pub fn patch_instructions<T>(prog: Program<T>) -> Program<T> {
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
      Instr::Add {
        src: src @ Arg::Deref(..),
        dest: dest @ Arg::Deref(..),
      } => {
        code.push(Instr::Mov {
          src,
          dest: Arg::Reg(Reg::Rax),
        });
        code.push(Instr::Add {
          src: Arg::Reg(Reg::Rax),
          dest,
        });
      }
      Instr::Mov {
        src: src @ Arg::Deref(..),
        dest: dest @ Arg::Deref(..),
      } => {
        if src != dest {
          code.push(Instr::Mov {
            src,
            dest: Arg::Reg(Reg::Rax),
          });
          code.push(Instr::Mov {
            src: Arg::Reg(Reg::Rax),
            dest,
          });
        }
      }
      Instr::Mov {
        src: Arg::Reg(ref src),
        dest: Arg::Reg(ref dest),
      } => {
        if src != dest {
          code.push(instr);
        }
      }
      instr => {
        code.push(instr);
      }
    }
  }
  Block {
    global: block.global,
    code,
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
    let prog = super::super::anf::anf(prog);
    let prog = super::super::explicate_control::explicate_control(prog);
    let prog = super::super::select_instruction::select_instruction(prog);
    let prog = super::super::assign_home::assign_home(prog);
    let result = patch_instructions(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
