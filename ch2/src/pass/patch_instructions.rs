use asm::{Arg, Block, Instr, Program, Reg};

pub fn patch_instructions<T>(prog: Program<T>) -> Program<T> {
  Program {
    blocks: prog
      .blocks
      .into_iter()
      .map(|(label, block)| (label, patch_block(block)))
      .collect(),
    ..prog
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
      Instr::Sub {
        src: src @ Arg::Deref(..),
        dest: dest @ Arg::Deref(..),
      } => {
        code.push(Instr::Mov {
          src,
          dest: Arg::Reg(Reg::Rax),
        });
        code.push(Instr::Sub {
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
      Instr::Mov {
        src: Arg::Imm(0),
        dest: dest @ Arg::Reg(_),
      } => {
        code.push(Instr::Xor {
          src: dest.clone(),
          dest,
        });
      }
      // ch4
      Instr::Movzx { src, dest } if !matches!(dest, Arg::Reg(_)) => {
        code.push(Instr::Movzx {
          src,
          dest: Arg::Reg(Reg::Rax),
        });
        code.push(Instr::Mov {
          src: Arg::Reg(Reg::Rax),
          dest,
        })
      }
      Instr::Xor {
        src: src @ Arg::Deref(..),
        dest: dest @ Arg::Deref(..),
      } => {
        code.push(Instr::Mov {
          src,
          dest: Arg::Reg(Reg::Rax),
        });
        code.push(Instr::Xor {
          src: Arg::Reg(Reg::Rax),
          dest,
        });
      }
      Instr::Cmp {
        src: src @ Arg::Deref(..),
        dest: dest @ Arg::Deref(..),
      } => {
        code.push(Instr::Mov {
          src,
          dest: Arg::Reg(Reg::Rax),
        });
        code.push(Instr::Cmp {
          src: Arg::Reg(Reg::Rax),
          dest,
        });
      }
      Instr::Cmp {
        src,
        dest: dest @ Arg::Imm(_),
      } => {
        code.push(Instr::Mov {
          src: dest,
          dest: Arg::Reg(Reg::Rax),
        });
        code.push(Instr::Cmp {
          src,
          dest: Arg::Reg(Reg::Rax),
        });
      }
      _ => {
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
    let prog = assign_home::assign_home(prog);
    let result = patch_instructions(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
