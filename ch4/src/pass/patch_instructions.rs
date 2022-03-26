use asm::{Arg, Block, Instr, Program, Reg};

pub fn patch_instructions<T>(prog: Program<T>) -> Program<T> {
  Program {
    info: prog.info,
    constants: prog.constants,
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
  use asm::{Label, Reg::*};
  use ast::*;
  use ch3::location_set::LocationSet;
  use insta::assert_snapshot;
  use maplit::hashmap;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = anf::anf(prog);
    let prog = explicate_control::explicate_control(prog);
    let prog = select_instruction::select_instruction(prog);
    let prog = liveness_analysis::analyze_liveness(
      prog,
      hashmap! {
        Label::Conclusion => LocationSet::regs([Rax, Rbp])
      },
    );
    let prog = interference::build_interference(prog);
    let prog = ch3::pass::move_biasing::build_move_graph(prog);
    let regs = &[
      Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15,
    ];
    let prog = ch3::pass::register_allocation::allocate_registers(prog, regs);
    let result = patch_instructions(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
