use super::register_allocation::Info;
use asm::{Arg, Block, Instr, Label, LabelOrArg, Program};

pub fn add_perilogue(mut prog: Program<Info>) -> Program<Info> {
  add_toplevel_prologue(&mut prog);
  add_toplevel_epilogue(&mut prog);
  for fun in &mut prog.funs {
    // merge_blocks will rearrange blocks, no need to put prologue first.
    fun.blocks.push(make_prologue(&fun.info, false));
    let mut epilogue = make_epilogue(&fun.info);
    epilogue.code.push(Instr::Ret);
    fun.blocks.push(epilogue);
  }
  prog
}

fn add_toplevel_prologue(prog: &mut Program<Info>) {
  let label_init = "rt_initialize".to_owned();
  prog.externs.insert(label_init);
  let mut block = make_prologue(&prog.info, true);
  block.label = Label::EntryPoint;
  prog.blocks.push(block);
}

fn calc_frame_size(info: &Info) -> usize {
  let frame_size = info.stack_space + info.used_callee_saved_regs.len() * 8;
  ((frame_size + 15) & !15) - info.used_callee_saved_regs.len() * 8
}

fn make_prologue(info: &Info, init_gc: bool) -> Block {
  use asm::Reg::*;
  use Instr::*;

  let mut code = vec![
    Push(Arg::Reg(Rbp)),
    Mov {
      src: Arg::Reg(Rsp),
      dest: Arg::Reg(Rbp),
    },
  ];
  let frame_size = calc_frame_size(info);
  if frame_size > 0 {
    code.push(Sub {
      src: Arg::Imm(frame_size as i64),
      dest: Arg::Reg(Rsp),
    })
  }
  for &reg in &info.used_callee_saved_regs {
    code.push(Push(Arg::Reg(reg)));
  }
  if init_gc {
    let label_init = "rt_initialize".to_owned();
    code.extend_from_slice(&[
      Mov {
        src: Arg::Imm(65536),
        dest: Arg::Reg(Rdi),
      },
      Mov {
        src: Arg::Imm(64),
        dest: Arg::Reg(Rsi),
      },
      Call {
        label: LabelOrArg::Label(label_init),
        arity: 2,
        gc: false,
      },
      Mov {
        src: Arg::Reg(Rax),
        dest: Arg::Reg(R15),
      },
    ]);
  }
  if info.rootstack_space > 0 {
    for i in (0..info.rootstack_space as i32).step_by(8) {
      code.push(Mov {
        src: Arg::Imm(0),
        dest: Arg::Deref(R15, i),
      });
    }
    code.push(Add {
      src: Arg::Imm(info.rootstack_space as i64),
      dest: Arg::Reg(R15),
    });
  }
  code.push(LocalJmp(Label::Start));
  Block {
    label: Label::Prologue,
    code,
  }
}

fn add_toplevel_epilogue(prog: &mut Program<Info>) {
  use asm::Reg::*;
  use Instr::*;
  let mut block = make_epilogue(&prog.info);
  block.code.extend_from_slice(&[
    Mov {
      src: Arg::Imm(60),
      dest: Arg::Reg(Rax),
    },
    Mov {
      src: Arg::Imm(0),
      dest: Arg::Reg(Rdi),
    },
    Syscall,
  ]);
  prog.blocks.push(block);
}

/// Does not include RET.
pub fn make_epilogue(info: &Info) -> Block {
  use asm::Reg::*;
  use Instr::*;
  let mut code: Vec<Instr> = vec![];
  if info.rootstack_space > 0 {
    code.push(Sub {
      src: Arg::Imm(info.rootstack_space as i64),
      dest: Arg::Reg(R15),
    });
  }
  code.extend(
    info
      .used_callee_saved_regs
      .iter()
      .rev()
      .map(|&reg| Pop(Arg::Reg(reg))),
  );
  let frame_size = calc_frame_size(info);
  if frame_size > 0 {
    code.push(Mov {
      src: Arg::Reg(Rbp),
      dest: Arg::Reg(Rsp),
    });
  }
  code.push(Pop(Arg::Reg(Rbp)));
  Block {
    label: Label::Epilogue,
    code,
  }
}
