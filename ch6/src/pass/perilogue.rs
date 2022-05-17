use super::register_allocation::Info;
use asm::{Arg, Block, Instr, Label, Program};

pub fn add_perilogue(mut prog: Program<Info>) -> Program<Info> {
  add_prologue(&mut prog);
  add_epilogue(&mut prog);
  prog
}

fn add_prologue(prog: &mut Program<Info>) {
  use asm::Reg::*;
  use Instr::*;
  let stack_space =
    prog.info.stack_space + prog.info.used_callee_saved_regs.len() * 8;
  let stack_space =
    ((stack_space + 15) & !15) - prog.info.used_callee_saved_regs.len() * 8;
  let mut code = vec![
    Push(Arg::Reg(Rbp)),
    Mov {
      src: Arg::Reg(Rsp),
      dest: Arg::Reg(Rbp),
    },
    Sub {
      src: Arg::Imm(stack_space as i64),
      dest: Arg::Reg(Rsp),
    },
  ];
  for &reg in &prog.info.used_callee_saved_regs {
    code.push(Push(Arg::Reg(reg)));
  }
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
      label: "rt_initialize".to_owned(),
      arity: 2,
    },
    Mov {
      src: Arg::Label("rt_rootstack_begin".to_owned()),
      dest: Arg::Reg(R15),
    },
  ]);
  for i in (0..prog.info.rootstack_space as i32).step_by(8) {
    code.push(Mov {
      src: Arg::Imm(0),
      dest: Arg::Deref(R15, i),
    });
  }
  code.extend_from_slice(&[
    Add {
      src: Arg::Imm(prog.info.rootstack_space as i64),
      dest: Arg::Reg(R15),
    },
    Jmp(Label::Start),
  ]);
  let block = Block { global: true, code };
  prog.blocks.push((Label::EntryPoint, block));
}

fn add_epilogue(prog: &mut Program<Info>) {
  use asm::Reg::*;
  use Instr::*;
  let mut code: Vec<Instr> = vec![Sub {
    src: Arg::Imm(prog.info.rootstack_space as i64),
    dest: Arg::Reg(R15),
  }];
  code.extend(
    prog
      .info
      .used_callee_saved_regs
      .iter()
      .rev()
      .map(|&reg| Pop(Arg::Reg(reg))),
  );
  code.extend_from_slice(&[
    Mov {
      src: Arg::Reg(Rbp),
      dest: Arg::Reg(Rsp),
    },
    Pop(Arg::Reg(Rbp)),
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
  let block = Block {
    global: false,
    code,
  };
  prog.blocks.push((Label::Conclusion, block));
}
