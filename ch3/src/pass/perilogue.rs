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
  code.push(Jmp(Arg::Label(Label::Start)));
  let block = Block { global: true, code };
  prog.blocks.push((Label::EntryPoint, block));
}

fn add_epilogue(prog: &mut Program<Info>) {
  use asm::Reg::*;
  use Instr::*;
  let label1 = "rt_print_int".to_owned();
  let label2 = "rt_print_newline".to_owned();
  prog.externs.insert(label1.clone());
  prog.externs.insert(label2.clone());
  let mut code = vec![
    Mov {
      src: Arg::Reg(Rax),
      dest: Arg::Reg(Rdi),
    },
    Call {
      label: label1,
      arity: 1,
      gc: false,
    },
    Call {
      label: label2,
      arity: 0,
      gc: false,
    },
  ];
  for &reg in prog.info.used_callee_saved_regs.iter().rev() {
    code.push(Pop(Arg::Reg(reg)));
  }
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
