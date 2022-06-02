use super::assign_home::Info;
use asm::{Arg, Block, Instr, Label, Program};

pub fn add_perilogue(mut prog: Program<Info>) -> Program<Info> {
  add_prologue(&mut prog);
  add_epilogue(&mut prog);
  prog
}

fn add_prologue(prog: &mut Program<Info>) {
  use asm::Reg::*;
  use Instr::*;
  let stack_space = (prog.info.stack_space + 15) & !15;
  let block = Block {
    global: true,
    code: vec![
      Push(Arg::Reg(Rbp)),
      Mov {
        src: Arg::Reg(Rsp),
        dest: Arg::Reg(Rbp),
      },
      Sub {
        src: Arg::Imm(stack_space as i64),
        dest: Arg::Reg(Rsp),
      },
      Jmp(Arg::Label(Label::Start)),
    ],
  };
  prog.blocks.push((Label::EntryPoint, block));
}

fn add_epilogue(prog: &mut Program<Info>) {
  use asm::Reg::*;
  use Instr::*;
  let label1 = "rt_print_int".to_owned();
  let label2 = "rt_print_newline".to_owned();
  prog.externs.insert(label1.clone());
  prog.externs.insert(label2.clone());
  let block = Block {
    global: false,
    code: vec![
      Mov {
        src: Arg::Reg(Rbp),
        dest: Arg::Reg(Rsp),
      },
      Pop(Arg::Reg(Rbp)),
      Mov {
        src: Arg::Reg(Rax),
        dest: Arg::Reg(Rdi),
      },
      Call {
        label: label1,
        arity: 0,
        gc: false,
      },
      Call {
        label: label2,
        arity: 0,
        gc: false,
      },
      Mov {
        src: Arg::Imm(60),
        dest: Arg::Reg(Rax),
      },
      Mov {
        src: Arg::Imm(0),
        dest: Arg::Reg(Rdi),
      },
      Syscall,
    ],
  };
  prog.blocks.push((Label::Conclusion, block));
}
