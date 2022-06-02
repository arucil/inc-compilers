use asm::{Arg, Block, Instr, Label, Program};
use ch4::pass::register_allocation::Info;

pub fn add_perilogue(prog: &mut Program<Info>) {
  add_prologue(prog);
  add_epilogue(prog);
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
  let mut code: Vec<Instr> = prog
    .info
    .used_callee_saved_regs
    .iter()
    .rev()
    .map(|&reg| Pop(Arg::Reg(reg)))
    .collect();
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
