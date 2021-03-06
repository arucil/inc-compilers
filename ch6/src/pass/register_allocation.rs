use asm::{Arg, Block, Fun, Instr, LabelOrArg, Program, Reg};
use ast::{IdxVar, Type};
use ch3::location_set::{Var, VarStore};
use ch3::pass::register_allocation::{Color, RegisterAlloc};
use ch4::pass::interference::Info as OldInfo;
use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

pub struct Info {
  pub locals: IndexMap<IdxVar, Type>,
  /// in bytes
  pub stack_space: usize,
  /// in bytes
  pub rootstack_space: usize,
  pub used_callee_saved_regs: IndexSet<Reg>,
}

impl Display for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}", self.locals)?;
    write!(f, "used_callee_saved_regs: ",)?;
    let mut comma = true;
    for reg in &self.used_callee_saved_regs {
      if comma {
        write!(f, ", ")?;
      }
      comma = true;
      write!(f, "{}", reg)?;
    }
    writeln!(f, "\nstack_space: {} bytes", self.stack_space)?;
    writeln!(f, "rootstack_space: {} bytes", self.rootstack_space)
  }
}

pub fn allocate_registers(
  prog: Program<OldInfo, IdxVar>,
  available_regs: &[Reg],
) -> Program<Info> {
  let types = &prog.types;
  let (info, blocks) =
    alloc_body_regs(prog.info, prog.blocks, types, available_regs);
  let funs = prog
    .funs
    .into_iter()
    .map(|fun| {
      let (info, blocks) =
        alloc_body_regs(fun.info, fun.blocks, types, available_regs);
      Fun {
        name: fun.name,
        info,
        blocks,
      }
    })
    .collect();
  Program {
    info,
    blocks,
    funs,
    ..prog
  }
}

fn alloc_body_regs(
  info: OldInfo,
  body: Vec<Block<IdxVar>>,
  types: &Arena<Type>,
  available_regs: &[Reg],
) -> (Info, Vec<Block>) {
  let mut num_prim_locals = 0;
  let mut num_ref_locals = 0;
  let used_callee_saved_regs;
  let blocks;

  {
    let reg_colors: HashMap<_, _> = available_regs
      .iter()
      .enumerate()
      .map(|(i, &reg)| (reg, i as _))
      .collect();

    let mut alloc = RegisterAlloc {
      num_locals: info.locals.len(),
      conflicts: &info.conflicts,
      moves: &info.moves,
      reg_colors: &reg_colors,
      available_regs,
      used_callee_saved_regs: IndexSet::new(),
      assign_instr_registers: gen_assign_instr_registers(
        &info.locals,
        types,
        &info.var_store,
        available_regs,
        &mut num_prim_locals,
        &mut num_ref_locals,
      ),
    };

    blocks = body
      .into_iter()
      .map(|block| alloc.allocate_block_registers(block))
      .collect();

    used_callee_saved_regs = alloc.used_callee_saved_regs;
  }

  (
    Info {
      locals: info.locals,
      stack_space: num_prim_locals * 8,
      rootstack_space: num_ref_locals * 8,
      used_callee_saved_regs,
    },
    blocks,
  )
}

pub fn gen_assign_instr_registers<'a>(
  locals: &'a IndexMap<IdxVar, Type>,
  types: &'a Arena<Type>,
  var_store: &'a VarStore,
  available_regs: &'a [Reg],
  num_prim_locals: &'a mut usize,
  num_ref_locals: &'a mut usize,
) -> impl (FnMut(Instr<IdxVar>, &IndexMap<Var, Color>) -> Instr) + 'a {
  let mut prim_var_indices = vec![-1; locals.len()];
  let mut prim_var_counter = 0;
  let mut ref_var_indices = vec![-1; locals.len()];
  let mut ref_var_counter = 0;

  move |instr, var_colors| {
    let mut assign = |arg: Arg<IdxVar>| match arg {
      Arg::Deref(reg, i) => Arg::Deref(reg, i),
      Arg::Reg(reg) => Arg::Reg(reg),
      Arg::Imm(i) => Arg::Imm(i),
      Arg::Var(var) => {
        let is_ref = locals[&var].is_ref(types);
        let i = var_colors[&var_store.get(var)].index();
        if let Some(&reg) = available_regs.get(i) {
          Arg::Reg(reg)
        } else {
          let i = i - available_regs.len();
          if is_ref {
            if ref_var_indices[i] < 0 {
              ref_var_counter += 1;
              ref_var_indices[i] = ref_var_counter;
              *num_ref_locals += 1;
            }
            Arg::Deref(Reg::R15, -8 * ref_var_indices[i])
          } else {
            if prim_var_indices[i] < 0 {
              prim_var_counter += 1;
              prim_var_indices[i] = prim_var_counter;
              *num_prim_locals += 1;
            }
            Arg::Deref(Reg::Rbp, -8 * prim_var_indices[i])
          }
        }
      }
      Arg::ByteReg(reg) => Arg::ByteReg(reg),
    };

    match instr {
      Instr::Add { src, dest } => Instr::Add {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::Call {
        label: LabelOrArg::Label(label),
        arity,
        gc,
      } => Instr::Call {
        label: LabelOrArg::Label(label),
        arity,
        gc,
      },
      Instr::Call {
        label: LabelOrArg::Arg(arg),
        arity,
        gc,
      } => Instr::Call {
        label: LabelOrArg::Arg(assign(arg)),
        arity,
        gc,
      },
      Instr::Jmp(arg) => unreachable!("jmp {}", arg),
      Instr::LocalJmp(label) => Instr::LocalJmp(label),
      Instr::TailJmp {
        label: LabelOrArg::Label(label),
        arity,
      } => Instr::TailJmp {
        label: LabelOrArg::Label(label),
        arity,
      },
      Instr::TailJmp {
        label: LabelOrArg::Arg(arg),
        arity,
      } => Instr::TailJmp {
        label: LabelOrArg::Arg(assign(arg)),
        arity,
      },
      Instr::Mov { src, dest } => Instr::Mov {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::Neg(dest) => Instr::Neg(assign(dest)),
      Instr::Pop(dest) => Instr::Pop(assign(dest)),
      Instr::Push(src) => Instr::Push(assign(src)),
      Instr::Ret => Instr::Ret,
      Instr::Sub { src, dest } => Instr::Sub {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::Syscall => Instr::Syscall,
      Instr::Cmp { src, dest } => Instr::Cmp {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::JumpIf { cmp, label } => Instr::JumpIf { cmp, label },
      Instr::Movzx { src, dest } => Instr::Movzx {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::SetIf { cmp, dest } => Instr::SetIf {
        cmp,
        dest: assign(dest),
      },
      Instr::Xor { src, dest } => Instr::Xor {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::And { src, dest } => Instr::And {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::Or { src, dest } => Instr::Or {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::Shl { dest, count } => Instr::Shl {
        dest: assign(dest),
        count: assign(count),
      },
      Instr::Shr { dest, count } => Instr::Shr {
        dest: assign(dest),
        count: assign(count),
      },
      Instr::IMul { src, dest } => Instr::IMul {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::IMul3 { src, num, dest } => Instr::IMul3 {
        src: assign(src),
        num,
        dest: assign(dest),
      },
      Instr::IDiv(arg) => Instr::IDiv(assign(arg)),
      Instr::Lea { label, dest } => Instr::Lea {
        label,
        dest: assign(dest),
      },
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use asm::Label;
  use ch3::location_set::LocationSet;
  use ch4::pass::instruction_selection::Info as OldOldInfo;
  use ch4::pass::interference;
  use ch4::pass::liveness_analysis;
  use indexmap::indexmap;
  use insta::assert_snapshot;
  use maplit::hashmap;

  #[test]
  fn vector() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
start:
    mov rdi, 8
    mov rsi, r15
    call rt_allocate
    mov [r11], rax
    mov [r11], 1
    mov tmp.1, r11
    mov rdi, 3
    lea rsi, const_1
    mov rdx, r15
    call rt_new_string
    mov tmp.2, rax
    mov rdi, 40
    mov rsi, r15
    call rt_allocate
    mov [r11], rax
    mov [r11], 6177
    mov [r11 + 8], 1
    mov [r11 + 16], 0
    mov [r11 + 24], tmp.1
    mov [r11 + 32], tmp.2
    mov y.1, r11
    mov r11, y.1
    mov tmp.3, [r11 + 16]
    mov tmp.4, tmp.3
    xor tmp.4, 1
    mov r11, y.1
    mov [r11 + 16], tmp.4
    mov tmp.5, 5
    mov r11, y.1
    mov [r11 + 8], tmp.5
    mov r11, y.1
    mov tmp.6, [r11 + 24]
    jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("tmp.1") => Type::Tuple(vec![Type::Void]),
          IdxVar::new("tmp.2") => Type::Str,
          IdxVar::new("y.1") => Type::Tuple(vec![Type::Int, Type::Bool, Type::Void, Type::Tuple(vec![Type::Void]), Type::Str]),
          IdxVar::new("tmp.3") => Type::Bool,
          IdxVar::new("tmp.4") => Type::Bool,
          IdxVar::new("tmp.5") => Type::Int,
          IdxVar::new("tmp.6") => Type::Tuple(vec![Type::Void]),
        },
      },
      blocks,
      ..Program::default()
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rcx]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn vector_r11() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
start:
    mov rdi, 8
    mov rsi, r15
    call rt_allocate
    mov [r11], rax
    mov [r11], 1
    mov tmp.1, r11
    mov rdi, 3
    lea rsi, const_1
    mov rdx, r15
    call rt_new_string
    mov tmp.2, rax
    mov rdi, 40
    mov rsi, r15
    call rt_allocate
    mov [r11], rax
    mov [r11], 6177
    mov [r11 + 8], 1
    mov [r11 + 16], 0
    mov [r11 + 24], tmp.1
    mov [r11 + 32], tmp.2
    mov y.1, r11
    mov r11, y.1
    mov tmp.3, [r11 + 16]
    mov tmp.4, tmp.3
    xor tmp.4, 1
    mov r11, y.1
    mov [r11 + 16], tmp.4
    mov tmp.5, 5
    mov r11, y.1
    mov [r11 + 8], tmp.5
    jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("tmp.1") => Type::Tuple(vec![Type::Void]),
          IdxVar::new("tmp.2") => Type::Str,
          IdxVar::new("y.1") => Type::Tuple(vec![Type::Int, Type::Bool, Type::Void, Type::Tuple(vec![Type::Void]), Type::Str]),
          IdxVar::new("tmp.3") => Type::Bool,
          IdxVar::new("tmp.4") => Type::Bool,
          IdxVar::new("tmp.5") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let result = allocate_registers(prog, &[R11]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn vector_spill() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
start:
    mov rdi, 8
    mov rsi, r15
    call rt_allocate
    mov [r11], rax
    mov [r11], 1
    mov tmp.1, r11
    mov rdi, 3
    lea rsi, const_1
    mov rdx, r15
    call rt_new_string
    mov tmp.2, rax
    mov rdi, 40
    mov rsi, r15
    call rt_allocate
    mov [r11], rax
    mov [r11], 6177
    mov [r11 + 8], 1
    mov [r11 + 16], 0
    mov [r11 + 24], tmp.1
    mov [r11 + 32], tmp.2
    mov y.1, r11
    mov r11, y.1
    mov tmp.3, [r11 + 16]
    mov tmp.4, tmp.3
    xor tmp.4, 1
    mov r11, y.1
    mov [r11 + 16], tmp.4
    mov tmp.5, 5
    mov r11, y.1
    mov [r11 + 8], tmp.5
    jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("tmp.1") => Type::Tuple(vec![Type::Void]),
          IdxVar::new("tmp.2") => Type::Str,
          IdxVar::new("y.1") => Type::Tuple(vec![Type::Int, Type::Bool, Type::Void, Type::Tuple(vec![Type::Void]), Type::Str]),
          IdxVar::new("tmp.3") => Type::Bool,
          IdxVar::new("tmp.4") => Type::Bool,
          IdxVar::new("tmp.5") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let result = allocate_registers(prog, &[]);

    assert_snapshot!(result.to_string_pretty());
  }
}
