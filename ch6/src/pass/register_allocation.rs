use asm::{Arg, Instr, Program, Reg};
use ast::{IdxVar, Type};
use ch3::location_set::{Var, VarStore};
use ch3::pass::register_allocation::{Color, RegisterAlloc};
use ch4::pass::interference::Info as OldInfo;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};
use id_arena::Arena;

pub struct Info {
  pub locals: IndexMap<IdxVar, Type>,
  /// in bytes
  pub stack_space: usize,
  /// in bytes
  pub rootstack_space: usize,
  pub used_callee_saved_regs: IndexSet<Reg>,
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}", self.locals)?;
    writeln!(
      f,
      "used_callee_saved_regs: {:?}",
      self.used_callee_saved_regs
    )?;
    writeln!(f, "stack_space: {} bytes", self.stack_space)?;
    writeln!(f, "rootstack_space: {} bytes", self.rootstack_space)
  }
}

pub fn allocate_registers(
  prog: Program<OldInfo, IdxVar>,
  available_regs: &[Reg],
) -> Program<Info> {
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
      num_locals: prog.info.locals.len(),
      conflicts: &prog.info.conflicts,
      moves: &prog.info.moves,
      reg_colors: &reg_colors,
      available_regs,
      used_callee_saved_regs: IndexSet::new(),
      assign_instr_registers: gen_assign_instr_registers(
        &prog.info.locals,
        &prog.types,
        &prog.info.var_store,
        available_regs,
        &mut num_prim_locals,
        &mut num_ref_locals,
      ),
    };

    blocks = prog
      .blocks
      .into_iter()
      .map(|(label, block)| {
        let block = alloc.allocate_block_registers(block);
        (label, block)
      })
      .collect();

    used_callee_saved_regs = alloc.used_callee_saved_regs;
  }

  Program {
    info: Info {
      locals: prog.info.locals,
      stack_space: num_prim_locals * 8,
      rootstack_space: num_ref_locals * 8,
      used_callee_saved_regs,
    },
    blocks,
    ..prog
  }
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
      Arg::Label(l) => Arg::Label(l),
    };

    match instr {
      Instr::Add { src, dest } => Instr::Add {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::Call { label, arity } => Instr::Call { label, arity },
      Instr::Jmp(label) => Instr::Jmp(label),
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
      Instr::Shl { src, count } => Instr::Shl {
        src: assign(src),
        count: assign(count),
      },
      Instr::Shr { src, count } => Instr::Shr {
        src: assign(src),
        count: assign(count),
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
    mov rsi, const_1
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
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
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
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
    mov rsi, const_1
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
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
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
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
    mov rsi, const_1
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
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
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let result = allocate_registers(prog, &[]);

    assert_snapshot!(result.to_string_pretty());
  }
}
