use self::location_set::LocationSet;
use asm::{Arg, Block, Instr, Program, Reg};
use maplit::hashmap;
use support::CompileError;

pub mod location_set;
pub mod pass;

pub fn compile(input: &str) -> Result<String, CompileError> {
  use Reg::*;
  let prog = ast::parse(input)?;
  let prog = ch2::pass::partial_evaluation::partial_evaluate(prog);
  let prog = ch2::pass::uniquify::uniquify(prog)?;
  let prog = ch2::pass::anf::anf(prog);
  let prog = ch2::pass::explicate_control::explicate_control(prog);
  let prog = ch2::pass::select_instruction::select_instruction(prog);
  let prog = self::pass::liveness_analysis::analyze_liveness(
    prog,
    hashmap! {
      "conclusion".to_owned() => {
        let mut set = LocationSet::new();
        set.add_reg(Rax);
        set.add_reg(Rbp);
        set
      }
    },
  );
  let prog = self::pass::interference::build_interference(prog);
  let prog = self::pass::register_allocation::allocate_registers(
    prog,
    &[
      Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15,
    ],
  );
  let mut prog = ch2::pass::patch_instructions::patch_instructions(prog);
  add_prologue(&mut prog);
  add_epilogue(&mut prog);

  Ok(prog.to_nasm())
}

fn add_prologue(prog: &mut Program<self::pass::register_allocation::Info>) {
  use asm::Reg::*;
  use Arg::*;
  use Instr::*;
  let stack_space =
    prog.info.stack_space + (prog.info.used_callee_saved_regs.len() & 15);
  let stack_space = (stack_space + 15) & !15;
  let mut code = vec![
    Push(Reg(Rbp)),
    Mov(Reg(Rsp), Reg(Rbp)),
    Sub(Imm(stack_space as i64), Reg(Rsp)),
  ];
  for &reg in &prog.info.used_callee_saved_regs {
    code.push(Push(Reg(reg)));
  }
  code.push(Jmp("start".to_owned()));
  let block = Block { code };
  prog.blocks.push(("_start".to_owned(), block));
}

fn add_epilogue<T>(prog: &mut Program<T>) {
  use asm::Reg::*;
  use Arg::*;
  use Instr::*;
  let block = Block {
    code: vec![
      Mov(Reg(Rbp), Reg(Rsp)),
      Pop(Reg(Rbp)),
      Call("print_int".to_owned(), 0),
      Call("print_newline".to_owned(), 0),
      Mov(Imm(60), Reg(Rax)),
      Mov(Imm(0), Reg(Rdi)),
      Syscall,
    ],
  };
  prog.blocks.push(("conclusion".to_owned(), block));
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog = compile(
      r#"
(let
  ([x (read)]
   [y (+ 2 3)])
  (+ (- (read)) (+ y (- 2))))
    "#,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn spilled() {
    let prog = compile(r#"
(let
  ([a (read)]
   [b (+ 2 3)]
   [c (- a)]
   [d 7]
   [e (+ (+ a c) b)]
   [f (- 12)]
   [g -50]
   [h 1]
   [i 0]
   [j 21]
   [k 77]
   [l 13]
   [m 13]
   [n 47]
   [o 1758]
   [p 1000])
  (+ (- a) (+ b (+ (+ c d) (+ e (+ (+ f (+ g (+ h (+ i (+ j (+ k (+ l (+ m (+ n o))))))))) p))))))
    "#).unwrap();
    assert_snapshot!(prog);
  }
}
