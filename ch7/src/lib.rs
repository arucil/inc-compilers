#![allow(incomplete_features)]
#![feature(box_syntax, type_changing_struct_update)]

use asm::{Label, Reg};
use ch3::location_set::LocationSet;
use maplit::hashmap;
use support::CompileError;

pub mod pass;

pub fn compile(
  input: &str,
  regs: Option<&[Reg]>,
) -> Result<String, CompileError> {
  use Reg::*;
  let prog = ast::parse(input)?;
  let prog = ch5::pass::typecheck::typecheck(prog)?;
  let prog = ch6::pass::r#struct::desugar_struct(prog);
  let prog = ch4::pass::shrink::shrink(prog);
  let prog = ch4::pass::uniquify::uniquify(prog);
  let prog = ch5::pass::uncover_get::uncover_get(prog);
  let prog = ch6::pass::array_bounds::insert_bounds_check(prog);
  let prog = ch6::pass::division::insert_division_check(prog);
  let prog = ch6::pass::string::expose_string_concat(prog);
  let prog = ch2::pass::remove_complex_operands::remove_complex_operands(prog);
  let prog = ch4::pass::explicate_control::explicate_control(prog);
  let prog = ch4::pass::instruction_selection::select_instruction(prog, true);
  let prog = ch5::pass::liveness_analysis::analyze_liveness(
    prog,
    hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rbp])
    },
  );
  let prog = ch4::pass::interference::build_interference(prog);
  let prog = ch4::pass::move_biasing::build_move_graph(prog);
  let regs =
    regs.unwrap_or(&[Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14]);
  let prog = ch6::pass::register_allocation::allocate_registers(prog, regs);
  let prog = ch2::pass::patch_instructions::patch_instructions(prog);
  let prog = ch6::pass::perilogue::add_perilogue(prog);
  let prog = ch4::pass::merge_blocks::merge_blocks(prog);

  Ok(prog.to_nasm(false))
}