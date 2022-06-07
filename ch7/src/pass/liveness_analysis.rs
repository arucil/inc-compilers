use asm::{Fun, Label, Program};
use ast::IdxVar;
use ch3::location_set::LocationSet;
use ch4::pass::instruction_selection::Info as OldInfo;
use ch4::pass::liveness_analysis::Info as NewInfo;
use ch5::pass::liveness_analysis::analyze_body;
use std::collections::HashMap;

/// `toplevel_label_live` and `fun_label_live` are maps from labels to sets of
/// live locations before the first instruction of the blocks.
/// 
/// `fun_label_live` is the default live-before sets for all functions.
pub fn analyze_liveness(
  prog: Program<OldInfo, IdxVar>,
  toplevel_label_live: HashMap<Label, LocationSet>,
  fun_label_live: HashMap<Label, LocationSet>,
) -> Program<NewInfo, IdxVar> {
  let funs = prog
    .funs
    .into_iter()
    .map(|fun| Fun {
      info: analyze_body(
        fun.info,
        &fun.blocks,
        fun_label_live.clone(),
      ),
      ..fun
    })
    .collect();
  let info = analyze_body(prog.info, &prog.blocks, toplevel_label_live);
  Program { info, funs, ..prog }
}
