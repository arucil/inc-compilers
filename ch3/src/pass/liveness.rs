use crate::location_set::{LocationSet, VarStore};
use asm::Program;
use ast::IdxVar;
use ch2::pass::instruction::Info as OldInfo;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  pub live: IndexMap<String, Vec<LocationSet>>,
  pub var_store: VarStore,
}

pub fn analyze_liveness(
  prog: Program<OldInfo, IdxVar>,
  label_live: HashMap<String, LocationSet>,
) -> Program<Info, IdxVar> {
  let mut var_store = VarStore::new();
  let live = prog
    .blocks
    .iter()
    .map(|(label, block)| {
      let live = block_liveness(block, &label_live, &mut var_store);
      (label.clone(), live)
    })
    .collect();

  Program {
    info: Info {
      locals: prog.info.locals,
      live,
      var_store,
    },
    blocks: prog.blocks,
  }
}

fn block_liveness(
  block: &asm::Block<IdxVar>,
  label_live: &HashMap<String, LocationSet>,
  var_store: &mut VarStore,
) -> Vec<LocationSet> {
  let mut set = vec![LocationSet::new(); block.code.len() + 1];
  for (i, ins) in block.code.iter().enumerate().rev() {
  }
  set
}
