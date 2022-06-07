use asm::{Block, Fun, Instr, Label, Program};
use indexmap::IndexMap;

pub fn merge_blocks<INFO>(prog: Program<INFO>) -> Program<INFO> {
  Program {
    funs: prog
      .funs
      .into_iter()
      .map(|fun| Fun {
        blocks: merge_body_blocks(fun.blocks),
        ..fun
      })
      .collect(),
    blocks: merge_body_blocks(prog.blocks),
    ..prog
  }
}

fn merge_body_blocks(body: Vec<Block>) -> Vec<Block> {
  let mut refs = IndexMap::<Label, usize>::new();
  let mut blocks = IndexMap::<Label, Block>::new();
  for block in body {
    if let Instr::LocalJmp(label) = block.code.last().unwrap() {
      *refs.entry(*label).or_default() += 1;
      if block.code.len() > 1 {
        if let Instr::JumpIf { label, .. } = &block.code[block.code.len() - 2] {
          *refs.entry(*label).or_default() += 1;
        }
      }
    }
    blocks.insert(block.label, block);
  }

  let mut worklist = vec![Label::EntryPoint];
  let mut new_blocks = vec![];
  while let Some(mut label) = worklist.pop() {
    let mut block = if let Some(block) = blocks.remove(&label) {
      block
    } else {
      continue;
    };
    while let Instr::LocalJmp(next_label) = block.code.last().unwrap() {
      if block.code.len() > 1 {
        if let Instr::JumpIf { label, .. } = &block.code[block.code.len() - 2] {
          worklist.push(*label);
        }
      }

      let next_label = *next_label;
      if refs[&next_label] == 1 {
        let mut next_block = blocks.remove(&next_label).unwrap();
        block.code.pop();
        block.code.append(&mut next_block.code);
      } else if let Some(next_block) = blocks.remove(&next_label) {
        block.code.pop();
        block.label = label;
        new_blocks.push(block);
        label = next_label;
        block = next_block;
      } else {
        break;
      }
    }
    block.label = label;
    new_blocks.push(block);
  }
  new_blocks
}
