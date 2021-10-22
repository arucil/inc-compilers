use asm::{Block, Instr, Label, Program};
use indexmap::IndexMap;

pub fn merge_jumps<INFO>(prog: Program<INFO>) -> Program<INFO> {
  let mut refs = IndexMap::<Label, usize>::new();
  let mut blocks = IndexMap::<Label, Block>::new();
  for (label, block) in prog.blocks {
    if let Instr::Jmp(label) = block.code.last().unwrap() {
      *refs.entry(label.clone()).or_default() += 1;
      if block.code.len() > 1 {
        if let Instr::JumpIf(_, label) = &block.code[block.code.len() - 2] {
          *refs.entry(label.clone()).or_default() += 1;
        }
      }
    }
    blocks.insert(label, block);
  }

  let mut worklist = vec![Label::EntryPoint];
  let mut new_blocks = vec![];
  while let Some(mut label) = worklist.pop() {
    let mut block = blocks.remove(&label).unwrap();
    while let Instr::Jmp(next_label) = block.code.last().unwrap() {
      if block.code.len() > 1 {
        if let Instr::JumpIf(_, label) = &block.code[block.code.len() - 2] {
          worklist.push(*label);
        }
      }

      let next_label = *next_label;
      if refs[&next_label] == 1 {
        let mut next_block = blocks.remove(&next_label).unwrap();
        block.code.pop();
        block.code.append(&mut next_block.code);
      } else {
        block.code.pop();
        new_blocks.push((label, block));
        label = next_label;
        block = blocks.remove(&label).unwrap();
      }
    }
    new_blocks.push((label, block));
  }
  Program {
    info: prog.info,
    blocks: new_blocks,
  }
}