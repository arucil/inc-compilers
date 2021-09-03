use asm::{Program, Label, Instr, Block};
use indexmap::IndexMap;

pub fn merge_jumps<INFO>(prog: Program<INFO>) -> Program<INFO> {
  let mut refs = IndexMap::<Label, usize>::new();
  let mut blocks = IndexMap::<Label, Block>::new();
  for (label, block) in prog.blocks {
    if let Instr::Jmp(label) = block.code.last().unwrap() {
      *refs.entry(label.clone()).or_default() += 1;
      if block.code.len() > 1 {
        if let Instr::Jmp(label) = &block.code[block.code.len() - 2] {
          *refs.entry(label.clone()).or_default() += 1;
        }
      }
    }
    blocks.insert(label, block);
  }
  let mut label = Label::EntryPoint;
  let mut block = blocks.remove(&label).unwrap();
  let mut new_blocks = vec![];
  while let Instr::Jmp(next_label) = block.code.last().unwrap() {
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
  new_blocks.extend(blocks.into_iter());
  Program {
    info: prog.info,
    blocks: new_blocks,
  }
}