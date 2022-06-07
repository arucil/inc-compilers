use asm::{Block, Instr, Program};
use ch6::pass::perilogue;
use ch6::pass::register_allocation::Info;

pub fn patch_tailjmps(mut prog: Program<Info>) -> Program<Info> {
  prog.blocks = patch_body(&prog.info, prog.blocks);
  for fun in &mut prog.funs {
    fun.blocks = patch_body(&fun.info, std::mem::take(&mut fun.blocks));
  }
  prog
}

fn patch_body(info: &Info, body: Vec<Block>) -> Vec<Block> {
  body
    .into_iter()
    .map(|block| patch_block(info, block))
    .collect()
}

fn patch_block(info: &Info, block: Block) -> Block {
  let mut code = vec![];
  for instr in block.code {
    if let Instr::TailJmp {
      label,
      arity: _,
    } = instr
    {
      code.extend(perilogue::make_epilogue(info).code);
      code.push(Instr::Jmp(label));
    } else {
      code.push(instr);
    }
  }
  Block {
    label: block.label,
    code,
  }
}
