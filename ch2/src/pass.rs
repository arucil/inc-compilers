use ast::Range;

#[derive(Debug, Clone)]
pub struct PassError {
  pub range: Range,
  pub message: String,
}

pub mod uniquify;
pub mod anf;
pub mod control;