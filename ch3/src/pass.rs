pub mod interference;
pub mod liveness_analysis;
pub mod move_biasing;
pub mod register_allocation;
pub mod perilogue;

use ast::IdxVar;

pub trait VarInfo {
  fn is_ref(&self, var: &IdxVar) -> bool;
}