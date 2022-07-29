
pub mod expr_walker;

use crate::ir::decl::TopLevel;
use crate::compile::error::GDError;

pub trait FileOptimization {
  fn run_on_file(&self, file: &mut TopLevel) -> Result<(), GDError>;
}
