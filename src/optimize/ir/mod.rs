
pub mod expr_walker;

use crate::ir::decl::TopLevel;
use crate::compile::error::Error;

pub trait FileOptimization {
  fn run_on_file(&self, file: &mut TopLevel) -> Result<(), Error>;
}
