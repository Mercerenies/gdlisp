
pub struct Progn;

use super::SpecialForm;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::sxp::ast::AST;

impl SpecialForm for Progn {

  fn compile<'a>(&mut self,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 tail: &[&AST],
                 needs_result: NeedsResult)
                 -> Result<StExpr, Error> {
    compiler.compile_stmts(builder, tail, needs_result)
  }

}
