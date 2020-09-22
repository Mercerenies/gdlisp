
pub struct Progn;

use super::SpecialForm;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::compile::symbol_table::SymbolTable;
use crate::sxp::ast::AST;

impl SpecialForm for Progn {

  fn compile<'a, 'b>(&mut self,
                     compiler: &mut Compiler<'a>,
                     builder: &mut StmtBuilder,
                     table: &mut impl SymbolTable<'b>,
                     tail: &[&AST],
                     needs_result: NeedsResult)
             -> Result<StExpr, Error> {
    compiler.compile_stmts(builder, table, tail, needs_result)
  }

}
