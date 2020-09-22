
pub mod progn;
pub mod if_;
pub mod cond;
pub mod let_;
pub mod lambda;
pub mod funcall;

use super::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::compile::symbol_table::SymbolTable;
use crate::sxp::ast::AST;

pub trait SpecialForm {

  fn compile<'a>(&mut self,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 table: &mut impl SymbolTable,
                 tail: &[&AST],
                 needs_result: NeedsResult)
                 -> Result<StExpr, Error>;

}

pub fn lookup_and_compile<'a>(compiler: &mut Compiler<'a>,
                              builder: &mut StmtBuilder,
                              table: &mut impl SymbolTable,
                              head: &str,
                              tail: &[&AST],
                              needs_result: NeedsResult)
                              -> Result<Option<StExpr>, Error> {
  match head {
    "progn" => Box::new(progn::Progn).compile(compiler, builder, table, tail, needs_result).map(Some),
    "if" => Box::new(if_::If).compile(compiler, builder, table, tail, needs_result).map(Some),
    "cond" => Box::new(cond::Cond).compile(compiler, builder, table, tail, needs_result).map(Some),
    "let" => Box::new(let_::Let).compile(compiler, builder, table, tail, needs_result).map(Some),
    "lambda" => Box::new(lambda::Lambda).compile(compiler, builder, table, tail, needs_result).map(Some),
    "funcall" => Box::new(funcall::Funcall).compile(compiler, builder, table, tail, needs_result).map(Some),
    _ => Ok(None),
  }
}
