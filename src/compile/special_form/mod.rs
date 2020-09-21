
pub mod progn;
pub mod if_;
pub mod cond;
pub mod let_;
pub mod lambda;

use super::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::sxp::ast::AST;

pub trait SpecialForm {

  fn compile<'a>(&mut self,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 tail: &[&AST],
                 needs_result: NeedsResult)
                 -> Result<StExpr, Error>;

}

pub fn lookup(head: &str) -> Option<Box<dyn SpecialForm>> {
  match head {
    "progn" => Some(Box::new(progn::Progn)),
    "if" => Some(Box::new(if_::If)),
    "cond" => Some(Box::new(cond::Cond)),
    "let" => Some(Box::new(let_::Let)),
    "lambda" => Some(Box::new(lambda::Lambda)),
    _ => None,
  }
}

pub fn lookup_and_compile<'a>(compiler: &mut Compiler<'a>,
                              builder: &mut StmtBuilder,
                              head: &str,
                              tail: &[&AST],
                              needs_result: NeedsResult)
                              -> Result<Option<StExpr>, Error> {
  match lookup(head) {
    None => Ok(None),
    Some(mut sf) => sf.compile(compiler, builder, tail, needs_result).map(Some),
  }
}
