
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::ir::incremental::IncCompiler;
use super::{ParseRule, ParseError};

use std::convert::TryInto;

// Modifiers that have to do with an entire GDLisp file.

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FileMod {
  NoStdLib,
}

#[derive(Clone, Debug)]
pub struct NoStdLibParser;

impl FileMod {

  pub fn apply(&self, icompiler: &mut IncCompiler) {
    match self {
      FileMod::NoStdLib => {
        icompiler.mark_as_minimalist()
      }
    }
  }

}

impl ParseRule for NoStdLibParser {
  type Modifier = ();

  fn name(&self) -> &str {
    "NoStdLibParser"
  }

  fn parse_once(&mut self, ast: &AST) -> Result<(), ParseError> {
    let vec: Vec<_> = DottedExpr::new(ast).try_into().map_err(|_| file_error(ast))?;
    if vec.len() != 1 {
      return Err(file_error(ast));
    }
    if let AST::Symbol(sys_nostdlib) = vec[0] {
      if sys_nostdlib == "sys/nostdlib" {
        return Ok(());
      }
    }
    Err(file_error(ast))
  }

}

fn file_error(ast: &AST) -> ParseError {
  ParseError::Expecting(String::from("(file-level declaration)"), ast.clone())
}

pub fn parser() -> impl ParseRule<Modifier=FileMod> {
  NoStdLibParser.unique().map(|_| FileMod::NoStdLib)
}
