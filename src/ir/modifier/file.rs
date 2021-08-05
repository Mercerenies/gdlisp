
//! Modifiers that have to do with an entire GDLisp file.

use crate::sxp::ast::{AST, ASTF};
use crate::sxp::dotted::DottedExpr;
use crate::ir::incremental::IncCompiler;
use super::{ParseRule, ParseError};

use std::convert::TryInto;

/// Modifier type for file-local modifiers.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FileMod {
  /// This modifier (`(sys/nostdlib)`) should be applied to the
  /// standard library file to initiate the bootstrapping process. It
  /// instructs the compiler to run a minimal compilation, which skips
  /// over the usual steps of binding the standard library and also
  /// disables macro resolution.
  NoStdLib,
}

/// Custom parse rule which only successfully parses the literal value
/// `(sys/nostdlib)` successfully.
#[derive(Clone, Debug)]
pub struct NoStdLibParser;

impl FileMod {

  /// Apply `self` to the `IncCompiler` supplied.
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
    if let ASTF::Symbol(sys_nostdlib) = &vec[0].value {
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

/// Parse rule for modifiers to an entire file.
pub fn parser() -> impl ParseRule<Modifier=FileMod> {
  NoStdLibParser.unique().map(|_| FileMod::NoStdLib)
}
