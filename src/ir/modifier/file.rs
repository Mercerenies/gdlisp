// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! Modifiers that have to do with an entire GDLisp file.

use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::ir::incremental::IncCompiler;
use super::{ParseRule, ParseError, ParseErrorF};

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
    if let Some(sys_nostdlib) = vec[0].as_symbol_ref() {
      if sys_nostdlib == "sys/nostdlib" {
        return Ok(());
      }
    }
    Err(file_error(ast))
  }

}

fn file_error(ast: &AST) -> ParseError {
  ParseError::new(ParseErrorF::Expecting(String::from("(file-level declaration)"), ast.clone()), ast.pos)
}

/// Parse rule for modifiers to an entire file.
pub fn parser() -> impl ParseRule<Modifier=FileMod> {
  NoStdLibParser.unique().map(|_| FileMod::NoStdLib)
}
