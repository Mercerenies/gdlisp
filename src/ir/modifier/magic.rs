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

//! Parser for call magic declarations.
//!
//! [Call magic](crate::compile::symbol_table::call_magic) can be used
//! to force a sort of inlining on standard library calls. This
//! modifier is responsible for parsing declarations that a function
//! would like to exhibit call magic behavior.

use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use super::{ParseRule, ParseError, ParseErrorF};

use std::convert::TryInto;

/// Parse rule for call magic. A `MagicParser` parses expressions of
/// the form `(sys/call-magic name)` where `name` is an arbitrary
/// symbol. On a successful parse, `name` is returned.
#[derive(Clone, Debug)]
pub struct MagicParser;

impl ParseRule for MagicParser {
  type Modifier = String;

  fn name(&self) -> &str {
    "MagicParser"
  }

  fn parse_once(&mut self, ast: &AST) -> Result<String, ParseError> {
    let vec: Vec<_> = DottedExpr::new(ast).try_into().map_err(|_| magic_error(ast))?;
    if vec.len() != 2 {
      return Err(magic_error(ast));
    }
    if let Some(sys_call_magic) = vec[0].as_symbol_ref() {
      if sys_call_magic == "sys/call-magic" {
        if let Some(name) = vec[1].as_symbol_ref() {
          return Ok(name.to_owned());
        }
      }
    }
    Err(magic_error(ast))
  }

}

fn magic_error(ast: &AST) -> ParseError {
  ParseError::new(ParseErrorF::Expecting(String::from("(magic declaration)"), ast.clone()), ast.pos)
}

/// Parser for call magic.
pub fn parser() -> impl ParseRule<Modifier=String> {
  MagicParser
}

