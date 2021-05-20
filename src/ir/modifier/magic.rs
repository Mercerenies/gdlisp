
// Parser for call magic declarations (used primary in FnMod).

use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use super::{ParseRule, ParseError};

use std::convert::TryInto;

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
    if let AST::Symbol(sys_call_magic) = vec[0] {
      if sys_call_magic == "sys/call-magic" {
        if let AST::Symbol(name) = vec[1] {
          return Ok(name.to_owned());
        }
      }
    }
    Err(magic_error(ast))
  }

}

fn magic_error(ast: &AST) -> ParseError {
  ParseError::Expecting(String::from("(magic declaration)"), ast.clone())
}

pub fn parser() -> impl ParseRule<Modifier=String> {
  MagicParser
}

