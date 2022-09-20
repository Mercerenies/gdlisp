
#![deprecated]

use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::compile::error::{GDError, GDErrorF};
use crate::compile::args::{Expecting, ExpectedShape};
use crate::pipeline::source::SourceOffset;

use std::convert::TryInto;

#[derive(Clone, Debug)]
pub struct AccessSlotSyntax<'a> {
  pub object: &'a AST,
  pub slot_name: String,
}

pub const ACCESS_SLOT_FORM_NAME: &str = "access-slot";

impl<'a> AccessSlotSyntax<'a> {

  pub fn parse_ast(ast: &'a AST) -> Result<Self, GDError> {
    let args: Vec<_> = DottedExpr::new(ast).try_into()?;
    if args.is_empty() {
      return Err(GDError::new(GDErrorF::InvalidArg(String::from(ACCESS_SLOT_FORM_NAME), ast.clone(), ExpectedShape::NonemptyList), ast.pos));
    }
    let head = ExpectedShape::extract_symbol(ACCESS_SLOT_FORM_NAME, args[0].clone())?;
    if head != ACCESS_SLOT_FORM_NAME {
      return Err(GDError::new(GDErrorF::InvalidArg(String::from(ACCESS_SLOT_FORM_NAME), ast.clone(), ExpectedShape::AccessSlotName), ast.pos));
    }
    Self::parse_from_tail(&args[1..], ast.pos)
  }

  pub fn parse_from_tail(tail: &[&'a AST], pos: SourceOffset) -> Result<Self, GDError> {
    Expecting::exactly(2).validate(ACCESS_SLOT_FORM_NAME, pos, tail)?;
    let object = tail[0];
    let slot_name = ExpectedShape::extract_string(ACCESS_SLOT_FORM_NAME, tail[1].clone())?;
    Ok(AccessSlotSyntax { object, slot_name })
  }

}
