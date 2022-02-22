
//! GDScript literal values.
//!
//! This module represents the subset of [GDScript
//! expressions](super::expr) which are literal values, i.e. which
//! evaluate to themselves. This includes literal integers, strings,
//! and the special value `null`.

// TODO Just a basic stub right now; we'll support all Godot literal
// types soon.

use crate::ir::literal::{Literal as IRLiteral};

use ordered_float::OrderedFloat;
use serde::{Serialize, Deserialize};

use std::convert::TryFrom;

/// The type of GDScript literal values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Int(i32),
  Float(OrderedFloat<f32>),
  String(String),
  NodeLiteral(String),
  Null,
  Bool(bool),
}

/// In most cases, we can convert an
/// [`ir::literal::Literal`](IRLiteral) to a [`Literal`]. However,
/// there are a handful of corner cases where `ir::literal::Literal`
/// compiles to something nontrivial in GDScript. In those cases, this
/// error will be returned.
#[derive(Debug, Clone, Default)]
pub struct IRToExprLiteralError;

impl Literal {

  /// Convert a GDScript literal to a string. The result will contain
  /// valid GDScript syntax.
  pub fn to_gd(&self) -> String {
    match self {
      Literal::Int(n) => n.to_string(),
      Literal::String(s) => format!("\"{}\"", s), // TODO Proper escaping
      Literal::NodeLiteral(s) => format!("${}", s), // TODO Proper escaping
      Literal::Null => String::from("null"),
      Literal::Bool(b) => if *b { String::from("true") } else { String::from("false") },
      Literal::Float(f) => format!("{:e}", **f),
    }
  }

}

impl From<i32> for Literal {
  fn from(x: i32) -> Literal {
    Literal::Int(x)
  }
}

impl From<OrderedFloat<f32>> for Literal {
  fn from(x: OrderedFloat<f32>) -> Literal {
    Literal::Float(x)
  }
}

impl From<String> for Literal {
  fn from(x: String) -> Literal {
    Literal::String(x)
  }
}

impl<'a> From<&'a str> for Literal {
  fn from(x: &'a str) -> Literal {
    Literal::String(String::from(x))
  }
}

impl From<bool> for Literal {
  fn from(x: bool) -> Literal {
    Literal::Bool(x)
  }
}

impl TryFrom<IRLiteral> for Literal {
  type Error = IRToExprLiteralError;

  fn try_from(value: IRLiteral) -> Result<Literal, Self::Error> {
    match value {
      IRLiteral::Nil => Ok(Literal::Null),
      IRLiteral::Int(n) => Ok(Literal::Int(n)),
      IRLiteral::Float(f) => Ok(Literal::Float(f)),
      IRLiteral::String(s) => Ok(Literal::String(s)),
      IRLiteral::Symbol(_) => Err(IRToExprLiteralError), // Doesn't compile to a GDScript literal
      IRLiteral::Bool(b) => Ok(Literal::Bool(b)),
    }
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn literal_test() {
    assert_eq!(Literal::Int(10).to_gd(), "10");
    assert_eq!(Literal::Null.to_gd(), "null");
    assert_eq!(Literal::Bool(false).to_gd(), "false");
    assert_eq!(Literal::Bool(true).to_gd(), "true");
  }

  #[test]
  fn string_test() {
    assert_eq!(Literal::String("foo".to_owned()).to_gd(), "\"foo\"");
  }

}
