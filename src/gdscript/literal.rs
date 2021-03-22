
// TODO Just a basic stub right now; we'll support all Godot literal
// types soon.

use crate::ir::literal::{Literal as IRLiteral};

use ordered_float::OrderedFloat;

use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Int(i32),
  Float(OrderedFloat<f32>),
  String(String),
  Null,
  Bool(bool),
}

#[derive(Debug, Clone, Default)]
pub struct IRToExprLiteralError;

impl Literal {

  pub fn to_gd(&self) -> String {
    match self {
      Literal::Int(n) => n.to_string(),
      Literal::String(s) => format!("\"{}\"", s), // TODO Proper escaping
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
