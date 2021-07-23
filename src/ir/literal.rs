
//! GDLisp literal values.

use ordered_float::OrderedFloat;

/// A GDLisp literal value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Nil,
  Int(i32),
  Float(OrderedFloat<f32>),
  String(String),
  Symbol(String),
  Bool(bool),
}

impl From<i32> for Literal {
  fn from(value: i32) -> Literal {
    Literal::Int(value)
  }
}

impl From<OrderedFloat<f32>> for Literal {
  fn from(value: OrderedFloat<f32>) -> Literal {
    Literal::Float(value)
  }
}

impl From<String> for Literal {
  fn from(value: String) -> Literal {
    Literal::String(value)
  }
}

impl<'a> From<&'a str> for Literal {
  fn from(value: &'a str) -> Literal {
    Literal::String(String::from(value))
  }
}

impl From<bool> for Literal {
  fn from(value: bool) -> Literal {
    Literal::Bool(value)
  }
}
