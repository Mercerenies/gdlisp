
use ordered_float::OrderedFloat;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Nil,
  Int(i32),
  Float(OrderedFloat<f32>),
  String(String),
  Symbol(String),
  Bool(bool),
}
