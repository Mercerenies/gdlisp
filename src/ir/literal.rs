
use ordered_float::OrderedFloat;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Nil,
  Int(i32),
  Float(OrderedFloat<f32>),
  Bool(bool),
}
