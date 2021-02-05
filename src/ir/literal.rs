
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Nil,
  Int(i32),
  Bool(bool),
}
