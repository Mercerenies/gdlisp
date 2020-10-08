
// TODO Just a basic stub right now; we'll support all Godot literal
// types soon.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Int(i32),
  String(String),
  Null,
  Bool(bool),
}

impl Literal {

  pub fn to_gd(&self) -> String {
    match self {
      Literal::Int(n) => n.to_string(),
      Literal::String(s) => format!("\"{}\"", s), // TODO Proper escaping
      Literal::Null => String::from("null"),
      Literal::Bool(b) => if *b { String::from("true") } else { String::from("false") },
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
