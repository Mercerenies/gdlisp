
// TODO Just a basic stub right now; we'll support all Godot literal
// types soon.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Int(i32),
}

impl Literal {

  pub fn to_gd(&self) -> String {
    match self {
      Literal::Int(n) => n.to_string(),
    }
  }

}
