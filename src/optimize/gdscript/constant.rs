
use crate::gdscript::expr::Expr;
use crate::gdscript::literal::Literal;

// Attempt to determine whether a value is true or false by looking at
// the expression that produces it. If we can't tell or if the
// expression is stateful, then we return None.
pub fn deduce_bool(expr: &Expr) -> Option<bool> {
  match expr {
    Expr::Literal(lit) => {
      match lit {
        Literal::Int(n) => Some(*n != 0),
        Literal::Float(f) => Some(**f != 0.0),
        Literal::String(s) => Some(s != ""),
        Literal::Null => Some(false),
        Literal::Bool(b) => Some(*b),
      }
    }
    _ => None,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn literals_as_bool_test() {
    assert_eq!(deduce_bool(&Expr::from(0)), Some(false));
    assert_eq!(deduce_bool(&Expr::from(1)), Some(true));
    assert_eq!(deduce_bool(&Expr::Literal(Literal::Null)), Some(false));
    assert_eq!(deduce_bool(&Expr::from(true)), Some(true));
    assert_eq!(deduce_bool(&Expr::from(false)), Some(false));
  }

  #[test]
  fn arbitrary_expr_as_bool_test() {
    assert_eq!(deduce_bool(&Expr::var("some-variable")), None);
  }

}
