
use super::expr::Expr;
use super::literal::Literal;

pub fn int(expr: Expr) -> Expr {
  if let Expr::Literal(Literal::Int(_)) = &expr {
    expr
  } else {
    Expr::Call(None, "int".to_owned(), vec!(expr))
  }
}

pub fn float(expr: Expr) -> Expr {
  // TODO Don't wrap floats if it's already a literal float.
  Expr::Call(None, "float".to_owned(), vec!(expr))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn int_test() {
    assert_eq!(int(Expr::Var("a".to_owned())),
               Expr::Call(None, "int".to_owned(), vec!(Expr::Var("a".to_owned()))));
    assert_eq!(int(Expr::from(10)),
               Expr::from(10));
  }

  #[test]
  fn float_test() {
    assert_eq!(float(Expr::Var("a".to_owned())),
               Expr::Call(None, "float".to_owned(), vec!(Expr::Var("a".to_owned()))));
  }

}
