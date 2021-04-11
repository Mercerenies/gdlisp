
use crate::gdscript::expr::{self, Expr};
use crate::gdscript::stmt::Stmt;
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
        Literal::String(s) => Some(s.is_empty()),
        Literal::Null => Some(false),
        Literal::Bool(b) => Some(*b),
      }
    }
    _ => None,
  }
}

// Attempt to discern whether or not a statement or expression has any
// side effects. If in doubt, assume it does.

// TODO Is this just noop::is_code_noop?
pub fn stmt_has_side_effects(stmt: &Stmt) -> bool {
  match stmt {
    Stmt::Expr(e) => {
      expr_has_side_effects(e)
    }
    Stmt::PassStmt => false,
    Stmt::IfStmt(_) | Stmt::ForLoop(_) | Stmt::WhileLoop(_) |
      Stmt::MatchStmt(_, _) => true, // TODO These
    Stmt::BreakStmt | Stmt::ContinueStmt | Stmt::VarDecl(_, _) |
      Stmt::ReturnStmt(_) | Stmt::Assign(_, _, _) => true,
  }
}

pub fn expr_has_side_effects(expr: &Expr) -> bool {
  match expr {
    Expr::Var(_) => false,
    Expr::Literal(_) => false,
    // Subscript is a bit questionable, because it can't call
    // arbitrary methods (only works in a predefined, simple way), but
    // it can err out if out of bounds.
    Expr::Subscript(a, b) => expr_has_side_effects(&*a) || expr_has_side_effects(&*b),
    // Attribute is questionable as well because setget can do method
    // calls.
    Expr::Attribute(a, _) => expr_has_side_effects(&*a),
    Expr::Call(_, _, _) => true,
    Expr::SuperCall(_, _) => true,
    Expr::Unary(_, e) => expr_has_side_effects(e),
    Expr::Binary(a, _, b) => expr_has_side_effects(&*a) || expr_has_side_effects(&*b),
    Expr::TernaryIf(expr::TernaryIf { true_case: a, cond: b, false_case: c }) =>
      expr_has_side_effects(&*a) || expr_has_side_effects(&*b) || expr_has_side_effects(&*c),
    Expr::ArrayLit(es) => es.iter().any(expr_has_side_effects),
    Expr::DictionaryLit(es) => es.iter().any(|(k, v)| expr_has_side_effects(k) || expr_has_side_effects(v)),
  }
}

// A constant expression neither reads nor writes to any local or
// global state. If a variable has a constant value, any references to
// that variable can be safely replaced with the value without
// changing the behavior of the code.
pub fn expr_is_constant(expr: &Expr) -> bool {
  match expr {
    Expr::Var(_) => false,
    Expr::Literal(_) => true,
    Expr::Subscript(_, _) => false,
    Expr::Attribute(_, _) => false,
    Expr::Call(_, _, _) => false,
    Expr::SuperCall(_, _) => false,
    Expr::Unary(_, e) => expr_is_constant(&*e),
    Expr::Binary(a, _, b) => expr_is_constant(&*a) && expr_is_constant(&*b),
    Expr::TernaryIf(expr::TernaryIf { true_case: a, cond: b, false_case: c }) =>
      expr_is_constant(&*a) && expr_is_constant(&*b) && expr_is_constant(&*c),
    // These two produce mutable values that cannot be blindly substituted
    Expr::ArrayLit(_) => false,
    Expr::DictionaryLit(_) => false,
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
