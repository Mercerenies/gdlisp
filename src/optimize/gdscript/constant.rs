// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

use crate::gdscript::expr::{self, Expr, ExprF};
use crate::gdscript::stmt::{Stmt, StmtF};
use crate::gdscript::literal::Literal;

// Attempt to determine whether a value is true or false by looking at
// the expression that produces it. If we can't tell or if the
// expression is stateful, then we return None.
pub fn deduce_bool(expr: &Expr) -> Option<bool> {
  match &expr.value {
    ExprF::Literal(lit) => {
      match lit {
        Literal::Int(n) => Some(*n != 0),
        Literal::Float(f) => Some(**f != 0.0),
        Literal::String(s) => Some(!s.is_empty()),
        Literal::NodeLiteral(_) => None, // Truthy iff the node exists; can't determine at compile-time
        Literal::NodePathLiteral(s) => Some(!s.is_empty()),
        Literal::Null => Some(false),
        Literal::Bool(b) => Some(*b),
      }
    }
    _ => None,
  }
}

// Attempt to discern whether or not a statement or expression has any
// side effects. If in doubt, assume it does.

pub fn stmt_has_side_effects(stmt: &Stmt) -> bool {
  match &stmt.value {
    StmtF::Expr(e) => {
      expr_has_side_effects(e)
    }
    StmtF::PassStmt => false,
    StmtF::IfStmt(_) | StmtF::ForLoop(_) | StmtF::WhileLoop(_) |
      StmtF::MatchStmt(_, _) => true, // TODO These
    StmtF::BreakStmt | StmtF::ContinueStmt | StmtF::VarDecl(_, _) |
      StmtF::ReturnStmt(_) | StmtF::Assign(_, _, _) => true,
  }
}

pub fn expr_has_side_effects(expr: &Expr) -> bool {
  match &expr.value {
    ExprF::Var(_) => false,
    ExprF::Literal(_) => false,
    // Subscript is a bit questionable, because it can't call
    // arbitrary methods (only works in a predefined, simple way), but
    // it can err out if out of bounds.
    ExprF::Subscript(a, b) => expr_has_side_effects(a) || expr_has_side_effects(b),
    // Attribute is questionable as well because setget can do method
    // calls.
    ExprF::Attribute(a, _) => expr_has_side_effects(a),
    ExprF::Call(_, _, _) => true,
    ExprF::SuperCall(_, _) => true,
    ExprF::Unary(_, e) => expr_has_side_effects(e),
    ExprF::Binary(a, _, b) => expr_has_side_effects(a) || expr_has_side_effects(b),
    ExprF::TernaryIf(expr::TernaryIf { true_case: a, cond: b, false_case: c }) =>
      expr_has_side_effects(a) || expr_has_side_effects(b) || expr_has_side_effects(c),
    ExprF::ArrayLit(es) => es.iter().any(expr_has_side_effects),
    ExprF::DictionaryLit(es) => es.iter().any(|(k, v)| expr_has_side_effects(k) || expr_has_side_effects(v)),
  }
}

// A constant expression neither reads nor writes to any local or
// global state. If a variable has a constant value, any references to
// that variable can be safely replaced with the value without
// changing the behavior of the code.
pub fn expr_is_constant(expr: &Expr) -> bool {
  match &expr.value {
    ExprF::Var(_) => false,
    ExprF::Literal(_) => true,
    ExprF::Subscript(_, _) => false,
    ExprF::Attribute(_, _) => false,
    ExprF::Call(_, _, _) => false,
    ExprF::SuperCall(_, _) => false,
    ExprF::Unary(_, e) => expr_is_constant(e),
    ExprF::Binary(a, _, b) => expr_is_constant(a) && expr_is_constant(b),
    ExprF::TernaryIf(expr::TernaryIf { true_case: a, cond: b, false_case: c }) =>
      expr_is_constant(a) && expr_is_constant(b) && expr_is_constant(c),
    // These two produce mutable values that cannot be blindly substituted
    ExprF::ArrayLit(_) => false,
    ExprF::DictionaryLit(_) => false,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pipeline::source::SourceOffset;

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  #[test]
  fn literals_as_bool_test() {
    assert_eq!(deduce_bool(&e(ExprF::from(0))), Some(false));
    assert_eq!(deduce_bool(&e(ExprF::from(1))), Some(true));
    assert_eq!(deduce_bool(&e(ExprF::Literal(Literal::Null))), Some(false));
    assert_eq!(deduce_bool(&e(ExprF::from(true))), Some(true));
    assert_eq!(deduce_bool(&e(ExprF::from(false))), Some(false));
    assert_eq!(deduce_bool(&e(ExprF::from("A"))), Some(true));
    assert_eq!(deduce_bool(&e(ExprF::from(""))), Some(false));
    assert_eq!(deduce_bool(&e(ExprF::Literal(Literal::NodeLiteral(String::from("foobar"))))), None);
    assert_eq!(deduce_bool(&e(ExprF::Literal(Literal::NodePathLiteral(String::from("A"))))), Some(true));
    assert_eq!(deduce_bool(&e(ExprF::Literal(Literal::NodePathLiteral(String::from(""))))), Some(false));
  }

  #[test]
  fn arbitrary_expr_as_bool_test() {
    assert_eq!(deduce_bool(&Expr::var("some-variable", SourceOffset::default())), None);
  }

}
