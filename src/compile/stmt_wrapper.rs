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

//! Trait and implementations for conveniently wrapping [`Expr`] into
//! [`Stmt`].
//!
//! This module defines the [`StmtWrapper`] trait, as well as several
//! implementations of it by structs.

use crate::gdscript::stmt::{Stmt, StmtF};
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::op;
use crate::compile::body::builder::StmtBuilder;
use crate::pipeline::source::SourceOffset;
use super::StExpr;

/// A `StmtWrapper` provides a mechanism for taking an arbitrary
/// [`Expr`] and wrapping it into a [`Stmt`]. It can be thought of as
/// a `Stmt` with a hole in it, which we'll later fill with an
/// expression.
pub trait StmtWrapper {

  /// Wraps the expression in a statement.
  fn wrap_expr(&self, expr: Expr) -> Stmt;

  /// Normally, a wrapper, as the name implies, wraps an expression in
  /// a statement. Sometimes, however, the expression has no side
  /// effects, and the work itself has already been done elsewhere. In
  /// this case, `is_vacuous` specifies whether we actually *need* the
  /// result or not. If `is_vacuous` is true and the expression has no
  /// side effects, it will be elided completely.
  ///
  /// The default `is_vacuous` always returns false. It can be
  /// overridden in implementors.
  fn is_vacuous(&self) -> bool {
    false
  }

  /// Wraps `expr` and places it in `builder`.
  ///
  /// If `self` is vacuous and `expr` does not modify state, this
  /// method does nothing. The reasoning for this is as follows. If
  /// `expr.side_effects` does not modify state, then the expression,
  /// as a value, is only being used for its result, not for its side
  /// effects. If `self.is_vacuous()` is true, then we've declared
  /// that this statement wrapper does not care about the result. So
  /// if we have an expression for which the only thing that matters
  /// is the result and a statement wrapper that's ready to ignore
  /// that result, then we effectively have no need to ever evaluate
  /// the expression in the first place.
  fn wrap_to_builder(&self, builder: &mut StmtBuilder, expr: StExpr) {
    if let Some(stmt) = self.wrap_to_stmt(expr) {
      builder.append(stmt);
    }
  }

  /// Wraps `expr` and returns either zero or one statements, using
  /// the same reasoning at [`StmtWrapper.wrap_to_builder`]. If `self`
  /// is vacuous and the expression is stateless, then the vector will
  /// be empty. Otherwise, a single statement will be produced via
  /// [`StmtWrapper::wrap_expr`].
  fn wrap_to_stmt(&self, expr: StExpr) -> Option<Stmt> {
    let StExpr { expr, side_effects } = expr;
    if side_effects.modifies_state() || !self.is_vacuous() {
      Some(self.wrap_expr(expr))
    } else {
      None
    }
  }

}

/// A [`StmtWrapper`] which wraps the expression in a
/// [`StmtF::ReturnStmt`].
pub struct Return;

/// A [`StmtWrapper`] which wraps the expression in a [`StmtF::Expr`].
/// This is a vacuous statement wrapper, as per
/// [`StmtWrapper::is_vacuous`].
pub struct Vacuous;

/// A [`StmtWrapper`] which wraps the expression in a
/// [`StmtF::Assign`], where the left-hand side is given by the
/// `AssignToExpr` value.
pub struct AssignToExpr(pub Expr);

impl StmtWrapper for Return {
  fn wrap_expr(&self, expr: Expr) -> Stmt {
    let pos = expr.pos;
    Stmt::new(StmtF::ReturnStmt(expr), pos)
  }
}

impl StmtWrapper for Vacuous {

  fn wrap_expr(&self, expr: Expr) -> Stmt {
    let pos = expr.pos;
    Stmt::new(StmtF::Expr(expr), pos)
  }

  fn is_vacuous(&self) -> bool {
    true
  }

}

/// An [`AssignToExpr`] which assigns to an [`ExprF::Var`] with the
/// given name.
pub fn assign_to_var(s: String, pos: SourceOffset) -> AssignToExpr {
  AssignToExpr(Expr::new(ExprF::Var(s), pos))
}

impl StmtWrapper for AssignToExpr {
  fn wrap_expr(&self, expr: Expr) -> Stmt {
    let pos = expr.pos;
    let lhs = Box::new(self.0.clone());
    let rhs = Box::new(expr);
    Stmt::new(StmtF::Assign(lhs, op::AssignOp::Eq, rhs), pos)
  }
}
