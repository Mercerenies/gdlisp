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

//! Functions for validating the use of looping primitives,
//! specifically `break` and `continue`.

pub mod error;

use super::decl::TopLevel;
use super::expr::{Expr, ExprF, AssignTarget, LambdaClass, CallTarget};
use error::{LoopPrimitiveError, LoopPrimitive, LoopPrimitiveErrorF};
use crate::pipeline::source::SourceOffset;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
enum LoopWalker {
  /// `LoopWalker::None` is the default state. If there are no loops
  /// that the current expression is inside, either directly or
  /// indirectly, then we are in this state.
  #[default]
  None,
  /// The state of expressions that are inside a loop, and for which
  /// there are no closures strictly between the current expression
  /// and the innermost enclosing loop. This is the only state in
  /// which it is permitted to use loop primitives.
  Within,
  /// The state of expressions that are inside a loop, but for which
  /// there is a closure strictly between the current expression and
  /// the innermost enclosing loop. Due to limitations in GDScript, we
  /// are not capable of compiling loop primitives in this state.
  ClosureWithin,
}

pub fn check_expr(expr: &Expr) -> Result<(), LoopPrimitiveError> {
  LoopWalker::new().check(expr)
}

pub fn check_all_exprs(toplevel: &TopLevel) -> Result<(), LoopPrimitiveError> {
  for expr in toplevel.inner_exprs() {
    check_expr(expr)?;
  }
  Ok(())
}

impl LoopWalker {

  fn new() -> Self {
    Self::default()
  }

  fn enter_loop(self) -> Self {
    LoopWalker::Within
  }

  fn enter_closure(self) -> Self {
    if self == LoopWalker::None {
      // There's no loop at all, so don't change that fact.
      LoopWalker::None
    } else {
      LoopWalker::ClosureWithin
    }
  }

  fn check(self, expr: &Expr) -> Result<(), LoopPrimitiveError> {
    match &expr.value {
      ExprF::BareName(_) => {}
      ExprF::Literal(_) => {}
      ExprF::Progn(exprs) => {
        for inner in exprs {
          self.check(inner)?;
        }
      }
      ExprF::CondStmt(clauses) => {
        for (cond, body) in clauses {
          self.check(cond)?;
          if let Some(body) = body {
            self.check(body)?;
          }
        }
      }
      ExprF::WhileStmt(cond, body) => {
        // Note: Both the condition *and* the body count as being
        // inside the loop. We *can* break inside of a `while`
        // condition; we'll just end up compiling to the "full" while
        // loop, where the condition is a sequence of statements in a
        // `while True`.
        let walker = self.enter_loop();
        walker.check(cond)?;
        walker.check(body)?;
      }
      ExprF::ForStmt(_, iter, body) => {
        // Contrary to `while`, the iteratee of a `for` loop is
        // *outside* the loop. A `break` in the iteratee expression is
        // meaningless.
        self.check(iter)?;
        self.enter_loop().check(body)?;
      }
      ExprF::Call(object, _, args) => {
        match object {
          CallTarget::Scoped | CallTarget::Super | CallTarget::Atomic => {}
          CallTarget::Object(inner) => { self.check(inner)?; }
        }
        for inner in args {
          self.check(inner)?;
        }
      }
      ExprF::Let(clauses, body) => {
        for clause in clauses {
          self.check(&clause.value)?;
        }
        self.check(body)?;
      }
      ExprF::FunctionLet(_, clauses, body) => {
        let closure_walker = self.enter_closure();
        for clause in clauses {
          closure_walker.check(&clause.body)?;
        }
        self.check(body)?;
      }
      ExprF::Lambda(_, body) => {
        self.enter_closure().check(body)?;
      }
      ExprF::FuncRef(_) => {}
      ExprF::Assign(lhs, rhs) => {
        match lhs {
          AssignTarget::Variable(_, _) => {}
          AssignTarget::InstanceField(_, lhs, _) => { self.check(lhs)?; }
        }
        self.check(rhs)?;
      }
      ExprF::Quote(_) => {}
      ExprF::FieldAccess(lhs, _) => {
        self.check(lhs)?;
      }
      ExprF::LambdaClass(class) => {
        let walker = self.enter_closure();
        let LambdaClass { extends: _, args, constructor, decls } = class.as_ref();
        for inner in args {
          // Not inside the lambda class yet.
          self.check(inner)?;
        }
        if let Some(constructor) = constructor {
          for inner in &constructor.super_call.call {
            walker.check(inner)?;
          }
          walker.check(&constructor.body)?;
        }
        for expr in decls.iter().flat_map(|d| d.inner_exprs()) {
          walker.check(expr)?;
        }
      }
      ExprF::Yield(args) => {
        if let Some((a, b)) = args {
          self.check(a)?;
          self.check(b)?;
        }
      }
      ExprF::Assert(a, b) => {
        self.check(a)?;
        if let Some(b) = b {
          self.check(b)?;
        }
      }
      ExprF::Return(arg) => {
        self.check(arg)?;
      }
      ExprF::Break => {
        self.check_loop_primitive(LoopPrimitive::Break, expr.pos)?;
      }
      ExprF::Continue => {
        self.check_loop_primitive(LoopPrimitive::Continue, expr.pos)?;
      }
      ExprF::SpecialRef(_) => {}
      ExprF::ContextualFilename(_) => {}
      ExprF::Split(_, expr) => {
        self.check(expr)?;
      }
      ExprF::Preload(_) => {}
    };
    Ok(())
  }

  /// This function is called when the walker encounters a loop
  /// primitive, either `break` or `continue`. For a loop primitive to
  /// succeed, `self` *must* be [`LoopWalker::Within`]. If it's in
  /// either of the other states, then an appropriate error is issued.
  fn check_loop_primitive(self, primitive: LoopPrimitive, pos: SourceOffset) -> Result<(), LoopPrimitiveError> {
    match self {
      LoopWalker::None => {
        Err(LoopPrimitiveError::new(
          LoopPrimitiveErrorF { primitive, is_in_closure: false },
          pos,
        ))
      }
      LoopWalker::Within => {
        // Everything is fine :)
        Ok(())
      }
      LoopWalker::ClosureWithin => {
        Err(LoopPrimitiveError::new(
          LoopPrimitiveErrorF { primitive, is_in_closure: true },
          pos,
        ))
      }
    }
  }

}
