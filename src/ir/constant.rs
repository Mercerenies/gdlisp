
// Functionality for checking whether an expression is actually an
// allowable constant expression.
//
// For the moment, what's allowable as a constant expression is
// *extremely* conservative. Even things like if-statements where all
// arguments are constant are disallowed, as we have no way to compile
// that on the GDScript side. Long term, it would be nice to support
// things like that which are constant "in spirit" and work around
// GDScript limitations. But for now, we're being super strict.

use super::expr::Expr;
use super::literal::Literal;
use crate::compile::error::Error;
use crate::sxp::ast::AST;

pub trait MaybeConstant {

  fn is_allowable_const(&self) -> bool;

  fn validate_const_expr(&self, name: &str) -> Result<(), Error> {
    if self.is_allowable_const() {
      Ok(())
    } else {
      Err(Error::NotConstantEnough(name.to_owned()))
    }
  }

}

impl MaybeConstant for AST {
  fn is_allowable_const(&self) -> bool {
    match self {
      AST::Int(_) | AST::Float(_) | AST::Bool(_) | AST::String(_) => true,
      AST::Vector2(a, b) => {
        a.is_allowable_const() && b.is_allowable_const()
      }
      AST::Vector3(a, b, c) => {
        a.is_allowable_const() && b.is_allowable_const() && c.is_allowable_const()
      }
      AST::Array(vec) => {
        vec.iter().all(AST::is_allowable_const)
      }
      AST::Dictionary(vec) => {
        vec.iter().all(|(k, v)| k.is_allowable_const() && v.is_allowable_const())
      }
      AST::Nil | AST::Symbol(_) | AST::Cons(_, _) => false, // TODO Can we magic this problem away?
    }
  }
}

impl MaybeConstant for Literal {
  fn is_allowable_const(&self) -> bool {
    match self {
      Literal::Int(_) | Literal::Float(_) | Literal::String(_) | Literal::Bool(_) => true,
      Literal::Nil | Literal::Symbol(_) => false, // TODO Can we magic this problem away?
    }
  }
}

impl MaybeConstant for Expr {
  fn is_allowable_const(&self) -> bool {
    match self {
      Expr::LocalVar(_) => {
        false // TODO Better?
      }
      Expr::Literal(lit) => {
        lit.is_allowable_const()
      }
      Expr::Progn(body) => {
        // TODO Magic? :)
        body.len() == 1 && body[0].is_allowable_const()
      }
      Expr::Call(_, _) => {
        // TODO Allow arithmetic operators?
        false
      }
      Expr::Array(arr) => {
        arr.iter().all(Expr::is_allowable_const)
      }
      Expr::Dictionary(arr) => {
        arr.iter().all(|(k, v)| k.is_allowable_const() && v.is_allowable_const())
      }
      Expr::Quote(ast) => {
        ast.is_allowable_const()
      }
      Expr::Vector2(a, b) => {
        a.is_allowable_const() && b.is_allowable_const()
      }
      Expr::Vector3(a, b, c) => {
        a.is_allowable_const() && b.is_allowable_const() && c.is_allowable_const()
      }
      Expr::CondStmt(_) | Expr::WhileStmt(_, _) | Expr::ForStmt(_, _, _) |
      Expr::Let(_, _) | Expr::FLet(_, _) | Expr::Labels(_, _) | Expr::Lambda(_, _) | Expr::FuncRef(_) |
      Expr::Assign(_, _) | Expr::FieldAccess(_, _) | Expr::MethodCall(_, _, _) | Expr::LambdaClass(_) |
      Expr::Yield(_) | Expr::Return(_) => {
        false
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ordered_float::OrderedFloat;

  #[test]
  fn constexpr_test_ast_positive() {
    assert!(AST::Int(100).is_allowable_const());
    assert!(AST::Float(OrderedFloat(0.0)).is_allowable_const());
    assert!(AST::Bool(true).is_allowable_const());
    assert!(AST::Bool(false).is_allowable_const());
    assert!(AST::String(String::from("foobar")).is_allowable_const());
    assert!(AST::Vector2(Box::new(AST::Int(1)), Box::new(AST::Int(2))).is_allowable_const());
    assert!(AST::Vector3(Box::new(AST::Int(1)), Box::new(AST::Int(2)), Box::new(AST::Int(3))).is_allowable_const());
    assert!(AST::Array(vec!()).is_allowable_const());
    assert!(AST::Array(vec!(AST::Int(1), AST::Int(2))).is_allowable_const());
  }

  #[test]
  fn constexpr_test_ast_negative() {
    assert!(!AST::Nil.is_allowable_const());
    assert!(!AST::Symbol(String::from("symbol-name")).is_allowable_const());
    assert!(!AST::Cons(Box::new(AST::Int(1)), Box::new(AST::Int(2))).is_allowable_const());
    assert!(!AST::Vector2(Box::new(AST::Nil), Box::new(AST::Int(2))).is_allowable_const());
    assert!(!AST::Vector3(Box::new(AST::Int(1)), Box::new(AST::Int(2)), Box::new(AST::Nil)).is_allowable_const());
    assert!(!AST::Array(vec!(AST::Int(1), AST::Int(2), AST::Nil)).is_allowable_const());
  }

  // TODO Test the rest of these

}
