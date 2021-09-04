
//! [`CallName`] is the type of valid cars in a call [`AST`]
//! expression.

use super::expr::Expr;
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::dotted::DottedExpr;

/// GDLisp is fairly conservative about what sort of [`AST`] values
/// are allowed as the subject of a call. Excluding special forms and
/// other quoted constructs, an `AST` appearing in evaluation context
/// must have a car of one of the forms permitted by `CallName`.
#[derive(Clone, Debug)]
pub enum CallName {
  /// A simple [`ASTF::Symbol`] name.
  SimpleName(String),
  /// An `access-slot` qualified call.
  MethodName(Box<Expr>, String),
  /// A `literally` call.
  AtomicName(String),
}

impl CallName {

  /// Attempts to resolve `ast` as an `access-slot` pair, with an
  /// `AST` left-hand side and a string method name.
  pub fn try_resolve_method_name(ast: &AST) -> Option<(&AST, &str)> {
    if let DottedExpr { elements: vec, terminal: AST { value: ASTF::Nil, pos: _ } } = DottedExpr::new(ast) {
      if vec.len() == 3 && vec[0].value == ASTF::symbol("access-slot") {
        if let ASTF::Symbol(name) = &vec[2].value {
          return Some((vec[1], name));
        }
      }
    }
    None
  }

  /// Attempts to resolve `ast` as a `literally` name with a single
  /// symbol argument.
  pub fn try_resolve_atomic_name(ast: &AST) -> Option<&str> {
    if let DottedExpr { elements: vec, terminal: AST { value: ASTF::Nil, pos: _ } } = DottedExpr::new(ast) {
      if vec.len() == 2 && vec[0].value == ASTF::symbol("literally") {
        if let ASTF::Symbol(name) = &vec[1].value {
          return Some(name);
        }
      }
    }
    None
  }

}
