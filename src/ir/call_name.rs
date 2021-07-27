
//! [`CallName`] is the type of valid cars in a call [`AST`]
//! expression.

use super::expr::Expr;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;

/// GDLisp is fairly conservative about what sort of [`AST`] values
/// are allowed as the subject of a call. Excluding special forms and
/// other quoted constructs, an `AST` appearing in evaluation context
/// must have a car of one of the forms permitted by `CallName`.
#[derive(Clone, Debug)]
pub enum CallName {
  /// A simple [`AST::Symbol`] name.
  SimpleName(String),
  /// An `access-slot` qualified call.
  MethodName(Box<Expr>, String),
}

impl CallName {

  /// Attempts to resolve `ast` as an `access-slot` pair, with an
  /// `AST` left-hand side and a string method name.
  pub fn try_resolve_method_name(ast: &AST) -> Option<(&AST, &str)> {
    if let DottedExpr { elements: vec, terminal: AST::Nil } = DottedExpr::new(ast) {
      if vec.len() == 3 && *vec[0] == AST::symbol("access-slot") {
        if let AST::Symbol(name) = vec[2] {
          return Some((vec[1], name));
        }
      }
    }
    None
  }

}
