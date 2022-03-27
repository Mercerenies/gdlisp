
//! [`CallName`] is the type of valid cars in a call [`AST`]
//! expression.

use super::incremental::IncCompiler;
use super::expr::Expr;
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::dotted::DottedExpr;
use crate::compile::error::{Error, ErrorF};
use crate::pipeline::Pipeline;
use crate::pipeline::error::{Error as PError};

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
  /// A call on the special `super` keyword, to invoke the superclass
  /// method with the given name.
  SuperName(String),
}

impl CallName {

  /// Identifies the type of call being referred to by a particular
  /// AST. `ast` shall be the AST we're calling, excluding any
  /// arguments or enclosing structures.
  pub fn resolve_call_name(icompiler: &mut IncCompiler,
                           pipeline: &mut Pipeline,
                           ast: &AST)
                           -> Result<CallName, PError> {
    if let Some((lhs, name)) = CallName::try_resolve_method_name(ast) {
      // Might be a super call; check for that first.
      if lhs.value == ASTF::symbol("super") {
        Ok(CallName::SuperName(name.to_owned()))
      } else {
        let lhs = icompiler.compile_expr(pipeline, lhs)?;
        Ok(CallName::MethodName(Box::new(lhs), name.to_owned()))
      }
    } else if let Some(name) = CallName::try_resolve_atomic_name(ast) {
      Ok(CallName::AtomicName(name.to_owned()))
    } else {
      match &ast.value {
        ASTF::Symbol(s) => Ok(CallName::SimpleName(s.clone())),
        _ => Err(PError::from(Error::new(ErrorF::CannotCall(ast.clone()), ast.pos))),
      }
    }
  }

  /// Attempts to resolve `ast` as an `access-slot` pair, with an
  /// `AST` left-hand side and a string method name.
  fn try_resolve_method_name(ast: &AST) -> Option<(&AST, &str)> {
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
  fn try_resolve_atomic_name(ast: &AST) -> Option<&str> {
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
