
//! [`CallName`] is the type of valid cars in a call [`AST`]
//! expression.

use super::incremental::IncCompiler;
use super::expr::Expr;
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::dotted::DottedExpr;
use crate::compile::error::{GDError, GDErrorF};
use crate::pipeline::Pipeline;
use crate::pipeline::error::PError;

/// GDLisp is fairly conservative about what sort of [`AST`] values
/// are allowed as the subject of a call. Excluding special forms and
/// other quoted constructs, an `AST` appearing in evaluation context
/// must have a car of one of the forms permitted by `CallName`.
#[derive(Clone, Debug, PartialEq, Eq)]
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
        _ => Err(PError::from(GDError::new(GDErrorF::CannotCall(ast.clone()), ast.pos))),
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::parser;
  use crate::pipeline::Pipeline;
  use crate::pipeline::source::SourceOffset;
  use crate::pipeline::config::ProjectConfig;
  use crate::pipeline::resolver::PanickingNameResolver;
  use crate::ir::expr::ExprF;

  use std::path::PathBuf;

  fn parse_ast(input: &str) -> AST {
    let parser = parser::ASTParser::new();
    parser.parse(input).unwrap()
  }

  fn dummy_config() -> ProjectConfig {
    ProjectConfig {
      root_directory: PathBuf::from(r"."),
      optimizations: false,
    }
  }

  fn dummy_pipeline() -> Pipeline {
    Pipeline::with_resolver(dummy_config(), Box::new(PanickingNameResolver))
  }

  fn resolve_call(input: &str) -> Result<CallName, PError> {
    let ast = parse_ast(input);
    let mut icompiler = IncCompiler::new(vec!());
    CallName::resolve_call_name(&mut icompiler, &mut dummy_pipeline(), &ast)
  }

  #[test]
  fn simple_call_name() {
    assert_eq!(resolve_call("foobar"), Ok(CallName::SimpleName(String::from("foobar"))));
    assert_eq!(resolve_call("some-complicated-name"), Ok(CallName::SimpleName(String::from("some-complicated-name"))));

    // Built-in names are still ordinary calls according to CallName.
    assert_eq!(resolve_call("if"), Ok(CallName::SimpleName(String::from("if"))));

    // `self` is just a name like any other.
    assert_eq!(resolve_call("self"), Ok(CallName::SimpleName(String::from("self"))));

    // Although probably a user mistake, `super` is a valid function
    // name.
    assert_eq!(resolve_call("super"), Ok(CallName::SimpleName(String::from("super"))));

  }

  #[test]
  fn method_name() {
    assert_eq!(resolve_call("foo:bar"), Ok(CallName::MethodName(Box::new(Expr::new(ExprF::LocalVar(String::from("foo")), SourceOffset(0))), String::from("bar"))));
    assert_eq!(resolve_call("self:bar"), Ok(CallName::MethodName(Box::new(Expr::new(ExprF::LocalVar(String::from("self")), SourceOffset(0))), String::from("bar"))));
  }

  #[test]
  fn super_name() {
    assert_eq!(resolve_call("super:bar"), Ok(CallName::SuperName(String::from("bar"))));
  }

  #[test]
  fn atomic_name() {
    assert_eq!(resolve_call("(literally bar)"), Ok(CallName::AtomicName(String::from("bar"))));
  }

}
