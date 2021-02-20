
use super::expr::Expr;
use crate::sxp::ast::{self, AST};
use crate::sxp::dotted::DottedExpr;

#[derive(Clone, Debug)]
pub enum CallName {
  SimpleName(String),
  MethodName(Box<Expr>, String),
}

impl CallName {

  pub fn try_resolve_method_name(ast: &AST) -> Option<(&AST, &str)> {
    if let DottedExpr { elements: vec, terminal: AST::Nil } = DottedExpr::new(ast) {
      if vec.len() == 3 {
        if *vec[0] == ast::symbol("access-slot") {
          if let AST::Symbol(name) = vec[2] {
            return Some((vec[1], name));
          }
        }
      }
    }
    None
  }

}
