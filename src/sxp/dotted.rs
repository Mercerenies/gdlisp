
use super::ast::AST;

use std::convert::TryFrom;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DottedExpr<'a> {
  pub elements: Vec<&'a AST>,
  pub terminal: &'a AST,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct TryFromDottedExprError {}

fn accumulate_ast<'a>(vec: &mut Vec<&'a AST>, ast: &'a AST) -> &'a AST {
  match ast {
    AST::Cons(car, cdr) => {
      vec.push(car);
      accumulate_ast(vec, cdr)
    }
    _ => ast
  }
}

impl<'a> DottedExpr<'a> {

  pub fn new(ast: &'a AST) -> DottedExpr<'a> {
    let mut elements = Vec::new();
    let terminal = accumulate_ast(&mut elements, ast);
    DottedExpr { elements, terminal }
  }

}

impl<'a> TryFrom<DottedExpr<'a>> for Vec<&'a AST> {
  type Error = TryFromDottedExprError;

  fn try_from(expr: DottedExpr<'a>) -> Result<Vec<&'a AST>, TryFromDottedExprError> {
    if *expr.terminal == AST::Nil {
      Ok(expr.elements)
    } else {
      Err(TryFromDottedExprError {})
    }
  }

}

//// Tests
