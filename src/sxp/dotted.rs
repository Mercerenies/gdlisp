
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::sxp::ast;

  #[test]
  fn simple_dot() {
    let ast = AST::Int(33);
    let dot = DottedExpr::new(&ast);

    assert!(dot.elements.is_empty());
    assert_eq!(*dot.terminal, ast);
    assert!(Vec::try_from(dot).is_err());

  }

  #[test]
  fn proper_list() {
    let vec = vec!(AST::Int(1), AST::Int(2), AST::Int(3));
    let ast = ast::list(vec.clone());
    let dot = DottedExpr::new(&ast);

    assert_eq!(dot.elements, vec.iter().collect::<Vec<_>>());
    assert_eq!(*dot.terminal, AST::Nil);

    let vec1 = Vec::try_from(dot);
    assert_eq!(vec1, Ok(vec.iter().collect()));

  }

  #[test]
  fn improper_list() {
    let vec = vec!(AST::Int(1), AST::Int(2), AST::Int(3));
    let end = AST::Int(4);
    let ast = ast::dotted_list(vec.clone(), end.clone());
    let dot = DottedExpr::new(&ast);

    assert_eq!(dot.elements, vec.iter().collect::<Vec<_>>());
    assert_eq!(*dot.terminal, end);

    assert!(Vec::try_from(dot).is_err());

  }

}
