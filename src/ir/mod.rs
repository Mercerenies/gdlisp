
// Intermediate representation used between the AST Lisp syntax and
// the GDScript syntax. This representation shares all of the nice
// expression semantics of Lisp but resolves any special forms to
// something easily recognizable.
//
// NOTE: IR still exclusively uses the GDLisp names. Generating
// GDScript names is the responsibility of the next compilation step.

pub mod expr;
pub mod literal;
pub mod special_form;

use expr::Expr;
use literal::Literal;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::error::Error;

use std::convert::TryInto;

pub fn compile_expr(expr: &AST,
                    table: &mut impl SymbolTable)
                    -> Result<Expr, Error> {
  match expr {
    AST::Nil | AST::Cons(_, _) => {
      let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
      if vec.is_empty() {
        Ok(Expr::Literal(Literal::Nil))
      } else {
        let head = resolve_call_name(vec[0])?;
        let tail = &vec[1..];
        if let Some(sf) = resolve_special_form(table, head, tail)? {
          Ok(sf)
        } else if let Some(call) = compile_builtin_call(table, head, tail)? {
          Ok(call)
        } else {
          let args = tail.into_iter().map(|x| compile_expr(x, table)).collect::<Result<Vec<_>, _>>()?;
          Ok(Expr::Call(head.to_owned(), args))
        }
      }
    }
    AST::Int(n) => {
      Ok(Expr::Literal(Literal::Int(*n)))
    }
      AST::Float(_) => {
        panic!("Not implemented yet!") ////
      }
      AST::String(_) => {
        panic!("Not implemented yet!") ////
      }
      AST::Symbol(s) => {
        // TODO Keep track of what kind of variable it is. (local, closure, etc.)
        table.get_var(s).ok_or_else(|| Error::NoSuchVar(s.clone())).map(|var| {
          Expr::LocalVar(var.to_string())
        })
      }
  }
}

// TODO For now, we can only call symbols. We'll need to extend this
// eventually to support attributed calls (foo.bar(), etc).
fn resolve_call_name<'c>(ast: &'c AST) -> Result<&'c str, Error> {
  match ast {
    AST::Symbol(s) => Ok(&*s),
    _ => Err(Error::CannotCall(ast.clone())),
  }
}

fn resolve_special_form(table: &mut impl SymbolTable,
                        head: &str,
                        tail: &[&AST])
                        -> Result<Option<Expr>, Error> {
  special_form::dispatch_form(table, head, tail)
}

fn compile_builtin_call(table: &mut impl SymbolTable,
                        head: &str,
                        tail: &[&AST])
                        -> Result<Option<Expr>, Error> {
  match head {
    "cons" => Some(String::from("cons")),
    _ => None,
  }.map(|head| {
    let args = tail.into_iter().map(|x| compile_expr(x, table)).collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::BuiltInCall(head, args))
  }).transpose()
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::sxp::ast;
  use crate::compile::symbol_table::concrete::ConcreteTable;

  fn compile(ast: &AST) -> Result<Expr, Error> {
    let mut table = ConcreteTable::new();
    compile_expr(ast, &mut table)
  }

  #[test]
  fn compile_call() {
    let ast = ast::list(vec!(AST::Symbol(String::from("foobar")), AST::Int(10)));
    let expected = Expr::Call(String::from("foobar"), vec!(Expr::Literal(Literal::Int(10))));
    let actual = compile(&ast).unwrap();
    assert_eq!(actual, expected);
  }

  #[test]
  fn compile_builtin() {
    let ast = ast::list(vec!(AST::Symbol(String::from("cons")), AST::Int(10)));
    let expected = Expr::BuiltInCall(String::from("cons"), vec!(Expr::Literal(Literal::Int(10))));
    let actual = compile(&ast).unwrap();
    assert_eq!(actual, expected);
  }

  #[test]
  fn compile_int() {
    assert_eq!(compile(&AST::Int(99)).unwrap(), Expr::Literal(Literal::Int(99)));
    assert_eq!(compile(&AST::Int(-10)).unwrap(), Expr::Literal(Literal::Int(-10)));
  }

  #[test]
  fn compile_nil() {
    assert_eq!(compile(&AST::Nil).unwrap(), Expr::Literal(Literal::Nil));
  }

  #[test]
  fn compile_progn() {
    assert_eq!(compile(&ast::list(vec!(AST::Symbol(String::from("progn"))))).unwrap(),
               Expr::Progn(vec!()));
    assert_eq!(compile(&ast::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1)))).unwrap(),
               Expr::Progn(vec!(Expr::Literal(Literal::Int(1)))));
    assert_eq!(compile(&ast::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1), AST::Int(2)))).unwrap(),
               Expr::Progn(vec!(Expr::Literal(Literal::Int(1)), Expr::Literal(Literal::Int(2)))));
  }

}
