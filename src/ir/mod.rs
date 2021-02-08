
// Intermediate representation used between the AST Lisp syntax and
// the GDScript syntax. This representation shares all of the nice
// expression semantics of Lisp but resolves any special forms to
// something easily recognizable.
//
// NOTE: IR still exclusively uses the GDLisp names. Generating
// GDScript names is the responsibility of the next compilation step.

pub mod locals;
pub mod functions;
pub mod expr;
pub mod decl;
pub mod arglist;
pub mod literal;
pub mod special_form;

use expr::Expr;
use decl::Decl;
use arglist::ArgList;
use literal::Literal;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::compile::error::Error;

use std::convert::TryInto;
use std::borrow::Borrow;

pub const MAIN_BODY_NAME: &str = "run";

pub fn compile_expr(expr: &AST)
                    -> Result<Expr, Error> {
  match expr {
    AST::Nil | AST::Cons(_, _) => {
      let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
      if vec.is_empty() {
        Ok(Expr::Literal(Literal::Nil))
      } else {
        let head = resolve_call_name(vec[0])?;
        let tail = &vec[1..];
        if let Some(sf) = resolve_special_form(head, tail)? {
          Ok(sf)
        } else {
          let args = tail.iter().map(|x| compile_expr(x)).collect::<Result<Vec<_>, _>>()?;
          Ok(Expr::Call(head.to_owned(), args))
        }
      }
    }
    AST::Array(vec) => {
      let vec = vec.iter().map(compile_expr).collect::<Result<Vec<_>, _>>()?;
      Ok(Expr::Array(vec))
    }
    AST::Int(n) => {
      Ok(Expr::Literal(Literal::Int(*n)))
    }
    AST::Bool(b) => {
      Ok(Expr::Literal(Literal::Bool(*b)))
    }
    AST::Float(f) => {
      Ok(Expr::Literal(Literal::Float(*f)))
    }
    AST::String(s) => {
      Ok(Expr::Literal(Literal::String(s.to_owned())))
    }
    AST::Symbol(s) => {
      Ok(Expr::LocalVar(s.to_string()))
    }
  }
}

pub fn compile_decl(decl: &AST)
                    -> Result<Decl, Error> {
  let vec: Vec<&AST> = DottedExpr::new(decl).try_into()?;
  if vec.is_empty() {
    return Err(Error::InvalidDecl(decl.clone()));
  }
  match vec[0] {
    AST::Symbol(s) => {
      match s.borrow() {
        "defn" => {
          if vec.len() < 3 {
            return Err(Error::InvalidDecl(decl.clone()));
          }
          let name = match vec[1] {
            AST::Symbol(s) => s,
            _ => return Err(Error::InvalidDecl(decl.clone())),
          };
          let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
          let args = ArgList::parse(args)?;
          let body = vec[3..].iter().map(|expr| compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
          Ok(Decl::FnDecl(decl::FnDecl {
            name: name.to_owned(),
            args: args,
            body: Expr::Progn(body),
          }))
        }
        _ => {
          Err(Error::UnknownDecl(s.clone()))
        }
      }
    }
    _ => {
      Err(Error::InvalidDecl(decl.clone()))
    }
  }
}

pub fn compile_toplevel(body: &AST)
                        -> Result<Vec<Decl>, Error> {
  let body: Vec<_> = DottedExpr::new(body).try_into()?;
  let mut decls: Vec<Decl> = Vec::new();
  let mut main: Vec<Expr> = Vec::new();
  for curr in body {
    match compile_decl(curr) {
      Ok(d) => decls.push(d),
      Err(Error::UnknownDecl(_)) => main.push(compile_expr(curr)?),
      Err(e) => return Err(e),
    }
  }
  let main_decl = Decl::FnDecl(decl::FnDecl {
    name: MAIN_BODY_NAME.to_owned(),
    args: ArgList::empty(),
    body: Expr::Progn(main),
  });
  decls.push(main_decl);
  Ok(decls)
}

// TODO For now, we can only call symbols. We'll need to extend this
// eventually to support attributed calls (foo.bar(), etc).
fn resolve_call_name(ast: &AST) -> Result<&str, Error> {
  match ast {
    AST::Symbol(s) => Ok(&*s),
    _ => Err(Error::CannotCall(ast.clone())),
  }
}

fn resolve_special_form(head: &str,
                        tail: &[&AST])
                        -> Result<Option<Expr>, Error> {
  special_form::dispatch_form(head, tail)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::sxp::ast;

  #[test]
  fn compile_call() {
    let ast = ast::list(vec!(AST::Symbol(String::from("foobar")), AST::Int(10)));
    let expected = Expr::Call(String::from("foobar"), vec!(Expr::Literal(Literal::Int(10))));
    let actual = compile_expr(&ast).unwrap();
    assert_eq!(actual, expected);
  }

  // These used to compile to different IR but they don't anymore.
  // Test is still here because meh.
  #[test]
  fn compile_builtin() {
    let ast = ast::list(vec!(AST::Symbol(String::from("cons")), AST::Int(10)));
    let expected = Expr::Call(String::from("cons"), vec!(Expr::Literal(Literal::Int(10))));
    let actual = compile_expr(&ast).unwrap();
    assert_eq!(actual, expected);
  }

  #[test]
  fn compile_int() {
    assert_eq!(compile_expr(&AST::Int(99)).unwrap(), Expr::Literal(Literal::Int(99)));
    assert_eq!(compile_expr(&AST::Int(-10)).unwrap(), Expr::Literal(Literal::Int(-10)));
  }

  #[test]
  fn compile_nil() {
    assert_eq!(compile_expr(&AST::Nil).unwrap(), Expr::Literal(Literal::Nil));
  }

  #[test]
  fn compile_progn() {
    assert_eq!(compile_expr(&ast::list(vec!(AST::Symbol(String::from("progn"))))).unwrap(),
               Expr::Progn(vec!()));
    assert_eq!(compile_expr(&ast::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1)))).unwrap(),
               Expr::Progn(vec!(Expr::Literal(Literal::Int(1)))));
    assert_eq!(compile_expr(&ast::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1), AST::Int(2)))).unwrap(),
               Expr::Progn(vec!(Expr::Literal(Literal::Int(1)), Expr::Literal(Literal::Int(2)))));
  }

  #[test]
  fn compile_defn() {
    assert_eq!(compile_decl(&ast::list(vec!(AST::Symbol("defn".to_owned()),
                                            AST::Symbol("foobar".to_owned()),
                                            ast::list(vec!(AST::Symbol("a".to_owned()),
                                                           AST::Symbol("b".to_owned()))),
                                            AST::Int(20)))).unwrap(),
               Decl::FnDecl(decl::FnDecl {
                 name: "foobar".to_owned(),
                 args: ArgList::required(vec!("a".to_owned(), "b".to_owned())),
                 body: Expr::Progn(vec!(Expr::Literal(Literal::Int(20)))),
               }));
  }

  #[test]
  fn compile_setq() {
    assert_eq!(compile_expr(&ast::list(vec!(AST::Symbol(String::from("setq")),
                                            AST::Symbol(String::from("foobar")),
                                            AST::Int(1)))).unwrap(),
               Expr::Assign(String::from("foobar"), Box::new(Expr::Literal(Literal::Int(1)))));
  }

}
