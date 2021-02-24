
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
pub mod symbol_table;
pub mod incremental;
pub mod depends;
pub mod macros;
pub mod quasiquote;
pub mod call_name;
pub mod import;

use expr::Expr;
use decl::Decl;
use crate::sxp::ast::AST;
use crate::compile::error::Error;
use crate::pipeline::error::{Error as PError};
use crate::pipeline::loader::FileLoader;

pub const MAIN_BODY_NAME: &str = "run";

pub fn compile_expr(expr: &AST)
                    -> Result<Expr, Error> {
  incremental::IncCompiler::new().compile_expr(expr)
}

pub fn compile_decl(decl: &AST)
                    -> Result<Decl, Error> {
  incremental::IncCompiler::new().compile_decl(decl)
}

pub fn compile_toplevel<L, E>(loader: &mut L, body: &AST)
                              -> Result<decl::TopLevel, PError>
where L : FileLoader<Error=E>,
      E : Into<PError> {
  let compiler = incremental::IncCompiler::new();
  compiler.compile_toplevel(loader, body)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::sxp::ast;
  use crate::ir::literal::Literal;
  use crate::ir::expr::Expr;
  use crate::ir::arglist::ArgList;

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
  fn compile_defmacro() {
    assert_eq!(compile_decl(&ast::list(vec!(AST::Symbol("defmacro".to_owned()),
                                            AST::Symbol("foobar".to_owned()),
                                            ast::list(vec!(AST::Symbol("a".to_owned()),
                                                           AST::Symbol("b".to_owned()))),
                                            AST::Int(20)))).unwrap(),
               Decl::MacroDecl(decl::MacroDecl {
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
