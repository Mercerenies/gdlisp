
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
pub mod export;
pub mod identifier;

use expr::Expr;
use decl::Decl;
use crate::sxp::ast::AST;
use crate::pipeline::error::{Error as PError};
use crate::pipeline::Pipeline;
use crate::ir::macros::MacroData;

use std::collections::HashMap;

pub const MAIN_BODY_NAME: &str = "run";

pub fn compile_expr(pipeline: &mut Pipeline, expr: &AST)
                    -> Result<Expr, PError> {
  let mut compiler = incremental::IncCompiler::new(expr.all_symbols());
  compiler.bind_builtin_macros();
  compiler.compile_expr(pipeline, expr)
}

pub fn compile_decl(pipeline: &mut Pipeline, decl: &AST)
                    -> Result<Decl, PError> {
  let mut compiler = incremental::IncCompiler::new(decl.all_symbols());
  compiler.bind_builtin_macros();
  compiler.compile_decl(pipeline, decl)
}

pub fn compile_toplevel(pipeline: &mut Pipeline, body: &AST)
                        -> Result<(decl::TopLevel, HashMap<String, MacroData>), PError> {
  let mut compiler = incremental::IncCompiler::new(body.all_symbols());
  compiler.bind_builtin_macros();
  compiler.compile_toplevel(pipeline, body)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ir::literal::Literal;
  use crate::ir::expr::{Expr, AssignTarget};
  use crate::ir::arglist::ArgList;
  use crate::pipeline::Pipeline;
  use crate::pipeline::config::ProjectConfig;

  use std::path::PathBuf;

  fn do_compile_expr(expr: &AST) -> Result<Expr, PError> {
    let mut pipeline = Pipeline::new(ProjectConfig { root_directory: PathBuf::from("."), optimizations: false });
    compile_expr(&mut pipeline, &expr)
  }

  fn do_compile_decl(decl: &AST) -> Result<Decl, PError> {
    let mut pipeline = Pipeline::new(ProjectConfig { root_directory: PathBuf::from("."), optimizations: false });
    compile_decl(&mut pipeline, &decl)
  }

  #[test]
  fn compile_call() {
    let ast = AST::list(vec!(AST::Symbol(String::from("foobar")), AST::Int(10)));
    let expected = Expr::Call(String::from("foobar"), vec!(Expr::Literal(Literal::Int(10))));
    let actual = do_compile_expr(&ast).unwrap();
    assert_eq!(actual, expected);
  }

  // These used to compile to different IR but they don't anymore.
  // Test is still here because meh.
  #[test]
  fn compile_builtin() {
    let ast = AST::list(vec!(AST::Symbol(String::from("cons")), AST::Int(10)));
    let expected = Expr::Call(String::from("cons"), vec!(Expr::Literal(Literal::Int(10))));
    let actual = do_compile_expr(&ast).unwrap();
    assert_eq!(actual, expected);
  }

  #[test]
  fn compile_int() {
    assert_eq!(do_compile_expr(&AST::Int(99)).unwrap(), Expr::Literal(Literal::Int(99)));
    assert_eq!(do_compile_expr(&AST::Int(-10)).unwrap(), Expr::Literal(Literal::Int(-10)));
  }

  #[test]
  fn compile_nil() {
    assert_eq!(do_compile_expr(&AST::Nil).unwrap(), Expr::Literal(Literal::Nil));
  }

  #[test]
  fn compile_progn() {
    assert_eq!(do_compile_expr(&AST::list(vec!(AST::Symbol(String::from("progn"))))).unwrap(),
               Expr::Progn(vec!()));
    assert_eq!(do_compile_expr(&AST::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1)))).unwrap(),
               Expr::Progn(vec!(Expr::Literal(Literal::Int(1)))));
    assert_eq!(do_compile_expr(&AST::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1), AST::Int(2)))).unwrap(),
               Expr::Progn(vec!(Expr::Literal(Literal::Int(1)), Expr::Literal(Literal::Int(2)))));
  }

  #[test]
  fn compile_defn() {
    assert_eq!(do_compile_decl(&AST::list(vec!(AST::Symbol("defn".to_owned()),
                                               AST::Symbol("foobar".to_owned()),
                                               AST::list(vec!(AST::Symbol("a".to_owned()),
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
    assert_eq!(do_compile_decl(&AST::list(vec!(AST::Symbol("defmacro".to_owned()),
                                               AST::Symbol("foobar".to_owned()),
                                               AST::list(vec!(AST::Symbol("a".to_owned()),
                                                              AST::Symbol("b".to_owned()))),
                                               AST::Int(20)))).unwrap(),
               Decl::MacroDecl(decl::MacroDecl {
                 name: "foobar".to_owned(),
                 args: ArgList::required(vec!("a".to_owned(), "b".to_owned())),
                 body: Expr::Progn(vec!(Expr::Literal(Literal::Int(20)))),
               }));
  }

  #[test]
  fn compile_set() {
    assert_eq!(do_compile_expr(&AST::list(vec!(AST::Symbol(String::from("set")),
                                               AST::Symbol(String::from("foobar")),
                                               AST::Int(1)))).unwrap(),
               Expr::Assign(AssignTarget::Variable(String::from("foobar")), Box::new(Expr::Literal(Literal::Int(1)))));
  }

}
