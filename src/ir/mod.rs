
// Intermediate representation used between the AST Lisp syntax and
// the GDScript syntax. This representation shares all of the nice
// expression semantics of Lisp but resolves any special forms to
// something easily recognizable.
//
// NOTE: IR still exclusively uses the GDLisp names. Generating
// GDScript names is the responsibility of the next compilation step.

pub mod expr;
pub mod decl;
pub mod arglist;
pub mod literal;
pub mod special_form;
pub mod declaration_table;
pub mod incremental;
pub mod depends;
pub mod macros;
pub mod quasiquote;
pub mod call_name;
pub mod import;
pub mod export;
pub mod identifier;
pub mod modifier;
pub mod access_type;
pub mod closure_names;
pub mod special_ref;
pub mod scope;

use decl::Decl;
use macros::MacroData;
use identifier::Id;
use crate::sxp::ast::AST;
use crate::pipeline::error::PError;
use crate::pipeline::Pipeline;

use std::collections::HashMap;

/// The name of a module's "run" function. A static top-level function
/// with this name should appear in every GDLisp module.
pub const MAIN_BODY_NAME: &str = "run";

pub fn compile_toplevel(pipeline: &mut Pipeline, body: &AST)
                        -> Result<(decl::TopLevel, HashMap<Id, MacroData>), PError> {
  let compiler = incremental::IncCompiler::new(body.all_symbols());
  compiler.compile_toplevel(pipeline, body)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ir::literal::Literal;
  use crate::ir::expr::{Expr, ExprF, AssignTarget};
  use crate::ir::decl::DeclF;
  use crate::ir::arglist::ordinary::ArgList;
  use crate::ir::export::Visibility;
  use crate::pipeline::Pipeline;
  use crate::pipeline::config::ProjectConfig;
  use crate::pipeline::source::SourceOffset;
  use crate::sxp::ast::ASTF;

  use std::path::PathBuf;

  fn int(n: i32) -> AST {
    AST::new(ASTF::Int(n), SourceOffset::default())
  }

  fn symbol(s: &str) -> AST {
    AST::new(ASTF::symbol(s), SourceOffset::default())
  }

  #[allow(dead_code)]
  fn string(s: &str) -> AST {
    AST::new(ASTF::string(s), SourceOffset::default())
  }

  fn nil() -> AST {
    AST::nil(SourceOffset::default())
  }

  #[allow(dead_code)]
  fn cons(a: AST, b: AST) -> AST {
    AST::new(ASTF::cons(a, b), SourceOffset::default())
  }

  fn list(data: Vec<AST>) -> AST {
    AST::dotted_list(data, nil())
  }

  fn call(name: &str, args: Vec<Expr>) -> Expr {
    Expr::call(String::from(name), args, SourceOffset::default())
  }

  fn literal(literal: Literal) -> Expr {
    Expr::literal(literal, SourceOffset::default())
  }

  fn progn(body: Vec<Expr>) -> Expr {
    Expr::progn(body, SourceOffset::default())
  }

  fn compile_expr(pipeline: &mut Pipeline, expr: &AST)
                  -> Result<Expr, PError> {
    let mut compiler = incremental::IncCompiler::new(expr.all_symbols());
    compiler.bind_builtin_macros(pipeline);
    compiler.compile_expr(pipeline, expr)
  }

  fn compile_decl(pipeline: &mut Pipeline, decl: &AST)
                  -> Result<Decl, PError> {
    let mut vec: Vec<Decl> = Vec::new();
    let mut compiler = incremental::IncCompiler::new(decl.all_symbols());
    compiler.bind_builtin_macros(pipeline);
    compiler.compile_decl(pipeline, &mut vec, decl)?;
    assert_eq!(vec.len(), 1);
    Ok(vec.remove(0))
  }

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
    let ast = list(vec!(symbol("foobar"), int(10)));
    let expected = call("foobar", vec!(literal(Literal::Int(10))));
    let actual = do_compile_expr(&ast).unwrap();
    assert_eq!(actual, expected);
  }

  // These used to compile to different IR but they don't anymore.
  // Test is still here because meh.
  #[test]
  fn compile_builtin() {
    let ast = list(vec!(symbol("cons"), int(10)));
    let expected = call("cons", vec!(literal(Literal::Int(10))));
    let actual = do_compile_expr(&ast).unwrap();
    assert_eq!(actual, expected);
  }

  #[test]
  fn compile_int() {
    assert_eq!(do_compile_expr(&int(99)).unwrap(), literal(Literal::Int(99)));
    assert_eq!(do_compile_expr(&int(-10)).unwrap(), literal(Literal::Int(-10)));
  }

  #[test]
  fn compile_nil() {
    assert_eq!(do_compile_expr(&nil()).unwrap(), literal(Literal::Nil));
  }

  #[test]
  fn compile_progn() {
    assert_eq!(do_compile_expr(&list(vec!(symbol("progn")))).unwrap(),
               progn(vec!()));
    assert_eq!(do_compile_expr(&list(vec!(symbol("progn"), int(1)))).unwrap(),
               progn(vec!(literal(Literal::Int(1)))));
    assert_eq!(do_compile_expr(&list(vec!(symbol("progn"), int(1), int(2)))).unwrap(),
               progn(vec!(literal(Literal::Int(1)), literal(Literal::Int(2)))));
  }

  #[test]
  fn compile_defn() {
    assert_eq!(do_compile_decl(&list(vec!(symbol("defn"),
                                          symbol("foobar"),
                                          list(vec!(symbol("a"),
                                                    symbol("b"))),
                                          int(20)))).unwrap(),
               Decl::new(DeclF::FnDecl(decl::FnDecl {
                 visibility: Visibility::FUNCTION,
                 call_magic: None,
                 name: "foobar".to_owned(),
                 args: ArgList::required(vec!("a".to_owned(), "b".to_owned())),
                 body: progn(vec!(literal(Literal::Int(20)))),
               }), SourceOffset::default()));
  }

  #[test]
  fn compile_defmacro() {
    assert_eq!(do_compile_decl(&list(vec!(symbol("defmacro"),
                                          symbol("foobar"),
                                          list(vec!(symbol("a"),
                                                    symbol("b"))),
                                          int(20)))).unwrap(),
               Decl::new(DeclF::MacroDecl(decl::MacroDecl {
                 visibility: Visibility::MACRO,
                 name: "foobar".to_owned(),
                 args: ArgList::required(vec!("a".to_owned(), "b".to_owned())),
                 body: progn(vec!(literal(Literal::Int(20)))),
               }), SourceOffset::default()));
  }

  #[test]
  fn compile_set() {
    assert_eq!(do_compile_expr(&list(vec!(symbol("set"),
                                          symbol("foobar"),
                                          int(1)))).unwrap(),
               Expr::new(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("foobar")), Box::new(literal(Literal::Int(1)))), SourceOffset::default()));
  }

}
