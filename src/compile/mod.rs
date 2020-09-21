
pub mod names;
pub mod body;
pub mod error;
pub mod stmt_wrapper;
pub mod symbol_table;
pub mod special_form;

use body::builder::StmtBuilder;
use names::fresh::FreshNameGenerator;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::literal::Literal;
use error::Error;
use stmt_wrapper::StmtWrapper;
use symbol_table::{SymbolTable, HasSymbolTable};

use std::convert::TryInto;

// Note that we are NOT consuming the AST here. This means that (at
// least for atoms) we'll be doing some copying, especially of strings
// and the like. This is fine; I don't want the compiled form and the
// original AST to be sharing responsibilities for the same data.

pub struct Compiler<'a> {
  gen: FreshNameGenerator<'a>,
  table: SymbolTable,
}

#[derive(Debug, Clone)]
pub struct StExpr(pub Expr, pub bool); // An expression and a declaration of whether or not it's stateful.

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NeedsResult { No, Yes }

impl From<NeedsResult> for bool {
  fn from(s: NeedsResult) -> bool {
    s == NeedsResult::Yes
  }
}

impl From<bool> for NeedsResult {
  fn from(b: bool) -> NeedsResult {
    if b { NeedsResult::Yes } else { NeedsResult::No }
  }
}

impl<'a> Compiler<'a> {

  pub fn new(gen: FreshNameGenerator<'a>) -> Compiler<'a> {
    Compiler { gen, table: SymbolTable::new() }
  }

  pub fn compile_stmts(&mut self,
                       builder: &mut StmtBuilder,
                       stmts: &[&AST],
                       needs_result: NeedsResult)
                       -> Result<StExpr, Error> {
    if stmts.is_empty() {
      Ok(Compiler::nil_expr())
    } else {
      let prefix = &stmts[..stmts.len()-1];
      let end = &stmts[stmts.len()-1];
      for x in prefix {
        self.compile_stmt(builder, &mut stmt_wrapper::Vacuous, x)?;
      }
      self.compile_expr(builder, end, needs_result)
    }
  }

  pub fn compile_stmt(&mut self,
                      builder: &mut StmtBuilder,
                      destination: &dyn StmtWrapper,
                      stmt: &AST)
                      -> Result<(), Error> {
    let needs_result = NeedsResult::from(!destination.is_vacuous());
    let expr = self.compile_expr(builder, stmt, needs_result)?;
    destination.wrap_to_builder(builder, expr);
    Ok(())
  }

  pub fn compile_expr(&mut self,
                      builder: &mut StmtBuilder,
                      expr: &AST,
                      needs_result: NeedsResult)
                      -> Result<StExpr, Error> {
    match expr {
      AST::Nil | AST::Cons(_, _) => {
        let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
        if vec.is_empty() {
          Ok(Compiler::nil_expr())
        } else {
          let head = Compiler::resolve_call_name(vec[0])?;
          let tail = &vec[1..];
          self.resolve_special_form(builder, head, tail, needs_result)?.map_or_else(|| {
            let args = tail.into_iter()
                           .map(|x| self.compile_expr(builder, x, NeedsResult::Yes))
                           .collect::<Result<Vec<_>, _>>()?;
            // Discard the stateful flag; we need the args either way.
            let args = args.into_iter().map(|x| x.0).collect();
            Ok(StExpr(Expr::Call(None, names::lisp_to_gd(head), args), true))
          }, Ok)
        }
      }
      AST::Int(n) => {
        Ok(StExpr(Expr::Literal(Literal::Int(*n)), false))
      }
      AST::Float(_) => {
        panic!("Not implemented yet!") ////
      }
      AST::String(_) => {
        panic!("Not implemented yet!") ////
      }
      AST::Symbol(s) => {
        // May have to revisit needs_resultness of this one. setget may cause issues here.
        self.get_var(s).ok_or_else(|| Error::NoSuchVar(s.clone())).map(|var| {
          StExpr(Expr::Var(var.to_string()), false)
        })
      }
    }
  }

  pub fn nil_expr() -> StExpr {
    let name = String::from("GDLisp");
    StExpr(Expr::Attribute(Box::new(Expr::Var(name)), String::from("Nil")), false)
  }

  // TODO For now, we can only call symbols. We'll need to extend this
  // eventually to support attributed calls (foo.bar(), etc).
  fn resolve_call_name<'c>(ast: &'c AST) -> Result<&'c str, Error> {
    match ast {
      AST::Symbol(s) => Ok(&*s),
      _ => Err(Error::CannotCall(ast.clone())),
    }
  }

  fn resolve_special_form(&mut self,
                          builder: &mut StmtBuilder,
                          head: &str,
                          tail: &[&AST],
                          needs_result: NeedsResult)
                          -> Result<Option<StExpr>, Error> {
    match special_form::lookup_and_compile(self, builder, head, tail, needs_result)? {
      None => Ok(None),
      Some(expr) => return Ok(Some(expr)),
    }
  }

  fn declare_var(&mut self, builder: &mut StmtBuilder, prefix: &str, value: Option<Expr>) -> String {
    let var_name = self.gen.generate_with(prefix);
    let value = value.unwrap_or(Compiler::nil_expr().0);
    builder.append(Stmt::VarDecl(var_name.clone(), value));
    var_name
  }

}

impl HasSymbolTable for Compiler<'_> {

  fn get_symbol_table(&self) -> &SymbolTable {
    &self.table
  }

  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable {
    &mut self.table
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::decl::Decl;
  use crate::sxp::ast;

  // TODO A lot more of this

  fn compile_stmt(ast: &AST) -> Result<(Vec<Stmt>, Vec<Decl>), Error> {
    let used_names = ast.all_symbols();
    let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));
    let mut builder = StmtBuilder::new();
    let () = compiler.compile_stmt(&mut builder, &mut stmt_wrapper::Return, &ast)?;
    Ok(builder.build())
  }

  /* With our new scoping rules, this won't work since the variable doesn't exist.
  #[test]
  fn compile_var() {
    let ast = AST::Symbol(String::from("foobar"));
    let expected = Stmt::ReturnStmt(Expr::Var(String::from("foobar")));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }
  */

  #[test]
  fn compile_call() {
    let ast = ast::list(vec!(AST::Symbol(String::from("foobar")), AST::Int(10)));
    let expected = Stmt::ReturnStmt(Expr::Call(None, String::from("foobar"), vec!(Expr::Literal(Literal::Int(10)))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_int() {
    let ast = AST::Int(99);
    let expected = Stmt::ReturnStmt(Expr::Literal(Literal::Int(99)));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_vacuous() {
    let ast = ast::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1), AST::Int(2)));
    let expected = vec!(Stmt::ReturnStmt(Expr::Literal(Literal::Int(2))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_stateful() {
    let ast = ast::list(vec!(AST::Symbol(String::from("progn")),
                             ast::list(vec!(AST::Symbol(String::from("foo")))),
                             ast::list(vec!(AST::Symbol(String::from("bar"))))));
    let expected = vec!(Stmt::Expr(Expr::Call(None, String::from("foo"), vec!())),
                        Stmt::ReturnStmt(Expr::Call(None, String::from("bar"), vec!())));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_nil() {
    let result1 = compile_stmt(&AST::Nil).unwrap();
    assert_eq!(result1, (vec!(Stmt::ReturnStmt(Compiler::nil_expr().0)), vec!()));

    let result2 = compile_stmt(&ast::list(vec!(AST::Symbol(String::from("progn"))))).unwrap();
    assert_eq!(result2, (vec!(Stmt::ReturnStmt(Compiler::nil_expr().0)), vec!()));
  }

}
