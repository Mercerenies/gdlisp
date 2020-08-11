
pub mod names;
pub mod body;
pub mod error;
pub mod stmt_wrapper;

use body::builder::StmtBuilder;
use names::fresh::FreshNameGenerator;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::gdscript::expr::Expr;
use crate::gdscript::literal::Literal;
use error::Error;
use stmt_wrapper::StmtWrapper;

use std::convert::TryInto;

// Note that we are NOT consuming the AST here. This means that (at
// least for atoms) we'll be doing some copying, especially of strings
// and the like. This is fine; I don't want the compiled form and the
// original AST to be sharing responsibilities for the same data.

pub struct Compiler<'a> {
  gen: FreshNameGenerator<'a>
}

impl<'a> Compiler<'a> {

  pub fn new(gen: FreshNameGenerator<'a>) -> Compiler<'a> {
    Compiler { gen }
  }

  pub fn compile_statement(&mut self,
                           builder: &mut StmtBuilder,
                           mut destination: impl StmtWrapper,
                           stmt: &AST)
                           -> Result<(), Error> {
    let expr = self.compile_expression(builder, stmt)?;
    builder.append(destination.wrap_expr(expr));
    Ok(())
  }

  pub fn compile_expression(&mut self,
                            builder: &mut StmtBuilder,
                            expr: &AST)
                            -> Result<Expr, Error> {
    match expr {
      AST::Nil | AST::Cons(_, _) => {
        let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
        if vec.is_empty() {
          panic!("Not implemented yet!") //// nil case, I'll deal with it later
        } else {
          let head = Compiler::resolve_call_name(vec[0])?;
          let tail = &vec[1..];
          self.resolve_special_form(builder, head, tail)?.map_or_else(|| {
            let args = tail.into_iter().map(|x| self.compile_expression(builder, x)).collect::<Result<_, _>>()?;
            Ok(Expr::Call(None, names::lisp_to_gd(head), args))
          }, Ok)
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
        Ok(Expr::Var(names::lisp_to_gd(s)))
      }
    }
  }

  // TODO For now, we can only call symbols. We'll need to extend this
  // eventually to support attributed calls (foo.bar(), etc).
  fn resolve_call_name<'c>(ast: &'c AST) -> Result<&'c str, Error> {
    match ast {
      AST::Symbol(s) => Ok(&*s),
      _ => Err(Error::CannotCall(ast.to_string())),
    }
  }

  fn resolve_special_form(&mut self,
                          builder: &mut StmtBuilder,
                          head: &str,
                          tail: &[&AST])
                          -> Result<Option<Expr>, Error> {
    match head {
      "progn" => {
        if tail.is_empty() {
          panic!("Not implemented yet!") //// Nil case
        } else {
          let prefix = &tail[..tail.len()-1];
          let end = &tail[tail.len()-1];
          for x in prefix {
            self.compile_statement(builder, stmt_wrapper::Vacuous, x)?;
          }
          self.compile_expression(builder, end).map(Some)
        }
      }
      _ => {
        Ok(None)
      }
    }
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::decl::Decl;
  use crate::gdscript::stmt::Stmt;
  use crate::sxp::ast;

  // TODO A lot more of this

  fn compile_stmt(ast: &AST) -> Result<(Vec<Stmt>, Vec<Decl>), Error> {
    let used_names = ast.all_symbols();
    let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));
    let mut builder = StmtBuilder::new();
    let () = compiler.compile_statement(&mut builder, stmt_wrapper::Return, &ast)?;
    Ok(builder.build())
  }

  #[test]
  fn compile_var() {
    let ast = AST::Symbol(String::from("foobar"));
    let expected = Stmt::ReturnStmt(Expr::Var(String::from("foobar")));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_call() {
    let ast = ast::list(vec!(AST::Symbol(String::from("foobar")), AST::Symbol(String::from("arg"))));
    let expected = Stmt::ReturnStmt(Expr::Call(None, String::from("foobar"), vec!(Expr::Var(String::from("arg")))));
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

}
