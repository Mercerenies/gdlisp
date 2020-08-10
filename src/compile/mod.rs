
pub mod names;
pub mod body;
pub mod error;

use body::builder::StmtBuilder;
use names::fresh::FreshNameGenerator;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;
use crate::gdscript::literal::Literal;
use error::Error;

use std::convert::TryInto;

// Note that we are NOT consuming the AST here. This means that (at
// least for atoms) we'll be doing some copying, especially of strings
// and the like. This is fine; I don't want the compiled form and the
// original AST to be sharing responsibilities for the same data.

pub struct Compiler<'a, 'b> {
  gen: &'a mut FreshNameGenerator<'b>
}

impl<'a, 'b> Compiler<'a, 'b> {

  pub fn new(gen: &'a mut FreshNameGenerator<'b>) -> Compiler<'a, 'b> {
    Compiler { gen }
  }

  pub fn compile_statement(&mut self,
                           builder: &mut StmtBuilder,
                           mut destination: impl FnMut(Expr) -> Stmt,
                           stmt: &AST)
                           -> Result<(), Error> {
    let expr = self.compile_expression(builder, stmt)?;
    builder.append(destination(expr));
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
          let args = tail.into_iter().map(|x| self.compile_expression(builder, x)).collect::<Result<_, _>>()?;
          Ok(Expr::Call(None, names::lisp_to_gd(head), args))
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

}
