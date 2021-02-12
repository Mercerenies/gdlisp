
// Incremental compilation (supplies backbone for macro resolution)

use super::symbol_table::SymbolTable;
use super::{MAIN_BODY_NAME, resolve_call_name};
use super::arglist::ArgList;
use super::literal::Literal;
use super::expr::Expr;
use super::special_form;
use super::decl::{self, Decl};
use crate::sxp::dotted::DottedExpr;
use crate::sxp::ast::AST;
use crate::compile::error::Error;

use std::convert::TryInto;
use std::borrow::Borrow;

pub struct IncCompiler {
  symbols: SymbolTable,
}

impl IncCompiler {

  pub fn new() -> IncCompiler {
    IncCompiler { symbols: SymbolTable::new() }
  }

  pub fn resolve_simple_call(&self, head: &str, tail: &[&AST]) -> Result<Expr, Error> {
    if let Some(sf) = special_form::dispatch_form(head, tail)? {
      Ok(sf)
    } else {
      let args = tail.iter().map(|x| self.compile_expr(x)).collect::<Result<Vec<_>, _>>()?;
      Ok(Expr::Call(head.to_owned(), args))
    }
  }

  pub fn compile_expr(&self, expr: &AST) -> Result<Expr, Error> {
    match expr {
      AST::Nil | AST::Cons(_, _) => {
        let vec: Vec<&AST> = DottedExpr::new(expr).try_into()?;
        if vec.is_empty() {
          Ok(Expr::Literal(Literal::Nil))
        } else {
          let head = resolve_call_name(vec[0])?;
          let tail = &vec[1..];
          self.resolve_simple_call(head, tail)
        }
      }
      AST::Array(vec) => {
        let vec = vec.iter().map(|e| self.compile_expr(e)).collect::<Result<Vec<_>, _>>()?;
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

  pub fn compile_decl(&self, decl: &AST)
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
            let body = vec[3..].iter().map(|expr| self.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
            Ok(Decl::FnDecl(decl::FnDecl {
              name: name.to_owned(),
              args: args,
              body: Expr::Progn(body),
            }))
          }
          "defmacro" => {
            if vec.len() < 3 {
              return Err(Error::InvalidDecl(decl.clone()));
            }
            let name = match vec[1] {
              AST::Symbol(s) => s,
              _ => return Err(Error::InvalidDecl(decl.clone())),
            };
            let args: Vec<_> = DottedExpr::new(vec[2]).try_into()?;
            let args = ArgList::parse(args)?;
            let body = vec[3..].iter().map(|expr| self.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
            Ok(Decl::MacroDecl(decl::MacroDecl {
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

  pub fn compile_toplevel(mut self, body: &AST)
                          -> Result<Vec<Decl>, Error> {
    let body: Vec<_> = DottedExpr::new(body).try_into()?;
    let mut main: Vec<Expr> = Vec::new();
    for curr in body {
      match self.compile_decl(curr) {
        Ok(d) => self.symbols.set(d.name().to_owned(), d),
        Err(Error::UnknownDecl(_)) => main.push(self.compile_expr(curr)?),
        Err(e) => return Err(e),
      }
    }
    let main_decl = Decl::FnDecl(decl::FnDecl {
      name: MAIN_BODY_NAME.to_owned(),
      args: ArgList::empty(),
      body: Expr::Progn(main),
    });
    self.symbols.set(MAIN_BODY_NAME.to_owned(), main_decl);
    Ok(self.into())
  }

}

impl Default for IncCompiler {

  fn default() -> IncCompiler {
    IncCompiler::new()
  }

}

impl From<IncCompiler> for Vec<Decl> {

  fn from(compiler: IncCompiler) -> Vec<Decl> {
    compiler.symbols.into()
  }

}
