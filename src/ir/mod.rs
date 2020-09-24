
// Intermediate representation used between the AST Lisp syntax and
// the GDScript syntax. This representation shares all of the nice
// expression semantics of Lisp but resolves any special forms to
// something easily recognizable.
//
// NOTE: IR still exclusively uses the GDLisp names. Generating
// GDScript names is the responsibility of the next compilation step.

pub mod expr;
pub mod literal;

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
//          Ok(Expr::Call(None, head, args))
          panic!("Not implemented yet!") ////
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
          Expr::Var(var.to_string())
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
  Ok(None)
}

fn compile_builtin_call(table: &mut impl SymbolTable,
                        head: &str,
                        tail: &[&AST])
                        -> Result<Option<Expr>, Error> {
  Ok(None)
}
