
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use super::expr::{self, Expr};
//use super::literal::Literal;
use crate::compile::symbol_table::{SymbolTable, HasSymbolTable};
use crate::compile::error::Error;
use crate::ir::compile_expr;

use std::convert::TryInto;

pub fn dispatch_form(table: &mut impl SymbolTable,
                     head: &str,
                     tail: &[&AST])
                     -> Result<Option<Expr>, Error> {
  match head {
    "progn" => progn_form(table, tail).map(Some),
    "if" => if_form(table, tail).map(Some),
    "cond" => cond_form(table, tail).map(Some),
    "let" => let_form(table, tail).map(Some),
    _ => Ok(None),
  }
}

pub fn progn_form(table: &mut impl SymbolTable,
                  tail: &[&AST])
                  -> Result<Expr, Error> {
  let body = tail.into_iter().map(|expr| compile_expr(expr, table)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Progn(body))
}

pub fn if_form(table: &mut impl SymbolTable,
               tail: &[&AST])
               -> Result<Expr, Error> {
  let (cond, t, f) = match tail {
    [] | [_] => Err(Error::TooFewArgs(String::from("if"), tail.len())),
    [cond, t] => Ok((*cond, *t, &AST::Nil)),
    [cond, t, f] => Ok((*cond, *t, *f)),
    _ => Err(Error::TooManyArgs(String::from("if"), tail.len())),
  }?;
  let cond = compile_expr(cond, table)?;
  let t = compile_expr(t, table)?;
  let f = compile_expr(f, table)?;
  Ok(expr::if_stmt(cond, t, f))
}

pub fn cond_form(table: &mut impl SymbolTable,
                 tail: &[&AST])
                 -> Result<Expr, Error> {
  let body = tail.iter().map(|clause| {
    let vec: Vec<&AST> = DottedExpr::new(clause).try_into()?;
    match vec.len() {
      0 => {
        Err(Error::InvalidArg(String::from("cond"), (*clause).clone(), String::from("nonempty list")))
      }
      1 => {
        let cond = compile_expr(vec[0], table)?;
        Ok((cond, None))
      }
      _ => {
        let cond = compile_expr(vec[0], table)?;
        let inner = vec[1..].into_iter().map(|expr| compile_expr(expr, table)).collect::<Result<Vec<_>, _>>()?;
        Ok((cond, Some(Expr::Progn(inner))))
      }
    }
  }).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::CondStmt(body))
}

pub fn let_form(table: &mut impl SymbolTable,
                tail: &[&AST])
                -> Result<Expr, Error> {
  if tail.len() < 1 {
    return Err(Error::TooFewArgs(String::from("let"), tail.len()));
  }
  let vars: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let var_clauses = vars.into_iter().map(|clause| {
    let var: Vec<_> = match DottedExpr::new(clause) {
      DottedExpr { elements, terminal: AST::Nil } if elements.len() > 0 => elements,
      DottedExpr { elements, terminal: tail@AST::Symbol(_) } if elements.len() == 0 => vec!(tail),
      _ => return Err(Error::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration")))
    };
    let result_value = var[1..].into_iter().map(|e| compile_expr(e, table)).collect::<Result<Vec<_>, _>>()?;
    let name = match var[0] {
      AST::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration"))),
    }?;
    Ok((name, Expr::Progn(result_value)))
  }).collect::<Result<Vec<_>, _>>()?;
  let mut names = var_clauses.iter().map(|x| (x.0.to_owned(), x.0.to_owned()));
  let body =
    table.with_local_vars(&mut names, |table| {
      tail[1..].into_iter().map(|expr| compile_expr(expr, table)).collect::<Result<Vec<_>, _>>()
    })?;
  Ok(Expr::Let(var_clauses, Box::new(Expr::Progn(body))))
}
