
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use super::expr::Expr;
use super::arglist::ArgList;
use crate::compile::error::Error;
use crate::ir::compile_expr;

use std::convert::TryInto;

pub fn dispatch_form(head: &str,
                     tail: &[&AST])
                     -> Result<Option<Expr>, Error> {
  match head {
    "progn" => progn_form(tail).map(Some),
    "if" => if_form(tail).map(Some),
    "cond" => cond_form(tail).map(Some),
    "let" => let_form(tail).map(Some),
    "lambda" => lambda_form(tail).map(Some),
    _ => Ok(None),
  }
}

pub fn progn_form(tail: &[&AST])
                  -> Result<Expr, Error> {
  let body = tail.into_iter().map(|expr| compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Progn(body))
}

pub fn if_form(tail: &[&AST])
               -> Result<Expr, Error> {
  let (cond, t, f) = match tail {
    [] | [_] => Err(Error::TooFewArgs(String::from("if"), tail.len())),
    [cond, t] => Ok((*cond, *t, &AST::Nil)),
    [cond, t, f] => Ok((*cond, *t, *f)),
    _ => Err(Error::TooManyArgs(String::from("if"), tail.len())),
  }?;
  let cond = compile_expr(cond)?;
  let t = compile_expr(t)?;
  let f = compile_expr(f)?;
  Ok(Expr::if_stmt(cond, t, f))
}

pub fn cond_form(tail: &[&AST])
                 -> Result<Expr, Error> {
  let body = tail.iter().map(|clause| {
    let vec: Vec<&AST> = DottedExpr::new(clause).try_into()?;
    match vec.len() {
      0 => {
        Err(Error::InvalidArg(String::from("cond"), (*clause).clone(), String::from("nonempty list")))
      }
      1 => {
        let cond = compile_expr(vec[0])?;
        Ok((cond, None))
      }
      _ => {
        let cond = compile_expr(vec[0])?;
        let inner = vec[1..].into_iter().map(|expr| compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
        Ok((cond, Some(Expr::Progn(inner))))
      }
    }
  }).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::CondStmt(body))
}

pub fn let_form(tail: &[&AST])
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
    let result_value = var[1..].into_iter().map(|e| compile_expr(e)).collect::<Result<Vec<_>, _>>()?;
    let name = match var[0] {
      AST::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration"))),
    }?;
    Ok((name, Expr::Progn(result_value)))
  }).collect::<Result<Vec<_>, _>>()?;
  let body = tail[1..].into_iter().map(|expr| compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Let(var_clauses, Box::new(Expr::Progn(body))))
}

pub fn lambda_form(tail: &[&AST])
                   -> Result<Expr, Error> {
  if tail.len() <= 0 {
    return Err(Error::TooFewArgs(String::from("lambda"), 1));
  }
  let args: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let args = ArgList::parse(args)?;
  let body = tail[1..].into_iter().map(|expr| compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Lambda(args, Box::new(Expr::Progn(body))))
}
