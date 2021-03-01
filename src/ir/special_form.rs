
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use super::expr::{Expr, FuncRefTarget};
use super::arglist::ArgList;
use super::quasiquote::quasiquote;
use crate::pipeline::error::Error;
use crate::compile::error::{Error as GDError};
use crate::ir::incremental::IncCompiler;

use std::convert::TryInto;

pub fn dispatch_form(icompiler: &mut IncCompiler,
                     head: &str,
                     tail: &[&AST])
                     -> Result<Option<Expr>, Error> {
  match head {
    "progn" => progn_form(icompiler, tail).map(Some),
    "if" => if_form(icompiler, tail).map(Some),
    "cond" => cond_form(icompiler, tail).map(Some),
    "while" => while_form(icompiler, tail).map(Some),
    "for" => for_form(icompiler, tail).map(Some),
    "let" => let_form(icompiler, tail).map(Some),
    "flet" => flet_form(icompiler, tail, Expr::FLet).map(Some),
    "labels" => flet_form(icompiler, tail, Expr::Labels).map(Some),
    "lambda" => lambda_form(icompiler, tail).map(Some),
    "function" => function_form(tail).map(Some),
    "setq" => assign_form(icompiler, tail).map(Some),
    "quote" => quote_form(tail).map(Some),
    "quasiquote" => quasiquote_form(icompiler, tail).map(Some),
    "unquote" => Err(Error::from(GDError::UnquoteOutsideQuasiquote)),
    "access-slot" => access_slot_form(icompiler, tail).map(Some),
    _ => Ok(None),
  }
}

pub fn progn_form(icompiler: &mut IncCompiler,
                  tail: &[&AST])
                  -> Result<Expr, Error> {
  let body = tail.iter().map(|expr| icompiler.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Progn(body))
}

pub fn if_form(icompiler: &mut IncCompiler,
               tail: &[&AST])
               -> Result<Expr, Error> {
  let (cond, t, f) = match tail {
    [] | [_] => Err(Error::from(GDError::TooFewArgs(String::from("if"), tail.len()))),
    [cond, t] => Ok((*cond, *t, &AST::Nil)),
    [cond, t, f] => Ok((*cond, *t, *f)),
    _ => Err(Error::from(GDError::TooManyArgs(String::from("if"), tail.len()))),
  }?;
  let cond = icompiler.compile_expr(cond)?;
  let t = icompiler.compile_expr(t)?;
  let f = icompiler.compile_expr(f)?;
  Ok(Expr::if_stmt(cond, t, f))
}

pub fn cond_form(icompiler: &mut IncCompiler,
                 tail: &[&AST])
                 -> Result<Expr, Error> {
  let body = tail.iter().map(|clause| {
    let vec: Vec<&AST> = DottedExpr::new(clause).try_into()?;
    match vec.len() {
      0 => {
        Err(Error::from(GDError::InvalidArg(String::from("cond"), (*clause).clone(), String::from("nonempty list"))))
      }
      1 => {
        let cond = icompiler.compile_expr(vec[0])?;
        Ok((cond, None))
      }
      _ => {
        let cond = icompiler.compile_expr(vec[0])?;
        let inner = vec[1..].iter().map(|expr| icompiler.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
        Ok((cond, Some(Expr::Progn(inner))))
      }
    }
  }).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::CondStmt(body))
}

pub fn while_form(icompiler: &mut IncCompiler,
                  tail: &[&AST])
                  -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("while"), tail.len())));
  }
  let cond = icompiler.compile_expr(tail[0])?;
  let body = tail[1..].iter().map(|x| icompiler.compile_expr(x)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::while_stmt(cond, Expr::Progn(body)))
}

pub fn for_form(icompiler: &mut IncCompiler,
                tail: &[&AST])
                -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("for"), tail.len())));
  }
  let name = match tail[0] {
    AST::Symbol(s) => s.to_owned(),
    _ => return Err(Error::from(GDError::InvalidArg(String::from("for"), (*tail[0]).clone(), String::from("variable name")))),
  };
  let iter = icompiler.compile_expr(tail[1])?;
  let body = tail[2..].iter().map(|x| icompiler.compile_expr(x)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::for_stmt(name, iter, Expr::Progn(body)))
}

pub fn let_form(icompiler: &mut IncCompiler,
                tail: &[&AST])
                -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("let"), tail.len())));
  }
  let vars: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let var_clauses = vars.into_iter().map(|clause| {
    let var: Vec<_> = match DottedExpr::new(clause) {
      DottedExpr { elements, terminal: AST::Nil } if !elements.is_empty() => elements,
      DottedExpr { elements, terminal: tail@AST::Symbol(_) } if elements.is_empty() => vec!(tail),
      _ => return Err(Error::from(GDError::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration"))))
    };
    let result_value = var[1..].iter().map(|e| icompiler.compile_expr(e)).collect::<Result<Vec<_>, _>>()?;
    let name = match var[0] {
      AST::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::from(GDError::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration")))),
    }?;
    Ok((name, Expr::Progn(result_value)))
  }).collect::<Result<Vec<_>, _>>()?;
  let body = tail[1..].iter().map(|expr| icompiler.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Let(var_clauses, Box::new(Expr::Progn(body))))
}

pub fn lambda_form(icompiler: &mut IncCompiler,
                   tail: &[&AST])
                   -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("lambda"), 1)));
  }
  let args: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let args = ArgList::parse(args)?;
  let body = tail[1..].iter().map(|expr| icompiler.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Lambda(args, Box::new(Expr::Progn(body))))
}

pub fn function_form(tail: &[&AST])
                     -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("function"), 1)));
  }
  if tail.len() > 1 {
    return Err(Error::from(GDError::TooManyArgs(String::from("function"), 1)));
  }
  match tail[0] {
    AST::Symbol(s) => {
      Ok(Expr::FuncRef(FuncRefTarget::SimpleName(s.clone())))
    }
    x => {
      Err(Error::from(GDError::InvalidArg(String::from("function"), x.clone(), String::from("symbol"))))
    }
  }
}

pub fn assign_form(icompiler: &mut IncCompiler,
                   tail: &[&AST])
                   -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("setq"), 2)))
  }
  if tail.len() > 2 {
    return Err(Error::from(GDError::TooManyArgs(String::from("setq"), 2)))
  }
  let var_name = match tail[0] {
    AST::Symbol(s) => s,
    x => return Err(Error::from(GDError::InvalidArg(String::from("setq"), x.clone(), String::from("symbol")))),
  };
  let value = icompiler.compile_expr(tail[1])?;
  Ok(Expr::Assign(var_name.clone(), Box::new(value)))
}

pub fn flet_form(icompiler: &mut IncCompiler,
                 tail: &[&AST],
                 container: impl FnOnce(Vec<(String, ArgList, Expr)>, Box<Expr>) -> Expr)
                 -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("flet"), tail.len())));
  }
  let fns: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let fn_clauses = fns.into_iter().map(|clause| {
    let func: Vec<_> = DottedExpr::new(clause).try_into()?;
    if func.len() < 2 {
      return Err(Error::from(GDError::InvalidArg(String::from("flet"), clause.clone(), String::from("function declaration"))));
    }
    let name = match func[0] {
      AST::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::from(GDError::InvalidArg(String::from("flet"), (*clause).clone(), String::from("function declaration")))),
    }?;
    let args: Vec<_> = DottedExpr::new(func[1]).try_into()?;
    let args = ArgList::parse(args)?;
    let body = func[2..].iter().map(|expr| icompiler.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
    Ok((name, args, Expr::Progn(body)))
  }).collect::<Result<Vec<_>, _>>()?;
  let body = tail[1..].iter().map(|expr| icompiler.compile_expr(expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(container(fn_clauses, Box::new(Expr::Progn(body))))
}

pub fn quote_form(tail: &[&AST]) -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("quote"), 1)))
  }
  if tail.len() > 1 {
    return Err(Error::from(GDError::TooManyArgs(String::from("quote"), 1)))
  }
  Ok(Expr::Quote(tail[0].clone()))
}

pub fn quasiquote_form(icompiler: &mut IncCompiler, tail: &[&AST]) -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("quasiquote"), 1)))
  }
  if tail.len() > 1 {
    return Err(Error::from(GDError::TooManyArgs(String::from("quasiquote"), 1)))
  }
  quasiquote(icompiler, tail[0])
}

pub fn access_slot_form(icompiler: &mut IncCompiler, tail: &[&AST]) -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("access-slot"), 2)))
  }
  if tail.len() > 2 {
    return Err(Error::from(GDError::TooManyArgs(String::from("access-slot"), 2)))
  }
  let lhs = icompiler.compile_expr(tail[0])?;
  let slot_name = match tail[1] {
    AST::Symbol(s) => s.to_owned(),
    _ => return Err(Error::from(GDError::InvalidArg(String::from("access-slot"), tail[1].clone(), String::from("symbol")))),
  };
  Ok(Expr::FieldAccess(Box::new(lhs), slot_name))
}
