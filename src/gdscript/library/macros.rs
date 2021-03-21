
use crate::runner::macro_server::named_file_server::MacroID;
use crate::sxp::ast::{self, AST};
use crate::sxp::dotted::DottedExpr;
use crate::compile::error::Error;

use std::convert::TryInto;

pub const ID_AND_FUNCTION:      u32 = 0;
pub const ID_OR_FUNCTION:       u32 = 1;
pub const ID_LETSTAR_FUNCTION:  u32 = 2;
pub const ID_DEFVARS_FUNCTION:  u32 = 3;

pub fn get_builtin_macro(id: MacroID) -> Option<fn(&[&AST]) -> Result<AST, Error>> {
  match id.0 {
    0 => {
      Some(and_function)
    }
    1 => {
      Some(or_function)
    }
    2 => {
      Some(let_star_function)
    }
    3 => {
      Some(defvars_function)
    }
    _ => {
      None
    }
  }
}

pub fn or_function(arg: &[&AST]) -> Result<AST, Error> {
  let mut iter = arg.iter().rev();
  if let Some(fst) = iter.next() {
    // Some arguments provided
    let mut result = ast::list(vec!(ast::list(vec!(AST::Bool(true), (*fst).clone()))));
    for x in iter {
      let car = ast::list(vec!((*x).clone()));
      result = AST::Cons(Box::new(car), Box::new(result));
    }
    Ok(AST::Cons(Box::new(AST::Symbol(String::from("cond"))), Box::new(result)))
  } else {
    // No arguments provided
    Ok(AST::Bool(false))
  }
}

pub fn and_function(arg: &[&AST]) -> Result<AST, Error> {
  let mut iter = arg.iter().rev();
  if let Some(fst) = iter.next() {
    // Some arguments provided
    let mut result = ast::list(vec!(ast::list(vec!(AST::Bool(true), (*fst).clone()))));
    for x in iter {
      let cond = ast::list(vec!(AST::Symbol(String::from("not")), (*x).clone()));
      let car = ast::list(vec!(cond, AST::Bool(false)));
      result = AST::Cons(Box::new(car), Box::new(result));
    }
    Ok(AST::Cons(Box::new(AST::Symbol(String::from("cond"))), Box::new(result)))
  } else {
    // No arguments provided
    Ok(AST::Bool(true))
  }
}

pub fn let_star_function(arg: &[&AST]) -> Result<AST, Error> {
  if arg.is_empty() {
    return Err(Error::TooFewArgs(String::from("let*"), arg.len()));
  }
  let vars: Vec<_> = DottedExpr::new(arg[0]).try_into()?;
  let body: Vec<_> = arg[1..].iter().map(|x| (*x).clone()).collect();
  let mut body = ast::cons(AST::Symbol(String::from("progn")), ast::list(body));
  for var in vars.into_iter().rev() {
    body = ast::list(vec!(AST::Symbol(String::from("let")),
                          ast::list(vec!(var.clone())),
                          body));
  }
  Ok(body)
}

pub fn defvars_function(arg: &[&AST]) -> Result<AST, Error> {
  let body: Vec<_> = arg[0..].iter().map(|x| ast::list(vec!(AST::Symbol(String::from("defvar")), (*x).clone()))).collect();
  let body = ast::cons(AST::Symbol(String::from("progn")), ast::list(body));
  Ok(body)
}
