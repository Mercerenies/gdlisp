
use crate::runner::macro_server::named_file_server::MacroID;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::compile::error::Error;
use crate::compile::names::fresh::FreshNameGenerator;

use std::convert::TryInto;

pub const ID_AND_FUNCTION:      u32 = 0;
pub const ID_OR_FUNCTION:       u32 = 1;
pub const ID_LETSTAR_FUNCTION:  u32 = 2;
pub const ID_DEFVARS_FUNCTION:  u32 = 3;
pub const ID_WHEN_FUNCTION:     u32 = 4;
pub const ID_UNLESS_FUNCTION:   u32 = 5;
pub const ID_IF_FUNCTION:       u32 = 6;

pub struct MacroState<'a, 'b> {
  pub generator: &'a mut FreshNameGenerator<'b>,
}

///// Get FreshNameGenerator down here so built-in macros can gensym correctly (then maybe yield*)
pub type BuiltInMacro = fn(MacroState<'_, '_>, &[&AST]) -> Result<AST, Error>;

pub fn get_builtin_macro(id: MacroID) -> Option<BuiltInMacro> {
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
    4 => {
      Some(when_function)
    }
    5 => {
      Some(unless_function)
    }
    6 => {
      Some(if_function)
    }
    _ => {
      None
    }
  }
}

pub fn or_function(_state: MacroState<'_, '_>, arg: &[&AST]) -> Result<AST, Error> {
  let mut iter = arg.iter().rev();
  if let Some(fst) = iter.next() {
    // Some arguments provided
    let mut result = AST::list(vec!(AST::list(vec!(AST::Bool(true), (*fst).clone()))));
    for x in iter {
      let car = AST::list(vec!((*x).clone()));
      result = AST::Cons(Box::new(car), Box::new(result));
    }
    Ok(AST::Cons(Box::new(AST::Symbol(String::from("cond"))), Box::new(result)))
  } else {
    // No arguments provided
    Ok(AST::Bool(false))
  }
}

pub fn and_function(_state: MacroState<'_, '_>, arg: &[&AST]) -> Result<AST, Error> {
  let mut iter = arg.iter().rev();
  if let Some(fst) = iter.next() {
    // Some arguments provided
    let mut result = AST::list(vec!(AST::list(vec!(AST::Bool(true), (*fst).clone()))));
    for x in iter {
      let cond = AST::list(vec!(AST::Symbol(String::from("not")), (*x).clone()));
      let car = AST::list(vec!(cond, AST::Bool(false)));
      result = AST::Cons(Box::new(car), Box::new(result));
    }
    Ok(AST::Cons(Box::new(AST::Symbol(String::from("cond"))), Box::new(result)))
  } else {
    // No arguments provided
    Ok(AST::Bool(true))
  }
}

pub fn let_star_function(_state: MacroState<'_, '_>, arg: &[&AST]) -> Result<AST, Error> {
  if arg.is_empty() {
    return Err(Error::TooFewArgs(String::from("let*"), arg.len()));
  }
  let vars: Vec<_> = DottedExpr::new(arg[0]).try_into()?;
  let body: Vec<_> = arg[1..].iter().map(|x| (*x).clone()).collect();
  let mut body = AST::cons(AST::symbol("progn"), AST::list(body));
  for var in vars.into_iter().rev() {
    body = AST::list(vec!(AST::Symbol(String::from("let")),
                          AST::list(vec!(var.clone())),
                          body));
  }
  Ok(body)
}

pub fn defvars_function(_state: MacroState<'_, '_>, arg: &[&AST]) -> Result<AST, Error> {
  let body: Vec<_> = arg[0..].iter().map(|x| AST::list(vec!(AST::Symbol(String::from("defvar")), (*x).clone()))).collect();
  let body = AST::cons(AST::symbol("progn"), AST::list(body));
  Ok(body)
}

pub fn when_function(_state: MacroState<'_, '_>, arg: &[&AST]) -> Result<AST, Error> {
  if arg.is_empty() {
    return Err(Error::TooFewArgs(String::from("when"), arg.len()));
  }
  let cond = arg[0].clone();
  let body = AST::list(arg[1..].iter().map(|x| (*x).clone()).collect());
  Ok(AST::list(vec!(AST::symbol("if"), cond, AST::cons(AST::symbol("progn"), body), AST::Nil)))
}

pub fn unless_function(_state: MacroState<'_, '_>, arg: &[&AST]) -> Result<AST, Error> {
  if arg.is_empty() {
    return Err(Error::TooFewArgs(String::from("unless"), arg.len()));
  }
  let cond = arg[0].clone();
  let body = AST::list(arg[1..].iter().map(|x| (*x).clone()).collect());
  Ok(AST::list(vec!(AST::symbol("if"), cond, AST::Nil, AST::cons(AST::symbol("progn"), body))))
}

pub fn if_function(_state: MacroState<'_, '_>, arg: &[&AST]) -> Result<AST, Error> {
  if arg.len() < 2 {
    return Err(Error::TooFewArgs(String::from("if"), arg.len()));
  }
  if arg.len() > 3 {
    return Err(Error::TooManyArgs(String::from("if"), arg.len()));
  }
  let cond = arg[0].clone();
  let true_case = arg[1].clone();
  let false_case = arg.get(2).map(|x| (*x).clone());

  let mut result = vec!(AST::symbol("cond"));
  result.push(AST::list(vec!(cond, true_case)));
  if let Some(false_case) = false_case {
    result.push(AST::list(vec!(AST::Bool(true), false_case)));
  }

  Ok(AST::list(result))
}
