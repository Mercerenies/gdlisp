
#![deprecated]

//! All of the built-in GDLisp macros are implemented in Rust here.
//!
//! This module is obsolete and should not be used. Macro resolution
//! is bootstrapped from `GDLisp.lisp` now and does not reference this
//! module. `library::macros` exists for historical reasons only.

use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::compile::error::Error;
use crate::compile::names::fresh::FreshNameGenerator;

use std::convert::TryInto;

pub const ID_AND_FUNCTION:       u32 = 0;
pub const ID_OR_FUNCTION:        u32 = 1;
pub const ID_LETSTAR_FUNCTION:   u32 = 2;
pub const ID_DEFVARS_FUNCTION:   u32 = 3;
pub const ID_WHEN_FUNCTION:      u32 = 4;
pub const ID_UNLESS_FUNCTION:    u32 = 5;
pub const ID_IF_FUNCTION:        u32 = 6;
pub const ID_YIELDSTAR_FUNCTION: u32 = 7;

/// The state given to Rust-side macros.
pub struct MacroState<'a, 'b> {
  pub generator: &'a mut FreshNameGenerator<'b>,
}

/// The type of Rust-side built-in macros.
///
/// Note carefully that this type is the primitive `fn` type, *not*
/// the trait [`Fn`]. All built-in macros are implemented in Rust as
/// concrete top-level functions, not closures or other function-like
/// objects.
pub type BuiltInMacro = fn(MacroState<'_, '_>, &[&AST]) -> Result<AST, Error>;

/// Get the macro with the given ID.
pub fn get_builtin_macro(id: u32) -> Option<BuiltInMacro> {
  match id {
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
    7 => {
      Some(yield_star_function)
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

// TODO Surely some optimizations are in order to get this down to a
// manageable compiled form.
pub fn yield_star_function(state: MacroState<'_, '_>, arg: &[&AST]) -> Result<AST, Error> {
  if arg.is_empty() {
    return Err(Error::TooFewArgs(String::from("yield*"), arg.len()));
  }
  if arg.len() > 1 {
    return Err(Error::TooManyArgs(String::from("yield*"), arg.len()));
  }
  let call = arg[0].clone();
  let symbol = state.generator.generate_with("_yield");

  // (let ((symbol ...))
  //   (while (and (instance? symbol GDScriptFunctionState) (symbol:is_valid))
  //     (yield)
  //     (set symbol (symbol:resume)))
  //   symbol)
  let result = AST::list(vec!(
    AST::symbol("let"),
    AST::list(vec!(AST::list(vec!(AST::symbol(&symbol), call)))),
    AST::list(vec!(
      AST::symbol("while"),
      AST::list(vec!(
        AST::symbol("and"),
        AST::list(vec!(
          AST::symbol("instance?"),
          AST::symbol(&symbol),
          AST::symbol("GDScriptFunctionState"),
        )),
        AST::list(vec!(
          AST::list(vec!(
            AST::symbol("access-slot"),
            AST::symbol(&symbol),
            AST::symbol("is_valid"),
          )),
        )),
      )),
      AST::list(vec!(
        AST::symbol("yield")
      )),
      AST::list(vec!(
        AST::symbol("set"),
        AST::symbol(&symbol),
        AST::list(vec!(
          AST::list(vec!(
            AST::symbol("access-slot"),
            AST::symbol(&symbol),
            AST::symbol("resume"),
          )),
        )),
      )),
    )),
    AST::symbol(&symbol),
  ));

  Ok(result)
}
