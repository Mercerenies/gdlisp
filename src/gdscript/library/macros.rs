
use crate::runner::macro_server::named_file_server::MacroID;
use crate::sxp::ast::{self, AST};
use crate::compile::error::Error;

pub const ID_AND_FUNCTION: u32 = 0;
pub const ID_OR_FUNCTION:  u32 = 1;

pub fn get_builtin_macro(id: MacroID) -> Option<fn(&[&AST]) -> Result<AST, Error>> {
  match id.0 {
    0 => {
      Some(and_function)
    }
    1 => {
      Some(or_function)
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
