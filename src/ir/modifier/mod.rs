
pub mod class;
pub mod instance_method;

use crate::sxp::ast::AST;

// There are a few situations in the compiler where we want to read an
// optional sequence of modifiers from a list and then return the rest
// of the list. Here, we capture that common pattern.

// TODO Can we use this for defvar export statements too?

pub struct Constant<M> {
  pub symbol_value: String,
  pub result: M,
}

pub struct Several<M> {
  pub values: Vec<Box<dyn ParseRule<Modifier=M>>>,
}

pub enum ParseError {
  GenericError,
}

pub trait ParseRule {
  type Modifier;

  fn parse_once(&self, ast: &AST) -> Result<Self::Modifier, ParseError>;

  fn parse<'a, 'b>(&self, args: &'a [&'b AST]) -> (Vec<Self::Modifier>, &'a [&'b AST]) {
    let mut modifiers = Vec::new();

    let mut position = 0;
    while position < args.len() {
      let curr = args[position];
      match self.parse_once(curr) {
        Err(_) => break,
        Ok(m) => modifiers.push(m),
      }
      position += 1;
    }

    (modifiers, &args[position..])
  }

}

impl<M> Constant<M> {
  pub fn new(symbol_value: &str, result: M) -> Constant<M> {
    Constant {
      symbol_value: String::from(symbol_value),
      result
    }
  }
}

impl<M> ParseRule for Constant<M> where M: Clone {
  type Modifier = M;
  fn parse_once(&self, ast: &AST) -> Result<M, ParseError> {
    if let AST::Symbol(s) = ast {
      if *s == self.symbol_value {
        return Ok(self.result.clone());
      }
    }
    Err(ParseError::GenericError)
  }
}

impl<M> Several<M> {
  pub fn new(values: Vec<Box<dyn ParseRule<Modifier=M>>>) -> Several<M> {
    Several { values }
  }
}

impl<M> ParseRule for Several<M> {
  type Modifier = M;
  fn parse_once(&self, ast: &AST) -> Result<M, ParseError> {
    for value in &self.values {
      if let Ok(modifier) = value.parse_once(ast) {
        return Ok(modifier);
      }
    }
    Err(ParseError::GenericError)
  }
}
