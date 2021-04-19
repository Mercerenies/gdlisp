
pub mod class;
pub mod instance_method;
pub mod visibility;

use crate::sxp::ast::AST;

// There are a few situations in the compiler where we want to read an
// optional sequence of modifiers from a list and then return the rest
// of the list. Here, we capture that common pattern.

// TODO Can we use this for defvar export statements too?

// Constant is a parse rule which looks for an AST::Symbol with a
// specific string value. If it finds it, it returns a preset value.
// The M type must implement Clone to be usable as a ParseRule.
pub struct Constant<M> {
  pub symbol_value: String,
  pub result: M,
}

// Several is a ParseRule which will attempt to parse all of the
// modifiers in its values field in order, taking the first one which
// succeeds. The Several instance will only fail if all constituent
// parsers fail on the same input.
pub struct Several<M> {
  pub values: Vec<Box<dyn ParseRule<Modifier=M>>>,
}

pub enum ParseError {
  GenericError,
}

// A ParseRule takes an AST and attempts to parse it as a particular
// modifier type. If successful, the modifier is returned. Otherwise,
// ParseError::GenericError is returned (there is only one instance of
// ParseError right now; we may opt to provide more specific errors in
// the future).
pub trait ParseRule {
  type Modifier;

  fn parse_once(&mut self, ast: &AST) -> Result<Self::Modifier, ParseError>;

  // parse(args) takes a slice of AST's and attempts to parse them
  // each using the current parse rule. Whenever a parse fails,
  // parsing stops and the successfully parsed modifiers are returned,
  // alongside the rest of the slice that was not parsed successfully.
  fn parse<'a, 'b>(&mut self, args: &'a [&'b AST]) -> (Vec<Self::Modifier>, &'a [&'b AST]) {
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
  fn parse_once(&mut self, ast: &AST) -> Result<M, ParseError> {
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
  fn parse_once(&mut self, ast: &AST) -> Result<M, ParseError> {
    for value in &mut self.values {
      if let Ok(modifier) = value.parse_once(ast) {
        return Ok(modifier);
      }
    }
    Err(ParseError::GenericError)
  }
}
