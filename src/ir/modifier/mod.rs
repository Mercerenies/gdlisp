
//! Framework for parsing and applying modifiers to various types.
//!
//! There are a few situations in the compiler where we want to read
//! an optional sequence of modifiers from a list and then return the
//! rest of the list. The [`ParseRule`] trait captures this pattern.

pub mod class;
pub mod instance_method;
pub mod function;
pub mod macros;
pub mod constant;
pub mod enums;
pub mod visibility;
pub mod magic;
pub mod file;
pub mod declare;

use crate::sxp::ast::AST;

use std::fmt;

// TODO Can we use this for defvar export statements too?

/// `Constant` is a [`ParseRule`] which looks for an [`AST::Symbol`]
/// with a specific string value. If it finds it, it returns a preset
/// value. The `M` type must implement [`Clone`] to be usable as a
/// `ParseRule`.
pub struct Constant<M> {
  /// The symbol value to look for.
  pub symbol_value: String,
  /// The value to return on successful parse.
  pub result: M,
}

/// `Several` is a [`ParseRule`] which will attempt to parse all of
/// the modifiers in its values field in order, taking the first one
/// which succeeds. The `Several` instance will only fail if all
/// constituent parsers fail on the same input.
pub struct Several<'a, M> {
  /// The `Several` instance's name, used to generate friendly error
  /// messages.
  pub name: String,
  /// The parsers to try in order. All parse rules must have the same
  /// `Modifier` type.
  pub values: Vec<Box<dyn ParseRule<Modifier=M> + 'a>>,
}

/// `Map` is a [`ParseRule`] which will perform another parse rule and
/// then apply a function to the result, effectively postprocessing
/// the value. This rule is constructed using [`ParseRule::map`].
pub struct Map<R, F> {
  rule: R,
  function: F,
}

/// `Unique` is a [`ParseRule`] which keeps track of whether or not
/// its inner parse rule has been tripped before. If it has and it
/// would trigger a second time successfully, an appropriate error is
/// signaled. This rule is constructed using ParseRule::unique.
pub struct Unique<R> {
  triggered: bool,
  rule: R,
}

/// The type of errors that can occur during parsing modifiers.
///
/// There are two broad categories of errors in parsing modifiers:
/// fatal and non-fatal. A non-fatal error can be recovered by
/// alternative cases, such as a [`Several`] instance with several
/// options. However, if a fatal error occurs at any point during
/// parsing, it is assumed to be critical and immediately fails the
/// entire parse, regardless of alternatives.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
  /// A `UniquenessError` occurs when a parser wrapped in [`Unique`]
  /// triggers successfully twice in the same parse. This error is
  /// fatal.
  UniquenessError(String),
  /// An error in which the parser was expecting something, but some
  /// non-matching `AST` was found instead.
  Expecting(String, AST),
  /// Generic error which is triggered when a [`Several`] exhausts its
  /// options.
  ExhaustedAlternatives,
}

/// A [`ParseRule`] takes an [`AST`] and attempts to parse it as a
/// particular modifier type. If successful, the modifier is returned.
/// Otherwise, a [`ParseError`] is returned.
pub trait ParseRule {

  /// The type of modifier to be returned. The interpretation of this
  /// type is entirely dependent on context, and no explicit
  /// requirements are placed on it.
  type Modifier;

  /// Attempts to parse the given AST using the parse rule.
  fn parse_once(&mut self, ast: &AST) -> Result<Self::Modifier, ParseError>;

  /// The name of a parse rule is used to generate more friendly error
  /// messages and does not affect the act of parsing itself.
  fn name(&self) -> &str;

  /// `parse` takes a slice of AST's and attempts to parse them each
  /// using the current parse rule. Whenever a parse fails, parsing
  /// stops.
  ///
  /// If the parse failed with a non-fatal (continuable) error, then
  /// the successfully parsed modifiers are returned, along with the
  /// rest of the slice that was not parsed successfully. If the parse
  /// failed with a fatal error, then that error is returned instead.
  fn parse<'a, 'b>(&mut self, args: &'a [&'b AST]) -> Result<(Vec<Self::Modifier>, &'a [&'b AST]), ParseError> {
    let mut modifiers = Vec::new();

    let mut position = 0;
    while position < args.len() {
      let curr = args[position];
      match self.parse_once(curr) {
        Err(e) if e.is_fatal() => return Err(e),
        Err(_) => break,
        Ok(m) => modifiers.push(m),
      }
      position += 1;
    }

    Ok((modifiers, &args[position..]))
  }

  /// Produces a [`ParseRule`] which performs `self`, then applies `f`
  /// to the result. Errors (fatal and non-fatal alike) will be
  /// propagated through to the new parse rule.
  fn map<N, F>(self, f: F) -> Map<Self, F>
  where Self : Sized,
        F : FnMut(Self::Modifier) -> N {
    Map { rule: self, function: f }
  }

  /// Produces a [`Unique`] parser representing `self`. A `Unique`
  /// parser will parse `self` successfully only once. If `self`
  /// parses successfully twice or more, a fatal
  /// [`ParseError::UniquenessError`] will be signaled.
  fn unique(self) -> Unique<Self>
  where Self : Sized {
    Unique { triggered: false, rule: self }
  }

}

impl ParseError {

  /// Fatal errors should abort the entire parse process, not allowing
  /// any alternatives to run. Non-fatal errors allow alternative
  /// parse attempts to be run.
  pub fn is_fatal(&self) -> bool {
    match self {
      ParseError::UniquenessError(_) => true,
      ParseError::Expecting(_, _) => false,
      ParseError::ExhaustedAlternatives => false,
    }
  }

}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ParseError::UniquenessError(e) =>
        write!(f, "Got duplicate modifiers for {}, expecting at most one", e),
      ParseError::Expecting(expected, actual) =>
        write!(f, "Expecting modifier '{}', found {}", expected, actual),
      ParseError::ExhaustedAlternatives =>
        write!(f, "Invalid modifier, no alternatives matched"),
    }
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
    Err(ParseError::Expecting(self.symbol_value.clone(), ast.clone()))
  }

  fn name(&self) -> &str {
    &self.symbol_value
  }

}

impl<'a, M> Several<'a, M> {

  pub fn new(values: Vec<Box<dyn ParseRule<Modifier=M> + 'a>>) -> Several<'a, M> {
    Several { name: String::from("(union parse rule)"), values }
  }

  /// Assigns a given name to `self` and returns it.
  pub fn named(mut self, name: &str) -> Self {
    self.name = String::from(name);
    self
  }

}

impl<'a, M> ParseRule for Several<'a, M> {
  type Modifier = M;

  fn parse_once(&mut self, ast: &AST) -> Result<M, ParseError> {
    for value in &mut self.values {
      match value.parse_once(ast) {
        Ok(modifier) => {
          return Ok(modifier);
        }
        Err(err) if err.is_fatal() => {
          return Err(err);
        }
        Err(_) => {
          // Continue with any other possible parses
        }
      }
    }
    Err(ParseError::ExhaustedAlternatives)
  }

  fn name(&self) -> &str {
    &self.name
  }

}

impl<M, N, R, F> ParseRule for Map<R, F>
where F : FnMut(M) -> N,
      R : ParseRule<Modifier=M> {
  type Modifier = N;

  fn parse_once(&mut self, ast: &AST) -> Result<N, ParseError> {
    let result = self.rule.parse_once(ast)?;
    Ok((self.function)(result))
  }

  fn name(&self) -> &str {
    self.rule.name()
  }

}

impl<R> ParseRule for Unique<R>
where R : ParseRule {
  type Modifier = R::Modifier;

  fn parse_once(&mut self, ast: &AST) -> Result<Self::Modifier, ParseError> {
    let result = self.rule.parse_once(ast)?;
    if self.triggered {
      Err(ParseError::UniquenessError(self.name().to_owned()))
    } else {
      self.triggered = true;
      Ok(result)
    }
  }

  fn name(&self) -> &str {
    self.rule.name()
  }

}
