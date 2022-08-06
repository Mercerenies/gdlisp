
//! The general-purpose parsing module of [`GeneralArgList`].

use crate::sxp::ast::{AST, ASTF};
use super::general::{GeneralArgList, GeneralArg};
use super::error::{ArgListParseError, ArgListParseErrorF};
use super::vararg::VarArg;

use std::cmp::Ordering;
use std::borrow::Borrow;

/// The current type of argument we're looking for when parsing an
/// argument list.
///
/// This is an internal type to this module and, generally, callers
/// from outside the module should not need to interface with it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseState {
  /// We're expecting required arguments. This is the state we begin
  /// in.
  Required,
  /// We're expecting optional arguments. This is the state following
  /// `&opt`.
  Optional,
  /// We're expecting the single `&rest` argument.
  Rest,
  /// We're expecting the single `&arr` argument.
  Arr,
  /// We have passed the "rest" argument and are expecting the end of
  /// an argument list. If *any* arguments occur in this state, an
  /// error will be issued.
  RestInvalid,
}

/// `ParseState` implements `PartialOrd` to indicate valid orderings
/// in which the argument type directives can occur. Specifically, we
/// can transition from a state `u` to a state `v` if and only if `u
/// <= v` is true. If we attempt a state transition where that is not
/// true, then an [`ArgListParseErrorF::DirectiveOutOfOrder`] error
/// will be issued.
///
/// There are two chains in this ordering:
///
/// * `Required < Optional < Rest < RestInvalid`
///
/// * `Required < Optional < Arr < RestInvalid`
///
/// The two states `Rest` and `Arr` are incomparable.
impl PartialOrd for ParseState {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    if *self == *other {
      return Some(Ordering::Equal);
    }
    if *self == ParseState::Required {
      return Some(Ordering::Less);
    }
    if *other == ParseState::Required {
      return Some(Ordering::Greater);
    }
    if *self == ParseState::Optional {
      return Some(Ordering::Less);
    }
    if *other == ParseState::Optional {
      return Some(Ordering::Greater);
    }
    if *self == ParseState::RestInvalid {
      return Some(Ordering::Greater);
    }
    if *other == ParseState::RestInvalid {
      return Some(Ordering::Less);
    }
    None
  }
}

impl ParseState {

  /// The state to begin parsing an argument list in.
  pub const START_STATE: ParseState =
    ParseState::Required;

  /// Given an argument directive, returns the state represented by
  /// that directive, or `None` if the string is not a valid argument
  /// directive.
  ///
  /// Note that [`ParseState::Required`] is not represented by an
  /// argument directive, as it is the start state and never requires
  /// (or admits) an `&` directive to transition *to* that state.
  pub fn state_transition(self, arg: &str) -> Option<ParseState> {
    match arg.borrow() {
      "&opt" => Some(ParseState::Optional),
      "&rest" => Some(ParseState::Rest),
      "&arr" => Some(ParseState::Arr),
      _ => None
    }
  }

  /// Parse the given [`AST`] value, modifying the current parse
  /// state, and the [`GeneralArgList`] being built, as necessary.
  pub fn parse_once(&mut self, arglist: &mut GeneralArgList, arg: &AST) -> Result<(), ArgListParseError> {
    if self.parse_state_transition(arg)? {
      Ok(())
    } else {
      let pos = arg.pos;
      match self {
        ParseState::Required => {
          let general_arg = GeneralArg::parse(arg)?;
          arglist.required_args.push(general_arg);
        }
        ParseState::Optional => {
          let general_arg = GeneralArg::parse(arg)?;
          arglist.optional_args.push(general_arg);
        }
        ParseState::Rest => {
          let general_arg = GeneralArg::parse(arg)?;
          arglist.rest_arg = Some((general_arg, VarArg::RestArg));
          *self = ParseState::RestInvalid;
        }
        ParseState::Arr => {
          let general_arg = GeneralArg::parse(arg)?;
          arglist.rest_arg = Some((general_arg, VarArg::ArrArg));
          *self = ParseState::RestInvalid;
        }
        ParseState::RestInvalid => {
          return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos));
        }
      }
      Ok(())
    }
  }

  fn parse_state_transition(&mut self, arg: &AST) -> Result<bool, ArgListParseError> {
    // Returns whether or not a transition was parsed.
    let pos = arg.pos;
    match &arg.value {
      ASTF::Symbol(arg) if arg.starts_with('&') => {
        let new_state = self.state_transition(arg.borrow());
        match new_state {
          None => {
            Err(ArgListParseError::new(ArgListParseErrorF::UnknownDirective(arg.to_owned()), pos))
          }
          Some(new_state) => {
            if *self < new_state {
              *self = new_state;
              Ok(true)
            } else {
              Err(ArgListParseError::new(ArgListParseErrorF::DirectiveOutOfOrder(arg.to_owned()), pos))
            }
          }
        }
      }
      _ => {
        Ok(false)
      }
    }
  }

}

/// Parse an argument list from an iterator of `AST` values. Returns
/// either the [`GeneralArgList`] or an appropriate error.
pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>)
                 -> Result<GeneralArgList, ArgListParseError> {
  let mut state = ParseState::START_STATE;
  let mut result = GeneralArgList::empty();
  for arg in args {
    state.parse_once(&mut result, arg)?;
  }
  Ok(result)
}
