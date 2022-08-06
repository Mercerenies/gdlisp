
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

/// Parse an argument list from an iterator of `AST` values. Returns
/// either the [`GeneralArgList`] or an appropriate error.
pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>)
                 -> Result<GeneralArgList, ArgListParseError> {
  let mut state = ParseState::Required;
  let mut req: Vec<GeneralArg> = Vec::new();
  let mut opt: Vec<GeneralArg> = Vec::new();
  let mut rest: Option<(GeneralArg, VarArg)> = None;
  for arg in args {
    let pos = arg.pos;
    if let ASTF::Symbol(arg) = &arg.value {
      if arg.starts_with('&') {
        match arg.borrow() {
          "&opt" => {
            if state < ParseState::Optional {
              state = ParseState::Optional;
            } else {
              return Err(ArgListParseError::new(ArgListParseErrorF::DirectiveOutOfOrder(arg.to_owned()), pos));
            }
          }
          "&rest" => {
            if state < ParseState::Rest {
              state = ParseState::Rest;
            } else {
              return Err(ArgListParseError::new(ArgListParseErrorF::DirectiveOutOfOrder(arg.to_owned()), pos));
            }
          }
          "&arr" => {
            if state < ParseState::Arr {
              state = ParseState::Arr;
            } else {
              return Err(ArgListParseError::new(ArgListParseErrorF::DirectiveOutOfOrder(arg.to_owned()), pos));
            }
          }
          _ => {
            return Err(ArgListParseError::new(ArgListParseErrorF::UnknownDirective(arg.to_owned()), pos));
          }
        }
        continue;
      }
    }
    match state {
      ParseState::Required => {
        match &arg.value {
          ASTF::Symbol(s) => req.push(GeneralArg::new(s.to_owned())),
          _ => return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos)),
        }
      }
      ParseState::Optional => {
        match &arg.value {
          ASTF::Symbol(s) => opt.push(GeneralArg::new(s.to_owned())),
          _ => return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos)),
        }
      }
      ParseState::Rest => {
        match &arg.value {
          ASTF::Symbol(s) => {
            rest = Some((GeneralArg::new(s.to_owned()), VarArg::RestArg));
            state = ParseState::RestInvalid;
          },
          _ => return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos)),
        }
      }
      ParseState::Arr => {
        match &arg.value {
          ASTF::Symbol(s) => {
            rest = Some((GeneralArg::new(s.to_owned()), VarArg::ArrArg));
            state = ParseState::RestInvalid;
          },
          _ => return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos)),
        }
      }
      ParseState::RestInvalid => {
        return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos));
      }
    }
  }
  Ok(GeneralArgList {
    required_args: req,
    optional_args: opt,
    rest_arg: rest,
  })
}
