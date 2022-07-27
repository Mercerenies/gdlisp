
//! Argument list types as supported by the IR.
//!
//! See [`crate::gdscript::arglist`] for the companion module on the
//! GDScript side.

use crate::gdscript::arglist::ArgList as GDArgList;
use crate::compile::names::{self, NameTrans};
use crate::compile::names::generator::NameGenerator;
use crate::compile::symbol_table::function_call::FnSpecs;
use crate::sxp::ast::{AST, ASTF};
use crate::pipeline::source::{SourceOffset, Sourced};

use serde::{Serialize, Deserialize};

use std::convert::TryFrom;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;

/// An argument list in GDLisp consists of a sequence of zero or more
/// required arguments, followed by zero or more optional arguments,
/// followed by (optionally) a "rest" argument.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgList {
  /// The list of required argument names.
  pub required_args: Vec<String>,
  /// The list of optional argument names. Note that optional
  /// arguments in GDLisp always default to `nil`, so no default value
  /// is explicitly mentioned here.
  pub optional_args: Vec<String>,
  /// The "rest" argument. If present, this indicates the name of the
  /// argument and the type of "rest" argument.
  pub rest_arg: Option<(String, VarArg)>,
}

/// A simple argument list consists only of required arguments and
/// nothing more. This is required in contexts where GDLisp cannot
/// determine the arity of a call, such as when invoking instance
/// methods on an unknown object in GDLisp.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimpleArgList {
  /// The list of required arguments.
  pub args: Vec<String>,
}

/// The type of "rest" argument which accumulates any extra arguments
/// to a function call.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum VarArg {
  /// A `&rest` argument accumulates the arguments into a GDLisp list.
  RestArg,
  /// An `&arr` argument accumulates the arguments into a Godot array.
  ArrArg,
}

/// `ArgListParseErrorF` describes the types of errors that can occur
/// when parsing an [`AST`] argument list.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ArgListParseErrorF {
  /// An argument of some specific type was expected but something
  /// else was provided. (TODO Remove this in favor of more specific
  /// errors)
  InvalidArgument(AST),
  /// An `&` directive was provided but the name was unknown.
  UnknownDirective(String),
  /// An `&` directive appeared in the wrong place in an argument
  /// list, such as attempting to specify `&opt` arguments after
  /// `&rest`.
  DirectiveOutOfOrder(String),
  /// A simple argument list with no directives was expected, but
  /// directives were used.
  SimpleArgListExpected,
}

/// An [`ArgListParseErrorF`] together with [`SourceOffset`] data.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArgListParseError {
  pub value: ArgListParseErrorF,
  pub pos: SourceOffset,
}

/// The current type of argument we're looking for when parsing an
/// argument list.
///
/// This is an internal type to this module and, generally, callers
/// from outside the module should not need to interface with it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseState {
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

impl ArgList {

  /// An empty argument list, taking no required or optional arguments
  /// and having no "rest" argument.
  pub fn empty() -> ArgList {
    ArgList {
      required_args: vec!(),
      optional_args: vec!(),
      rest_arg: None,
    }
  }

  /// An argument list consisting only of a single [`VarArg::RestArg`]
  /// argument with a default name.
  #[deprecated(note="Directly construct the arglist with an explicit rest name instead")]
  pub fn rest() -> ArgList {
    ArgList {
      required_args: vec!(),
      optional_args: vec!(),
      rest_arg: Some((String::from("rest-arg"), VarArg::RestArg)),
    }
  }

  /// An argument list consisting only of required arguments with the
  /// given names.
  pub fn required(args: Vec<String>) -> ArgList {
    ArgList {
      required_args: args,
      optional_args: vec!(),
      rest_arg: None,
    }
  }

  /// Converts an [`FnSpecs`] to an [`ArgList`] with dummy names for
  /// the variables.
  ///
  /// The names of the generated arguments may change and should not
  /// be relied upon; only the shape and lengths should be considered
  /// stable.
  pub fn from_specs(specs: FnSpecs) -> ArgList {
    // Uses dummy names for variables.
    let required_args = (0..specs.required).into_iter().map(|i| format!("required_arg{}", i)).collect();
    let optional_args = (0..specs.optional).into_iter().map(|i| format!("optional_arg{}", i)).collect();
    let rest_arg = specs.rest.map(|arg| (String::from("rest_arg"), arg));
    ArgList { required_args, optional_args, rest_arg }
  }

  /// Parse an argument list from an iterator of `AST` values. Returns
  /// either the [`ArgList`] or an appropriate error.
  pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>)
                   -> Result<ArgList, ArgListParseError> {
    let mut state = ParseState::Required;
    let mut req = Vec::new();
    let mut opt = Vec::new();
    let mut rest = None;
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
            ASTF::Symbol(s) => req.push(s.to_owned()),
            _ => return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos)),
          }
        }
        ParseState::Optional => {
          match &arg.value {
            ASTF::Symbol(s) => opt.push(s.to_owned()),
            _ => return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos)),
          }
        }
        ParseState::Rest => {
          match &arg.value {
            ASTF::Symbol(s) => {
              rest = Some((s.to_owned(), VarArg::RestArg));
              state = ParseState::RestInvalid;
            },
            _ => return Err(ArgListParseError::new(ArgListParseErrorF::InvalidArgument(arg.clone()), pos)),
          }
        }
        ParseState::Arr => {
          match &arg.value {
            ASTF::Symbol(s) => {
              rest = Some((s.to_owned(), VarArg::ArrArg));
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
    Ok(ArgList {
      required_args: req,
      optional_args: opt,
      rest_arg: rest,
    })
  }

  /// Converts the argument list into a GDScript argument list, using
  /// the given name generator to produce unique GDScript names.
  ///
  /// * Each required argument will be translated into a GDScript
  /// argument.
  ///
  /// * Each optional argument, likewise, will be translated into a
  /// GDScript argument. On the GDScript side, required and optional
  /// arguments are indistinguishable.
  ///
  /// * If there is a "rest" argument of any kind, it is translated to
  /// a single GDScript argument as well.
  pub fn into_gd_arglist(self, gen: &mut impl NameGenerator) -> (GDArgList, Vec<NameTrans>) {
    let cap = 1 + self.required_args.len() + self.optional_args.len();
    let mut name_translations = Vec::with_capacity(cap);
    let mut args = Vec::with_capacity(cap);
    for arg in self.required_args {
      let gd = gen.generate_with(&names::lisp_to_gd(&arg));
      name_translations.push(NameTrans { lisp_name: arg, gd_name: gd.clone() });
      args.push(gd);
    }
    for arg in self.optional_args {
      let gd = gen.generate_with(&names::lisp_to_gd(&arg));
      name_translations.push(NameTrans { lisp_name: arg, gd_name: gd.clone() });
      args.push(gd);
    }
    if let Some((arg, _)) = self.rest_arg {
      let gd = gen.generate_with(&names::lisp_to_gd(&arg));
      name_translations.push(NameTrans { lisp_name: arg, gd_name: gd.clone() });
      args.push(gd);
    }
    (GDArgList::required(args), name_translations)
  }

  /// An iterator over all variable names mentioned in the argument
  /// list, in order.
  pub fn iter_vars(&self) -> impl Iterator<Item = &str> {
    self.required_args.iter()
      .chain(self.optional_args.iter())
      .chain(self.rest_arg.iter().map(|x| &x.0))
      .map(|x| x.borrow())
  }

}

impl SimpleArgList {

  /// Converts the argument list into a GDScript argument list, using
  /// the given name generator to produce unique names, similar to
  /// [`ArgList::into_gd_arglist`].
  pub fn into_gd_arglist(self, gen: &mut impl NameGenerator) -> (GDArgList, Vec<NameTrans>) {
    ArgList::from(self).into_gd_arglist(gen)
  }

  /// An iterator over all variable names mentioned in the argument
  /// list, in order.
  pub fn iter_vars(&self) -> impl Iterator<Item = &str> {
    self.args.iter().map(|x| x.borrow())
  }

  /// Attempts to parse a simple argument list from the iterator, as
  /// though by [`ArgList::parse`], and then attempts to convert that
  /// argument list into a [`SimpleArgList`]. If either step fails, an
  /// error is reported.
  pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>, pos: SourceOffset)
                   -> Result<SimpleArgList, ArgListParseError> {
    ArgList::parse(args).and_then(|arglist| {
      SimpleArgList::try_from(arglist).map_err(|err| ArgListParseError::new(err, pos))
    })
  }

  /// The length of the argument list.
  pub fn len(&self) -> usize {
    self.args.len()
  }

  /// Whether the argument list consists of zero arguments.
  pub fn is_empty(&self) -> bool {
    self.args.is_empty()
  }

}

impl VarArg {

  /// An [`i32`] constant representing no variable argument at all.
  pub const NONE: i32 = 0;

  /// Converts `self` into a numerical value, suitable for
  /// communication with GDScript. This method is guaranteed to never
  /// return [`VarArg::NONE`].
  pub fn into_constant(self) -> i32 {
    match self {
      VarArg::RestArg => 1,
      VarArg::ArrArg => 2,
    }
  }

  /// Returns [`VarArg::NONE`] if `opt` is `None`, or calls
  /// [`VarArg::into_constant`] otherwise.
  pub fn arg_to_const(opt: Option<VarArg>) -> i32 {
    opt.map_or(VarArg::NONE, VarArg::into_constant)
  }

}

impl From<ArgList> for FnSpecs {

  /// [`FnSpecs`] is simply an [`ArgList`] without the argument names;
  /// it merely preserves the shape. From an `ArgList` we can always
  /// construct an `FnSpecs` in a canonical way.
  fn from(arglist: ArgList) -> FnSpecs {
    // TODO We need to define an upper limit on argument list length
    // (and check if Godot already has one we need to respect)
    FnSpecs::new(
      arglist.required_args.len(),
      arglist.optional_args.len(),
      arglist.rest_arg.map(|x| x.1),
    )
  }

}

impl From<SimpleArgList> for ArgList {
  fn from(arglist: SimpleArgList) -> ArgList {
    ArgList::required(arglist.args)
  }
}

impl TryFrom<ArgList> for SimpleArgList {
  type Error = ArgListParseErrorF;

  fn try_from(arglist: ArgList) -> Result<Self, Self::Error> {
    if arglist.optional_args.is_empty() && arglist.rest_arg.is_none() {
      Ok(SimpleArgList { args: arglist.required_args })
    } else {
      Err(ArgListParseErrorF::SimpleArgListExpected)
    }
  }

}

impl ArgListParseError {
  pub fn new(value: ArgListParseErrorF, pos: SourceOffset) -> ArgListParseError {
    ArgListParseError { value, pos }
  }
}

impl fmt::Display for ArgListParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      ArgListParseErrorF::InvalidArgument(ast) => {
        write!(f, "Invalid arglist argument {}", ast)
      }
      ArgListParseErrorF::UnknownDirective(s) => {
        write!(f, "Unknown arglist directive {}", s)
      }
      ArgListParseErrorF::DirectiveOutOfOrder(s) => {
        write!(f, "Arglist directive appeared out of order {}", s)
      }
      ArgListParseErrorF::SimpleArgListExpected => {
        write!(f, "Only simple arglists are allowed in this context")
      }
    }
  }
}

impl Sourced for ArgListParseError {
  type Item = ArgListParseErrorF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &ArgListParseErrorF {
    &self.value
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::parser;
  use crate::sxp::dotted::DottedExpr;
  use crate::compile::names::fresh::FreshNameGenerator;
  use std::convert::TryInto;

  fn parse_ast(input: &str) -> AST {
    let parser = parser::ASTParser::new();
    parser.parse(input).unwrap()
  }

  fn parse_arglist(input: &str) -> Result<ArgList, ArgListParseError> {
    let ast = parse_ast(input);
    let dotted: Vec<_> = DottedExpr::new(&ast).try_into().unwrap();
    ArgList::parse(dotted)
  }

  fn arglist(req: Vec<&str>, opt: Vec<&str>, rest: Option<(&str, VarArg)>) -> ArgList {
    ArgList {
      required_args: req.into_iter().map(|x| x.to_owned()).collect(),
      optional_args: opt.into_iter().map(|x| x.to_owned()).collect(),
      rest_arg: rest.map(|(x, y)| (x.to_owned(), y)),
    }
  }

  fn gdarglist(req: Vec<&str>) -> GDArgList {
    GDArgList::required(req.into_iter().map(|x| x.to_owned()).collect())
  }

  fn into_gd(args: ArgList) -> (GDArgList, Vec<NameTrans>) {
    let mut tmp = FreshNameGenerator::new(vec!());
    args.into_gd_arglist(&mut tmp)
  }

  #[test]
  fn test_parsing() {
    assert_eq!(parse_arglist("()").unwrap(), arglist(vec!(), vec!(), None));
    assert_eq!(parse_arglist("(a)").unwrap(), arglist(vec!("a"), vec!(), None));
    assert_eq!(parse_arglist("(a b)").unwrap(), arglist(vec!("a", "b"), vec!(), None));
    assert_eq!(parse_arglist("(a b &opt c)").unwrap(), arglist(vec!("a", "b"), vec!("c"), None));
    assert_eq!(parse_arglist("(a &rest rest)").unwrap(), arglist(vec!("a"), vec!(), Some(("rest", VarArg::RestArg))));
    assert_eq!(parse_arglist("(a b c &opt d &rest e)").unwrap(), arglist(vec!("a", "b", "c"), vec!("d"), Some(("e", VarArg::RestArg))));
    assert_eq!(parse_arglist("(a b c &opt d e &rest f)").unwrap(), arglist(vec!("a", "b", "c"), vec!("d", "e"), Some(("f", VarArg::RestArg))));
    assert_eq!(parse_arglist("(a b c &opt d e &arr f)").unwrap(), arglist(vec!("a", "b", "c"), vec!("d", "e"), Some(("f", VarArg::ArrArg))));
    assert_eq!(parse_arglist("(a b c &opt d e)").unwrap(), arglist(vec!("a", "b", "c"), vec!("d", "e"), None));
  }

  #[test]
  fn test_invalid_parse() {
    assert!(parse_arglist("(&silly-name)").is_err());
    assert!(parse_arglist("(&opt a &opt b)").is_err());
    assert!(parse_arglist("(&rest a &opt b)").is_err());
    assert!(parse_arglist("(&arr a &opt b)").is_err());
    assert!(parse_arglist("(&rest a &rest b)").is_err());
    assert!(parse_arglist("(&rest a b)").is_err());
    assert!(parse_arglist("(&rest a &arr b)").is_err());
    assert!(parse_arglist("(&arr a &rest b)").is_err());
  }

  #[test]
  fn test_arglist_gen() {
    assert_eq!(into_gd(arglist(vec!(), vec!(), None)).0, gdarglist(vec!()));
    assert_eq!(into_gd(arglist(vec!("a"), vec!(), None)).0, gdarglist(vec!("a_0")));
    assert_eq!(into_gd(arglist(vec!("a", "b"), vec!(), None)).0, gdarglist(vec!("a_0", "b_1")));
    assert_eq!(into_gd(arglist(vec!("a"), vec!("b"), None)).0, gdarglist(vec!("a_0", "b_1")));
    assert_eq!(into_gd(arglist(vec!("a"), vec!("b"), Some(("r", VarArg::RestArg)))).0, gdarglist(vec!("a_0", "b_1", "r_2")));
    assert_eq!(into_gd(arglist(vec!("a"), vec!("b"), Some(("r", VarArg::ArrArg)))).0, gdarglist(vec!("a_0", "b_1", "r_2")));
  }

}
