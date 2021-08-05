
use crate::gdscript::arglist::ArgList as GDArgList;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::symbol_table::function_call::FnSpecs;
use crate::sxp::ast::{AST, ASTF};

use std::convert::{TryFrom, TryInto};
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgList {
  pub required_args: Vec<String>,
  pub optional_args: Vec<String>,
  pub rest_arg: Option<(String, VarArg)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimpleArgList {
  pub args: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VarArg { RestArg, ArrArg }

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ArgListParseError {
  InvalidArgument(AST),
  UnknownDirective(String),
  DirectiveOutOfOrder(String),
  ModifiersNotAllowed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseState {
  Required, Optional, Rest, Arr, RestInvalid
}

impl PartialOrd for ParseState {
  // There are two chains in this ordering.
  // Required < Optional < Rest < RestInvalid
  // Required < Optional < Arr  < RestInvalid
  // Rest and Arr are incomparable
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

  pub fn empty() -> ArgList {
    ArgList {
      required_args: vec!(),
      optional_args: vec!(),
      rest_arg: None,
    }
  }

  pub fn rest() -> ArgList {
    ArgList {
      required_args: vec!(),
      optional_args: vec!(),
      rest_arg: Some((String::from("rest-arg"), VarArg::RestArg)),
    }
  }

  pub fn required(args: Vec<String>) -> ArgList {
    ArgList {
      required_args: args,
      optional_args: vec!(),
      rest_arg: None,
    }
  }

  pub fn from_specs(specs: FnSpecs) -> ArgList {
    // Uses dummy names for variables.
    let required_args = (0..specs.required).into_iter().map(|i| format!("required_arg{}", i)).collect();
    let optional_args = (0..specs.optional).into_iter().map(|i| format!("optional_arg{}", i)).collect();
    let rest_arg = specs.rest.map(|arg| (String::from("rest_arg"), arg));
    ArgList { required_args, optional_args, rest_arg }
  }

  pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>)
                   -> Result<ArgList, ArgListParseError> {
    let mut state = ParseState::Required;
    let mut req = Vec::new();
    let mut opt = Vec::new();
    let mut rest = None;
    for arg in args {
      if let ASTF::Symbol(arg) = &arg.value {
        if arg.starts_with('&') {
          match arg.borrow() {
            "&opt" => {
              if state < ParseState::Optional {
                state = ParseState::Optional;
              } else {
                return Err(ArgListParseError::DirectiveOutOfOrder(arg.to_owned()));
              }
            }
            "&rest" => {
              if state < ParseState::Rest {
                state = ParseState::Rest;
              } else {
                return Err(ArgListParseError::DirectiveOutOfOrder(arg.to_owned()));
              }
            }
            "&arr" => {
              if state < ParseState::Arr {
                state = ParseState::Arr;
              } else {
                return Err(ArgListParseError::DirectiveOutOfOrder(arg.to_owned()));
              }
            }
            _ => {
              return Err(ArgListParseError::UnknownDirective(arg.to_owned()));
            }
          }
          continue;
        }
      }
      match state {
        ParseState::Required => {
          match &arg.value {
            ASTF::Symbol(s) => req.push(s.to_owned()),
            _ => return Err(ArgListParseError::InvalidArgument(arg.clone())),
          }
        }
        ParseState::Optional => {
          match &arg.value {
            ASTF::Symbol(s) => opt.push(s.to_owned()),
            _ => return Err(ArgListParseError::InvalidArgument(arg.clone())),
          }
        }
        ParseState::Rest => {
          match &arg.value {
            ASTF::Symbol(s) => {
              rest = Some((s.to_owned(), VarArg::RestArg));
              state = ParseState::RestInvalid;
            },
            _ => return Err(ArgListParseError::InvalidArgument(arg.clone())),
          }
        }
        ParseState::Arr => {
          match &arg.value {
            ASTF::Symbol(s) => {
              rest = Some((s.to_owned(), VarArg::ArrArg));
              state = ParseState::RestInvalid;
            },
            _ => return Err(ArgListParseError::InvalidArgument(arg.clone())),
          }
        }
        ParseState::RestInvalid => {
          return Err(ArgListParseError::InvalidArgument(arg.clone()));
        }
      }
    }
    Ok(ArgList {
      required_args: req,
      optional_args: opt,
      rest_arg: rest,
    })
  }

  pub fn into_gd_arglist(self, gen: &mut FreshNameGenerator) -> (GDArgList, Vec<(String, String)>) {
    let cap = 1 + self.required_args.len() + self.optional_args.len();
    let mut name_translations = Vec::with_capacity(cap);
    let mut args = Vec::with_capacity(cap);
    for arg in self.required_args {
      let gd = gen.generate_with(&arg);
      name_translations.push((arg, gd.clone()));
      args.push(gd);
    }
    for arg in self.optional_args {
      let gd = gen.generate_with(&arg);
      name_translations.push((arg, gd.clone()));
      args.push(gd);
    }
    if let Some((arg, _)) = self.rest_arg {
      let gd = gen.generate_with(&arg);
      name_translations.push((arg, gd.clone()));
      args.push(gd);
    }
    (GDArgList::required(args), name_translations)
  }

  pub fn iter_vars(&self) -> impl Iterator<Item = &str> {
    self.required_args.iter()
      .chain(self.optional_args.iter())
      .chain(self.rest_arg.iter().map(|x| &x.0))
      .map(|x| x.borrow())
  }

}

impl SimpleArgList {

  pub fn into_gd_arglist(self, gen: &mut FreshNameGenerator) -> (GDArgList, Vec<(String, String)>) {
    ArgList::from(self).into_gd_arglist(gen)
  }

  pub fn iter_vars(&self) -> impl Iterator<Item = &str> {
    self.args.iter().map(|x| x.borrow())
  }

  pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>)
                   -> Result<SimpleArgList, ArgListParseError> {
    ArgList::parse(args).and_then(
      SimpleArgList::try_from
    )
  }

}

impl From<ArgList> for FnSpecs {

  fn from(arglist: ArgList) -> FnSpecs {
    // TODO We need to define an upper limit on argument list length
    // (and check if Godot already has one we need to respect)
    FnSpecs::new(
      arglist.required_args.len().try_into().unwrap(),
      arglist.optional_args.len().try_into().unwrap(),
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
  type Error = ArgListParseError;

  fn try_from(arglist: ArgList) -> Result<Self, Self::Error> {
    if arglist.optional_args.is_empty() && arglist.rest_arg.is_none() {
      Ok(SimpleArgList { args: arglist.required_args })
    } else {
      Err(ArgListParseError::ModifiersNotAllowed)
    }
  }

}

impl fmt::Display for ArgListParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ArgListParseError::InvalidArgument(ast) => {
        write!(f, "Invalid arglist argument {}", ast)
      }
      ArgListParseError::UnknownDirective(s) => {
        write!(f, "Unknown arglist directive {}", s)
      }
      ArgListParseError::DirectiveOutOfOrder(s) => {
        write!(f, "Arglist directive appeared out of order {}", s)
      }
      ArgListParseError::ModifiersNotAllowed => {
        write!(f, "Arglist modifiers not allowed in this context")
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::parser;
  use crate::sxp::dotted::DottedExpr;

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

  fn into_gd(args: ArgList) -> (GDArgList, Vec<(String, String)>) {
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
