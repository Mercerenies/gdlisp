
use crate::gdscript::arglist::ArgList as GDArgList;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::symbol_table::function_call::FnSpecs;
use crate::sxp::ast::AST;

use std::convert::TryInto;
use std::borrow::Borrow;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgList {
  pub required_args: Vec<String>,
  pub optional_args: Vec<String>,
  pub rest_arg: Option<String>,
}

#[derive(Debug, Clone)]
pub enum ArgListParseError {
  InvalidArgument(AST),
  UnknownDirective(String),
  DirectiveOutOfOrder(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
enum ParseState {
  Required, Optional, Rest, RestInvalid
}

// TODO Support default arguments

impl ArgList {

  pub fn empty() -> ArgList {
    ArgList {
      required_args: vec!(),
      optional_args: vec!(),
      rest_arg: None,
    }
  }

  pub fn required(args: Vec<String>) -> ArgList {
    ArgList {
      required_args: args,
      optional_args: vec!(),
      rest_arg: None,
    }
  }

  pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>)
                   -> Result<ArgList, ArgListParseError> {
    let mut state = ParseState::Required;
    let mut req = Vec::new();
    let mut opt = Vec::new();
    let mut rest = None;
    for arg in args {
      if let AST::Symbol(arg) = arg {
        if arg.starts_with("&") {
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
            _ => {
              return Err(ArgListParseError::UnknownDirective(arg.to_owned()));
            }
          }
          continue;
        }
      }
      match state {
        ParseState::Required => {
          match arg {
            AST::Symbol(s) => req.push(s.to_owned()),
            _ => return Err(ArgListParseError::InvalidArgument(arg.clone())),
          }
        }
        ParseState::Optional => {
          match arg {
            AST::Symbol(s) => opt.push(s.to_owned()),
            _ => return Err(ArgListParseError::InvalidArgument(arg.clone())),
          }
        }
        ParseState::Rest => {
          match arg {
            AST::Symbol(s) => {
              rest = Some(s.to_owned());
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
    if let Some(arg) = self.rest_arg {
      let gd = gen.generate_with(&arg);
      name_translations.push((arg, gd.clone()));
      args.push(gd);
    }
    (GDArgList::required(args), name_translations)
  }

}

impl From<ArgList> for FnSpecs {

  fn from(arglist: ArgList) -> FnSpecs {
    // TODO We need to define an upper limit on argument list length
    // (and check if Godot already has one we need to respect)
    FnSpecs::new(
      arglist.required_args.len().try_into().unwrap(),
      arglist.optional_args.len().try_into().unwrap(),
      arglist.rest_arg.is_some(),
    )
  }

}
