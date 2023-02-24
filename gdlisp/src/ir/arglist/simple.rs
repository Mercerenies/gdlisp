// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! Provides [`SimpleArgList`], the type of argument lists which
//! consist of simple names and no other features.

use super::error::{ArgListParseError, ArgListParseErrorF};
use super::general::{GeneralArgList, GeneralArg};
use super::ordinary::ArgList;
use super::parser;
use crate::sxp::ast::AST;
use crate::gdscript::arglist::ArgList as GDArgList;
use crate::compile::names::generator::NameGenerator;
use crate::compile::names::NameTrans;
use crate::pipeline::source::SourceOffset;

use std::convert::TryFrom;
use std::borrow::Borrow;

/// A simple argument list consists only of required arguments and
/// nothing more. This is required in contexts where GDLisp cannot
/// determine the arity of a call, such as when invoking instance
/// methods on an unknown object in GDLisp.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimpleArgList {
  /// The list of required arguments.
  pub args: Vec<String>,
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
    parser::parse(args).and_then(|arglist| {
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

impl From<SimpleArgList> for GeneralArgList {
  fn from(arglist: SimpleArgList) -> GeneralArgList {
    let required_args: Vec<_> = arglist.args.into_iter().map(GeneralArg::simple).collect();
    GeneralArgList {
      required_args: required_args,
      optional_args: vec!(),
      rest_arg: None,
    }
  }
}

impl TryFrom<GeneralArgList> for SimpleArgList {
  type Error = ArgListParseErrorF;

  fn try_from(arglist: GeneralArgList) -> Result<Self, Self::Error> {
    if arglist.optional_args.is_empty() && arglist.rest_arg.is_none() {
      let required_args: Vec<_> = arglist.required_args.into_iter().map(|x| x.into_simple_name()).collect::<Result<_, _>>()?;
      Ok(SimpleArgList { args: required_args })
    } else {
      Err(ArgListParseErrorF::SimpleArgListExpected)
    }
  }

}

impl From<SimpleArgList> for ArgList {
  fn from(arglist: SimpleArgList) -> ArgList {
    let required_args: Vec<_> = arglist.args;
    ArgList {
      required_args: required_args,
      optional_args: vec!(),
      rest_arg: None,
    }
  }
}
