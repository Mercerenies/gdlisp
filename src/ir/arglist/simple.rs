
//! Provides [`SimpleArgList`], the type of argument lists which
//! consist of simple names and no other features.

use super::error::{ArgListParseError, ArgListParseErrorF};
use super::ordinary::ArgList;
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
    // TODO Change this to go through GeneralArgList instead of ArgList
    ArgList::parse(args, pos).and_then(|arglist| {
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
