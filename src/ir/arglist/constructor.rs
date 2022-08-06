
//! Provides the [`ConstructorArgList`] type, the type of argument
//! lists which allow for instance variables to be initialized
//! directly from them.

use super::error::{ArgListParseError, ArgListParseErrorF};
use super::simple::SimpleArgList;
use super::general::{GeneralArgList, GeneralArg};
use super::parser;
use crate::sxp::ast::AST;
use crate::compile::names::NameTrans;
use crate::compile::names::generator::NameGenerator;
use crate::pipeline::source::SourceOffset;
use crate::gdscript::arglist::ArgList as GDArgList;

use std::borrow::Borrow;
use std::convert::TryFrom;

/// A constructor argument list consists of only required arguments,
/// similar to [`SimpleArgList`]. However, some of the arguments can
/// be marked as instance fields. These will be assigned to the
/// corresponding instance variable on the class at runtime.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstructorArgList {
  /// The list of required arguments, together with whether or not
  /// they've been marked as instance variables.
  pub args: Vec<(String, bool)>,
}

impl ConstructorArgList {

  /// Converts the argument list into a GDScript argument list, using
  /// the given name generator to produce unique names, similar to
  /// [`ArgList::into_gd_arglist`](super::ordinary::ArgList::into_gd_arglist).
  ///
  /// The second return value of this function includes, in addition
  /// to the name translations, whether or not each argument is marked
  /// as a constructor instance field.
  pub fn into_gd_arglist(self, gen: &mut impl NameGenerator) -> (GDArgList, Vec<(NameTrans, bool)>) {
    let (args, instance_fields): (Vec<_>, Vec<_>) = self.args.into_iter().unzip();
    let arglist = SimpleArgList { args };
    let (gd_arglist, trans) = arglist.into_gd_arglist(gen);
    let names: Vec<(NameTrans, bool)> = trans.into_iter().zip(instance_fields.into_iter()).collect();
    (gd_arglist, names)
  }

  /// Returns true iff any arguments are marked as instance fields.
  pub fn has_any_instance_fields(&self) -> bool {
    self.args.iter().any(|(_, b)| *b)
  }

  /// An iterator over all variable names, together with whether or
  /// not they are instance variables, mentioned in the argument list,
  /// in order.
  pub fn iter_vars(&self) -> impl Iterator<Item = (&str, bool)> {
    self.args.iter().map(|(name, field)| (name.borrow(), *field))
  }

  /// An iterator over all variable names, together with whether or
  /// not they are instance variables, as a mutable iterator.
  pub fn iter_vars_mut(&mut self) -> impl Iterator<Item = &mut (String, bool)> {
    self.args.iter_mut()
  }

  /// Attempts to parse a constructor argument list from the iterator.
  /// A constructor argument list can contain ordinary names, just
  /// like a [`SimpleArgList`], but those names can also be written in
  /// one of the following three equivalent forms.
  ///
  /// ```text
  /// @foobar
  /// self:foobar
  /// (access-slot self foobar)
  /// ```
  ///
  /// In that case, the argument will be marked as a constructor
  /// instance field argument.
  pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>, pos: SourceOffset)
                   -> Result<ConstructorArgList, ArgListParseError> {
    parser::parse(args).and_then(|arglist| {
      ConstructorArgList::try_from(arglist).map_err(|err| ArgListParseError::new(err, pos))
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

impl From<ConstructorArgList> for GeneralArgList {
  fn from(arglist: ConstructorArgList) -> GeneralArgList {
    let required_args: Vec<_> = arglist.args.into_iter().map(|(name, is_instance_field)| {
      GeneralArg { name, is_instance_field }
    }).collect();
    GeneralArgList {
      required_args: required_args,
      optional_args: vec!(),
      rest_arg: None,
    }
  }
}

impl TryFrom<GeneralArgList> for ConstructorArgList {
  type Error = ArgListParseErrorF;

  fn try_from(arglist: GeneralArgList) -> Result<Self, Self::Error> {
    if arglist.optional_args.is_empty() && arglist.rest_arg.is_none() {
      let required_args: Vec<_> = arglist.required_args.into_iter().map(|x| {
        (x.name, x.is_instance_field)
      }).collect();
      Ok(ConstructorArgList { args: required_args })
    } else {
      Err(ArgListParseErrorF::ConstructorArgListExpected)
    }
  }

}
