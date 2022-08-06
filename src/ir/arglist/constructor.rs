
//! Provides the [`ConstructorArgList`] type, the type of argument
//! lists which allow for instance variables to be initialized
//! directly from them.

use super::error::ArgListParseError;
use super::simple::SimpleArgList;
use crate::sxp::ast::AST;
use crate::compile::names::NameTrans;
use crate::compile::names::generator::NameGenerator;
use crate::pipeline::source::SourceOffset;
use crate::gdscript::arglist::ArgList as GDArgList;

use std::borrow::Borrow;

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

  /// An iterator over all variable names, together with whether or
  /// not they are instance variables, mentioned in the argument list,
  /// in order.
  pub fn iter_vars(&self) -> impl Iterator<Item = (&str, bool)> {
    self.args.iter().map(|(name, field)| (name.borrow(), *field))
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
  pub fn parse<'a>(_args: impl IntoIterator<Item = &'a AST>, _pos: SourceOffset)
                   -> Result<ConstructorArgList, ArgListParseError> {
    todo!() /////
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
