
//! GDScript argument lists.
//!
//! The type [`ArgList`] defines a list of arguments as GDScript sees
//! them.
//!
//! See [`crate::ir::arglist`] for the companion module used during IR
//! analysis.

use super::expr::Expr;

use std::convert::AsRef;
use std::mem::swap;

/// A list of arguments in a function declaration.
///
/// Currently, GDLisp only compiles to required GDScript arguments and
/// does not provide support for optional arguments on the GDScript
/// side.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgList {
  /// The required arguments.
  required_args: Vec<String>,
  /// The optional arguments.
  optional_args: Vec<(String, Expr)>,
}

impl ArgList {

  /// An empty argument list.
  pub fn empty() -> ArgList {
    ArgList { required_args: Vec::new(), optional_args: Vec::new() }
  }

  /// A list of required arguments.
  pub fn required(args: Vec<String>) -> ArgList {
    ArgList { required_args: args, optional_args: Vec::new() }
  }

  /// Appends the given values to the current argument list as
  /// optional arguments. Takes ownership but returns `self`.
  ///
  /// This method is intended to be used in a builder style, like so.
  ///
  /// ```
  /// # use gdlisp::gdscript::arglist::ArgList;
  /// # use gdlisp::gdscript::expr::Expr;
  /// # use gdlisp::pipeline::source::SourceOffset;
  /// # let required_vec = vec!(String::from("a"), String::from("b"));
  /// # let optional_vec = vec!((String::from("c"), Expr::null(SourceOffset(0))));
  /// let args = ArgList::required(required_vec).optional(optional_vec);
  /// # let all_arg_names: Vec<&str> = args.all_args_iter().collect();
  /// # assert_eq!(all_arg_names, vec!("a", "b", "c"));
  /// ```
  pub fn optional(mut self, args: impl IntoIterator<Item=(String, Expr)>) -> Self {
    self.optional_args.extend(args);
    self
  }

  /// Whether the argument list is empty.
  pub fn is_empty(&self) -> bool {
    self.required_args.is_empty() && self.optional_args.is_empty()
  }

  /// Insert required arguments at the beginning of this argument
  /// list.
  pub fn prepend_required(&mut self, required: impl Iterator<Item=String>) {
    let mut new_vec: Vec<_> = required.collect();
    swap(&mut new_vec, &mut self.required_args);
    self.required_args.extend(new_vec);
  }

  /// Converts all required arguments into optional arguments, where
  /// the default value is the given value.
  ///
  /// The default value will be cloned once for each argument.
  pub fn optionalize_all(&mut self, default_value: &Expr) {
    let required_args = self.required_args.drain(..).map(|arg| (arg, default_value.to_owned()));
    self.optional_args.splice(0..0, required_args);
  }

  /// All of the arguments, required and optional alike, as an
  /// iterator.
  pub fn all_args_iter(&self) -> impl Iterator<Item=&str> {
    self.required_args.iter().map(String::as_ref)
      .chain(self.optional_args.iter().map(|x| x.0.as_ref()))
  }

  /// Returns the list of arguments in GDScript form, i.e. as a
  /// comma-separated list of arguments.
  pub fn to_gd(&self) -> String {
    let required = self.required_args.iter().map(String::from);
    let optional = self.optional_args.iter().map(|(name, default)| {
      format!("{} = {}", name, default.to_gd())
    });

    let mut all_args = Vec::new();
    all_args.extend(required);
    all_args.extend(optional);
    all_args.join(", ")

  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::pipeline::source::SourceOffset;

  fn null() -> Expr {
    Expr::null(SourceOffset(0))
  }

  #[test]
  fn empty_arglist_to_gd_test() {
    assert_eq!(ArgList::empty().to_gd(), "");
  }

  #[test]
  fn required_arglist_to_gd_test() {
    assert_eq!(ArgList::required(vec!()).to_gd(), "");
    assert_eq!(ArgList::required(vec!(String::from("foo"))).to_gd(), "foo");
    assert_eq!(ArgList::required(vec!(String::from("foo"), String::from("bar"))).to_gd(), "foo, bar");
  }

  #[test]
  fn optional_arglist_to_gd_test() {
    assert_eq!(ArgList::empty().optional(vec!((String::from("foo"), null()))).to_gd(), "foo = null");
    assert_eq!(ArgList::empty().optional(vec!((String::from("foo"), null()), (String::from("bar"), null()))).to_gd(), "foo = null, bar = null");
  }

  #[test]
  fn full_arglist_to_gd_test() {
    assert_eq!(ArgList::required(vec!(String::from("foo"), String::from("bar")))
               .optional(vec!((String::from("baz"), null())))
               .to_gd(),
               "foo, bar, baz = null");
  }

  #[test]
  fn optional_arglist_custom_expr_to_gd_test() {
    let expr = Expr::from_value(10, SourceOffset(0));
    assert_eq!(ArgList::empty().optional(vec!((String::from("foo"), expr))).to_gd(), "foo = 10");
  }

  #[test]
  fn prepend_required_test() {
    let mut args = ArgList::required(vec!(String::from("c"), String::from("d")));
    args.prepend_required(vec!(String::from("a"), String::from("b")).into_iter());
    assert_eq!(args.to_gd(), "a, b, c, d");
  }

  #[test]
  fn all_args_iter_test() {
    let args =
      ArgList::required(vec!(String::from("foo"), String::from("bar")))
      .optional(vec!((String::from("baz"), null())));
    let names: Vec<_> = args.all_args_iter().collect();
    assert_eq!(names, vec!("foo", "bar", "baz"));
  }

}
