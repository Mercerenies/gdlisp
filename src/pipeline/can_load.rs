
//! Provides the [`CanLoad`] trait.

use crate::runner::path::RPathBuf;
use super::Pipeline;

/// Trait for things that have a reasonable implementation of "load
/// the current file" as an expression. The typical example is
/// [`Pipeline`], and this trait is mainly provided as a way for the
/// type checker to say "I only need the load expression" as opposed
/// to requiring the whole `Pipeline` in general.
///
/// That is, there are many functions which need to take a `Pipeline`
/// but only do so in order to get the current file name. Those
/// functions can opt to take `impl CanLoad` instead, to demonstrate
/// to the viewer and the type checker that they only need a limited
/// part of the `Pipeline` functionality.
pub trait CanLoad {

  /// Gets the current filename, or `None` if there is no current
  /// file.
  fn current_filename_option(&self) -> Option<RPathBuf>;

  /// As
  /// [`current_filename_option`](CanLoad::current_filename_option),
  /// but panics in case of `None`. Generally, when we call the
  /// functions on this trait, we fully expect our pipeline (or
  /// equivalent object) to be in a fully-initialized state. If this
  /// precondition is violated, we should fail fast.
  ///
  /// # Panics
  ///
  /// Panics if `current_filename_option` returns `None`.
  fn current_filename(&self) -> RPathBuf {
    self.current_filename_option().expect("Could not identify current filename")
  }

}

impl CanLoad for Pipeline {
  fn current_filename_option(&self) -> Option<RPathBuf> {
    let mut filename = self.currently_loading_file()?.to_owned();
    filename.path_mut().set_extension("gd");
    Some(filename)
  }
}
