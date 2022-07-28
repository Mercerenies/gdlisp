
//! Provides the [`Lazy`] type for lazy initialization of data.

/// A lazy data structure, containing a `T` which may or may not be
/// initialized yet.
#[derive(Clone, Debug, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Lazy<T, F> {
  value: Option<T>,
  init: Option<F>,
}

impl<T, F: FnOnce() -> T> Lazy<T, F> {

  // Implementation detail: A Lazy<T, F> shall always have exactly one
  // of `value` or `init` defined. The other shall be empty. An
  // uninitialized lazy value has only `init` and an initialized one
  // has only `value`. If both or neither is defined at once, the
  // object is in an illegal state.

  pub fn new(init: F) -> Lazy<T, F> {
    Lazy { value: None, init: Some(init) }
  }

  pub fn force_mut(&mut self) -> &mut T {
    if self.value.is_none() {
      let init = self.init.take().expect("Internal error in gdlisp::util::lazy");
      self.value = Some(init());
    }
    self.value.as_mut().expect("Internal error in gdlisp::util::lazy")
  }

}
