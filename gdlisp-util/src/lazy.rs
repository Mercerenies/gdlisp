// Copyright 2023, 2024 Silvio Mayolo
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn new_lazy_has_no_value_test() {
    let lazy = Lazy::new(|| 1);
    assert!(lazy.value.is_none());
    assert!(lazy.init.is_some());
  }

  #[test]
  fn forced_lazy_has_value_test() {
    let mut lazy = Lazy::new(|| 1);
    assert_eq!(*lazy.force_mut(), 1);
    assert_eq!(lazy.value, Some(1));
    assert!(lazy.init.is_none());
  }

  #[test]
  fn lazy_init_runs_only_once_test() {
    let mut call_count = 0;
    {
      let mut lazy = Lazy::new(|| {
        call_count += 1;
        42
      });
      // Force it multiple times.
      lazy.force_mut();
      lazy.force_mut();
      lazy.force_mut();
    }
    assert_eq!(call_count, 1);
  }

}
