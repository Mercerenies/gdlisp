
//! Provides the [`One`] type, a trivial collection which always
//! contains one value.

use std::mem::swap;

/// A `One<T>` always contains a single `T`. `One<T>` is a collection
/// type and can be iterated over.
#[derive(Clone, Debug, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct One<T>(pub T);

/// The underlying iterator for [`One`]. Returned by [`One::iter`].
pub struct Iter<'a, T>(Option<&'a T>);

/// The underlying mutable iterator for [`One`]. Returned by [`One::iter_mut`].
pub struct IterMut<'a, T>(Option<&'a mut T>);

/// The underlying owning iterator for [`One`]. Returned by [`One::into_iter`].
#[derive(Clone)]
pub struct IntoIter<T>(Option<T>);

impl<T> One<T> {

  pub fn iter(&self) -> Iter<'_, T> {
    Iter(Some(&self.0))
  }

  pub fn iter_mut(&mut self) -> IterMut<'_, T> {
    IterMut(Some(&mut self.0))
  }

}

// Default implementation imposes T: Clone, which is unnecessary.
impl<'a, T> Clone for Iter<'a, T> {
  fn clone(&self) -> Self {
    Iter(self.0)
  }
}

impl<T> IntoIterator for One<T> {
  type Item = T;
  type IntoIter = IntoIter<T>;

  fn into_iter(self) -> IntoIter<T> {
    IntoIter(Some(self.0))
  }

}

impl<'a, T> Iterator for Iter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<&'a T> {
    let mut result = None;
    swap(&mut self.0, &mut result);
    result
  }

}

impl<'a, T> Iterator for IterMut<'a, T> {
  type Item = &'a mut T;

  fn next(&mut self) -> Option<&'a mut T> {
    let mut result = None;
    swap(&mut self.0, &mut result);
    result
  }

}

impl<'a, T> Iterator for IntoIter<T> {
  type Item = T;

  fn next(&mut self) -> Option<T> {
    let mut result = None;
    swap(&mut self.0, &mut result);
    result
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn one_iter() {
    let one = One(100);
    assert_eq!(one.iter().collect::<Vec<_>>(), vec!(&100));
  }

  #[test]
  fn one_iter_mut() {
    let mut one = One(100);
    for x in one.iter_mut() {
      *x = 200;
    }
    assert_eq!(one, One(200));
  }


  #[test]
  fn one_into_iter() {
    let one = One(100);
    for x in one {
      assert_eq!(x, 100);
    }
  }

}
