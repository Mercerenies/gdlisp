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

//! Various utility functions that don't have a better home.

pub mod debug_wrapper;
pub mod group_by;
pub mod lattice;
pub mod lazy;
pub mod path;
pub mod prefix_matcher;
pub mod recursive;

use std::collections::HashMap;
use std::hash::Hash;
use std::io::{self, Read};
use std::convert::Infallible;
use std::iter::once;

/// The type of iterator returned by [`each_pair`].
pub struct PairIter<T, I>(Option<T>, I);

/// Return an iterator over each pair of adjacent elements in `iter`.
///
/// # Examples
///
/// ```
/// # use gdlisp_util::each_pair;
/// let vec1: Vec<i32> = vec!();
/// assert_eq!(each_pair(vec1.into_iter()).collect::<Vec<_>>(), vec!());
///
/// let vec2: Vec<i32> = vec!(10);
/// assert_eq!(each_pair(vec2.into_iter()).collect::<Vec<_>>(), vec!());
///
/// let vec3: Vec<i32> = vec!(10, 20);
/// assert_eq!(each_pair(vec3.into_iter()).collect::<Vec<_>>(), vec!((10, 20)));
///
/// let vec4: Vec<i32> = vec!(10, 20, 30);
/// assert_eq!(each_pair(vec4.into_iter()).collect::<Vec<_>>(), vec!((10, 20), (20, 30)));
/// ```
pub fn each_pair<T, I>(iter: I) -> PairIter<T, I> where I : Iterator<Item=T> {
  PairIter(None, iter)
}

impl<T, I> Iterator for PairIter<T, I> where I : Iterator<Item=T>, T : Clone {
  type Item = (T, T);

  fn next(&mut self) -> Option<(T, T)> {
    let fst = self.0.clone().or_else(|| self.1.next());
    let snd = self.1.next();
    self.0 = snd.clone();
    fst.and_then(|fst| snd.map(|snd| (fst, snd)))
  }

}

/// The type of iterator returned by [`each_non_overlapping_pair`].
pub struct NonOverlappingPairIter<I>(I);

/// Return an iterator over each non-overlapping pair of adjacent
/// elements in `iter`. If there are an odd number of elements, then
/// the final element will not be included in any of the pairs.
///
/// # Examples
///
/// ```
/// # use gdlisp_util::each_non_overlapping_pair;
/// let vec1: Vec<i32> = vec!();
/// assert_eq!(each_non_overlapping_pair(vec1.into_iter()).collect::<Vec<_>>(), vec!());
///
/// let vec2: Vec<i32> = vec!(10);
/// assert_eq!(each_non_overlapping_pair(vec2.into_iter()).collect::<Vec<_>>(), vec!());
///
/// let vec3: Vec<i32> = vec!(10, 20);
/// assert_eq!(each_non_overlapping_pair(vec3.into_iter()).collect::<Vec<_>>(), vec!((10, 20)));
///
/// let vec4: Vec<i32> = vec!(10, 20, 30);
/// assert_eq!(each_non_overlapping_pair(vec4.into_iter()).collect::<Vec<_>>(), vec!((10, 20)));
///
/// let vec5: Vec<i32> = vec!(10, 20, 30, 40);
/// assert_eq!(each_non_overlapping_pair(vec5.into_iter()).collect::<Vec<_>>(), vec!((10, 20), (30, 40)));
/// ```
pub fn each_non_overlapping_pair<I: Iterator>(iter: I) -> NonOverlappingPairIter<I> {
  NonOverlappingPairIter(iter)
}

impl<I: Iterator> Iterator for NonOverlappingPairIter<I> {
  type Item = (<I as Iterator>::Item, <I as Iterator>::Item);

  fn next(&mut self) -> Option<Self::Item> {
    let fst = self.0.next();
    let snd = self.0.next();
    fst.and_then(|fst| snd.map(|snd| (fst, snd)))
  }

}

/// Given two [`HashMap`] values `a` and `b`, merge `b` into `a`.
///
/// For every key of `b` which is not in `a`, that key-value pair is
/// added verbatim to `a`. For every key of `b` which *is* present in
/// `a`, apply `merge_fn` to the two values to combine them, storing
/// the result in `a`. The `a` value is passed to `merge_fn` before
/// the `b` value.
pub fn merge_hashmap_inplace<K : Eq + Hash, V>(a: &mut HashMap<K, V>,
                                               b: HashMap<K, V>,
                                               mut merge_fn: impl FnMut(V, V) -> V) {
  for (k, v) in b {
    match a.remove(&k) {
      None => a.insert(k, v),
      Some(v1) => a.insert(k, merge_fn(v, v1)),
    };
  }
}

/// Consume the `Read` instance, as though using [`Read::read_to_end`].
///
/// Accumulates the result into a string, returning an error of kind
/// [`io::ErrorKind::InvalidData`] if the data is not valid UTF-8.
pub fn read_to_end(input: &mut impl Read) -> io::Result<String> {
  let mut vec = Vec::new();
  input.read_to_end(&mut vec)?;
  String::from_utf8(vec).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Convert the `Option` to a `Vec`, returning a vector of length
/// either zero or one.
pub fn option_to_vec<T>(value: Option<T>) -> Vec<T> {
  match value {
    None => vec!(),
    Some(x) => vec!(x),
  }
}

/// This is equivalent to the nightly-only Rust function
/// `Result::into_ok`, for safely unwrapping `Result` values which can
/// provably never be an error.
pub fn extract_err<T>(value: Result<T, Infallible>) -> T {
  match value {
    Ok(value) => value,
    Err(contra) => match contra {},
  }
}

/// Unzip an iterator into two collections, taking error values into
/// consideration. This is the spiritual composition of
/// [`Iterator::unzip`] and the
/// [`FromIterator`](std::iter::FromIterator) implementation on
/// [`Result`].
pub fn unzip_err<E, FromA, FromB, I, A, B>(iter: I) -> Result<(FromA, FromB), E>
where I : Iterator<Item=Result<(A, B), E>>,
      FromA : Default + Extend<A>,
      FromB : Default + Extend<B> {
  let mut from_a = FromA::default();
  let mut from_b = FromB::default();
  for term in iter {
    let (a, b) = term?;
    from_a.extend(once(a));
    from_b.extend(once(b));
  }
  Ok((from_a, from_b))
}

/// Returns a matching element from the vector, mutably.
///
/// If no matching element is found, then a new element is appended
/// and returned.
#[allow(clippy::redundant_closure)] // Using &mut pred for one and not the other reduces readability.
pub fn find_or_else_mut<T>(vec: &mut Vec<T>, default: impl FnOnce() -> T, mut pred: impl FnMut(&T) -> bool) -> &mut T {
  if vec.iter().any(|x| pred(x)) {
    vec.iter_mut().find(|x| pred(x)).expect("Internal error in find_or_else_mut")
  } else {
    vec.push(default());
    vec.last_mut().expect("Internal error in find_or_else_mut")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[derive(Eq, PartialEq, Copy, Clone, Debug)]
  struct FakeError;

  #[test]
  fn test_extract_err() {
    assert_eq!(extract_err(Ok(1)), 1);
  }

  #[test]
  fn test_unzip_err_1() {
    let vec1: Vec<Result<(i32, i32), FakeError>> = vec!(Ok((1, 10)), Ok((2, 20)), Ok((3, 30)));
    assert_eq!(unzip_err(vec1.into_iter()), Ok((vec!(1, 2, 3), vec!(10, 20, 30))));

    let vec2: Vec<Result<(i32, i32), FakeError>> = vec!(Ok((1, 10)), Err(FakeError), Ok((3, 30)));
    let unzip_result2: Result<(Vec<_>, Vec<_>), _> = unzip_err(vec2.into_iter());
    assert_eq!(unzip_result2, Err(FakeError));

    let vec3: Vec<Result<(i32, i32), FakeError>> = vec!();
    assert_eq!(unzip_err(vec3.into_iter()), Ok((vec!(), vec!())));
  }

  #[test]
  fn test_unzip_err_2() {
    // Make sure errors actually abort the process and the iterator is
    // lazy.
    let mut count = 0;
    let vec = vec!(1, 2, 3, 4, 5);
    let iter = vec.into_iter().map(|x| {
      match x {
        1 => { count += 1; }
        3 => { return Err(FakeError) }
        5 => { count += 100; }
        _ => {}
      };
      Ok((x, 0))
    });
    let unzip_result: Result<(Vec<_>, Vec<_>), _> = unzip_err(iter);
    assert_eq!(unzip_result, Err(FakeError));
    // Only the count += 1 should have run, not the count += 100
    assert_eq!(count, 1);
  }

  #[test]
  fn find_or_else_mut_1() {
    // If it already exists
    let mut v = vec!(1, 2, 3, 4);
    {
      let m = find_or_else_mut(&mut v, || panic!("default case called unexpectedly"), |x| *x == 3);
      assert_eq!(m, &mut 3);
    }
    assert_eq!(v, vec!(1, 2, 3, 4));
  }

  #[test]
  fn find_or_else_mut_2() {
    // If it doesn't exist
    let mut v = vec!(1, 2, 3, 4);
    {
      let m = find_or_else_mut(&mut v, || 10, |x| *x == 10);
      assert_eq!(m, &mut 10);
    }
    assert_eq!(v, vec!(1, 2, 3, 4, 10));
  }

}
