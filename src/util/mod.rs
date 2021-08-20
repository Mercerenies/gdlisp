
//! Various utility functions that don't have a better home.

pub mod debug_wrapper;
pub mod lattice;
pub mod one;

use std::collections::HashMap;
use std::hash::Hash;
use std::io::{self, Read};
use std::convert::Infallible;

/// `fold1` acts like [`Iterator::fold`] but without a "starting"
/// value.
///
/// If `iter` is nonempty, then the first element is used as the
/// starting value, and iteration begins formally at the second. If
/// `iter` is empty, then [`None`] is returned.
pub fn fold1<I : Iterator, F : FnMut(I::Item, I::Item) -> I::Item>(iter: I, mut f: F) -> Option<I::Item> {
  iter.fold(None, |x, y| match x {
    None => Some(y),
    Some(x) => Some(f(x, y)),
  })
}

/// The type of iterator returned by [`each_pair`].
pub struct PairIter<T, I>(Option<T>, I);

/// Return an iterator over each pair of adjacent elements in `iter`.
///
/// # Examples
///
/// ```
/// # use gdlisp::util::each_pair;
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
pub fn unzip_err<I, A, B, E, FromA, FromB>(iter: I) -> Result<(FromA, FromB), E>
where I : Iterator<Item=Result<(A, B), E>>,
      FromA : Default + Extend<A>,
      FromB : Default + Extend<B> {
  let mut from_a = FromA::default();
  let mut from_b = FromB::default();
  for term in iter {
    let (a, b) = term?;
    from_a.extend(one::One(a));
    from_b.extend(one::One(b));
  }
  Ok((from_a, from_b))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[derive(Eq, PartialEq, Copy, Clone, Debug)]
  struct FakeError;

  #[test]
  fn test_fold1() {
    let vec1: Vec<i32> = vec!(1, 2, 3, 4);
    assert_eq!(fold1(vec1.into_iter(), |x, y| x + y), Some(10));

    let vec2: Vec<i32> = vec!();
    assert_eq!(fold1(vec2.into_iter(), |x, y| x + y), None);

    let vec3: Vec<i32> = vec!(1, 2, 3, 4);
    assert_eq!(fold1(vec3.into_iter(), |x, y| x - y), Some(-8));
  }

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

}
