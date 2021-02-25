
pub mod debug_wrapper;

use std::collections::HashMap;
use std::hash::Hash;
use std::io::{self, Read};

pub fn fold1<I : Iterator, F : FnMut(I::Item, I::Item) -> I::Item>(iter: I, mut f: F) -> Option<I::Item> {
  iter.fold(None, |x, y| match x {
    None => Some(y),
    Some(x) => Some(f(x, y)),
  })
}

pub struct PairIter<T, I>(Option<T>, I);

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

pub fn read_to_end(input: &mut impl Read) -> io::Result<String> {
  let mut vec = Vec::new();
  input.read_to_end(&mut vec)?;
  String::from_utf8(vec).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

#[cfg(test)]
mod tests {
  use super::*;

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
  fn test_each_pair() {
    let vec1: Vec<i32> = vec!();
    assert_eq!(each_pair(vec1.into_iter()).collect::<Vec<_>>(), vec!());

    let vec2: Vec<i32> = vec!(10);
    assert_eq!(each_pair(vec2.into_iter()).collect::<Vec<_>>(), vec!());

    let vec3: Vec<i32> = vec!(10, 20);
    assert_eq!(each_pair(vec3.into_iter()).collect::<Vec<_>>(), vec!((10, 20)));

    let vec4: Vec<i32> = vec!(10, 20, 30);
    assert_eq!(each_pair(vec4.into_iter()).collect::<Vec<_>>(), vec!((10, 20), (20, 30)));
  }

}
