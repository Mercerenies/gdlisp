
pub mod debug_wrapper;

pub fn fold1<I : Iterator, F : FnMut(I::Item, I::Item) -> I::Item>(iter: I, mut f: F) -> Option<I::Item> {
  iter.fold(None, |x, y| match x {
    None => Some(y),
    Some(x) => Some(f(x, y)),
  })
}

pub struct PairIter<T, I>(Option<T>, I);

pub fn each_pair<'a, T, I>(iter: I) -> PairIter<T, I> where I : Iterator<Item=T> {
  PairIter(None, iter)
}

impl<T, I> Iterator for PairIter<T, I> where I : Iterator<Item=T>, T : Clone {
  type Item = (T, T);

  fn next(&mut self) -> Option<(T, T)> {
    let fst = self.0.clone().or_else(|| self.1.next());
    let snd = self.1.next();
    self.0 = snd.clone();
    fst.and_then(|fst| snd.and_then(|snd| Some((fst, snd))))
  }

}

// TODO Tests (Especially for each_pair)
