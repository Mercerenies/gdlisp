
pub mod debug_wrapper;

pub fn fold1<I : Iterator, F : FnMut(I::Item, I::Item) -> I::Item>(iter: I, mut f: F) -> Option<I::Item> {
  iter.fold(None, |x, y| match x {
    None => Some(y),
    Some(x) => Some(f(x, y)),
  })
}
