// Copyright 2023 Silvio Mayolo
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

//! Provides the [`group_by`] function and associated iterator.

use std::iter::Peekable;

/// A grouping iterator. This iterator is constructed via [`group_by`]
/// or [`group_with`] and produces vectors of elements in the original
/// (input) iterator, grouped together by the predicate. See
/// [`group_by`] for details.
pub struct GroupBy<I: Iterator, F> {
  iter: Peekable<I>,
  function: F,
}

impl<I, F> Iterator for GroupBy<I, F>
where I: Iterator,
      F: FnMut(&<I as Iterator>::Item, &<I as Iterator>::Item) -> bool {

  type Item = Vec<<I as Iterator>::Item>;

  fn next(&mut self) -> Option<Self::Item> {
    let mut result: Vec<<I as Iterator>::Item> = Vec::new();
    let mut head = match self.iter.next() {
      None => {
        // Iterator is already exhausted, so there's no further groups.
        return None;
      }
      Some(x) => {
        x
      }
    };
    // Loop until either we've exhausted the iterator or we've hit an
    // element that doesn't match.
    loop {
      match self.iter.peek() {
        None => {
          // At end of iterator, so the group is complete.
          break;
        }
        Some(next) => {
          if (self.function)(&head, next) {
            // next is part of the current group, so continue.
            result.push(head);
            head = self.iter.next().expect("Inconsistent behavior in Peekable");
          } else {
            // next is not part of the current group, so break.
            break;
          }
        }
      }
    }
    // head is the final element of the current group.
    result.push(head);
    Some(result)
  }

}

/// Groups elements of the iterator based on the predicate function.
/// The resulting output iterator will produce vectors of items from
/// the original iterator, where the subvectors are the longest
/// subsequences for which the predicate returns true on consecutive
/// elements.
///
/// See also [`group_with`].
pub fn group_by<I, F>(iter: I, function: F) -> GroupBy<I, F>
where I: Iterator,
      F: FnMut(&<I as Iterator>::Item, &<I as Iterator>::Item) -> bool {
  GroupBy { iter: iter.peekable(), function }
}

/// Groups elements of the iterator based on the grouping function,
/// similar to [`group_by`]. The grouping function takes one argument
/// and returns a value that can be compared for equality. The
/// resulting iterator is grouped by consecutive terms which, under
/// the supplied unary function, produce values which are considered
/// equal.
pub fn group_with<I, F, G>(iter: I, mut function: F) -> GroupBy<I, impl FnMut(&<I as Iterator>::Item, &<I as Iterator>::Item) -> bool>
where I: Iterator,
      F: FnMut(&<I as Iterator>::Item) -> G,
      G: Eq {
  group_by(iter, move |a, b| {
    function(a) == function(b)
  })
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn group_by_test_1() {
    let vec = vec!(0, 0, 1, 1, 2, 3, 2, 2);
    let grouped: Vec<_> = group_by(vec.into_iter(), |a, b| a == b).collect();
    assert_eq!(grouped, vec!(vec!(0, 0), vec!(1, 1), vec!(2), vec!(3), vec!(2, 2)));
  }

  #[test]
  fn group_by_test_2() {
    let vec = vec!(0, 0, 1, 1, 2, 3, 2, 2, 1);
    let grouped: Vec<_> = group_by(vec.into_iter(), |a, b| a == b).collect();
    assert_eq!(grouped, vec!(vec!(0, 0), vec!(1, 1), vec!(2), vec!(3), vec!(2, 2), vec!(1)));
  }

  #[test]
  fn group_by_test_3() {
    let vec: Vec<i32> = vec!();
    let grouped: Vec<_> = group_by(vec.into_iter(), |a, b| a == b).collect();
    assert_eq!(grouped, Vec::<Vec<i32>>::new());
  }

  #[test]
  fn group_with_test() {
    let vec: Vec<i32> = vec!(0, 0, 1, 1, -1, 2, -3);
    let grouped: Vec<_> = group_with(vec.into_iter(), |a| a.abs()).collect();
    assert_eq!(grouped, vec!(vec!(0, 0), vec!(1, 1, -1), vec!(2), vec!(-3)));
  }

}
