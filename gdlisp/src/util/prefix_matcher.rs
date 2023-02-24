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

/// Provides the [`PrefixMatcher`] type, for efficiently finding a
/// matching prefix of a given string.

use std::collections::HashMap;
use std::iter;
use std::borrow::Borrow;

/// [`PrefixMatcher`] is a state machine designed for identifying one
/// of a finite number of prefixes.
pub struct PrefixMatcher<'a> {
  characters: HashMap<char, usize>,
  table: PrefixTable<usize>,
  terminal_states: Vec<Option<&'a str>>,
}

// A 2D array masquerading as a vector.
struct PrefixTable<T> {
  width_impl: usize,
  table: Vec<T>,
}

impl<T> PrefixTable<T> {

  fn new(width: usize) -> PrefixTable<T> {
    PrefixTable { width_impl: width, table: Vec::new() }
  }

  fn width(&self) -> usize {
    self.width_impl
  }

  fn height(&self) -> usize {
    self.table.len() / self.width()
  }

  fn get(&self, y: usize, x: usize) -> Option<&T> {
    if y >= self.height() || x >= self.width() {
      None
    } else {
      Some(&self.table[y * self.width() + x])
    }
  }

  fn get_mut(&mut self, y: usize, x: usize) -> Option<&mut T> {
    if y >= self.height() || x >= self.width() {
      None
    } else {
      let w = self.width();
      Some(&mut self.table[y * w + x])
    }
  }

  fn add_row(&mut self, row: impl Iterator<Item=T>) {
    let row: Vec<_> = row.collect();
    assert_eq!(row.len(), self.width());
    self.table.extend(row);
  }

}

impl<T: Clone> PrefixTable<T> {

  fn add_row_value(&mut self, value: T) {
    let iter = iter::repeat(value).take(self.width());
    self.add_row(iter);
  }

}

impl<'a> PrefixMatcher<'a> {

  /// Build a prefix matcher from a collection of strings. The slice
  /// of strings should not contain any duplicates, but it is
  /// permitted to contain strings which are prefixes of each other.
  pub fn build<I, S>(options: I) -> Self
  where S: Borrow<str> + ?Sized + 'a,
        I: Iterator<Item=&'a S> {
    let options: Vec<_> = options.collect();
    let characters = PrefixMatcher::build_chars_map(&options);

    let mut table = PrefixTable::new(characters.len());
    let mut terminal_states = Vec::new();

    table.add_row_value(EMPTY_CELL);
    terminal_states.push(None);

    for opt in options {
      let mut current_state: usize = 0;
      for ch in opt.borrow().chars() {
        let column = characters[&ch];
        let mut new_state = *table.get(current_state, column).unwrap();
        if new_state == EMPTY_CELL {
          new_state = table.height();
          table.add_row_value(EMPTY_CELL);
          terminal_states.push(None);
          *table.get_mut(current_state, column).unwrap() = new_state;
        }
        current_state = new_state;
      }
      terminal_states[current_state] = Some(opt.borrow());
    }

    PrefixMatcher { characters, table, terminal_states }
  }

  fn build_chars_map<S>(options: &[&S]) -> HashMap<char, usize>
  where S: Borrow<str> + ?Sized + 'a {
    let mut next_index: usize = 0;
    let mut result = HashMap::new();
    for string in options {
      for ch in (*string).borrow().chars() {
        result.entry(ch).or_insert_with(|| {
          next_index += 1;
          next_index - 1
        });
      }
    }
    result
  }

  /// Returns the longest prefix in this [`PrefixMatcher`] which is a
  /// prefix of the input string. If there is no match, returns
  /// `None`.
  pub fn identify_prefix(&self, string: &str) -> Option<&'a str> {
    let mut final_result: Option<&'a str> = None;
    let mut state: usize = 0;
    for ch in string.chars() {
      if let Some(new_result) = self.terminal_states[state] {
        final_result = Some(new_result);
      }
      match self.characters.get(&ch) {
        None => {
          // Unknown character, we're done.
          break;
        }
        Some(column) => {
          let new_state = *self.table.get(state, *column).unwrap();
          if new_state == EMPTY_CELL {
            // Unknown prefix, we're done.
            break;
          }
          state = new_state;
        }
      }
    }
    if let Some(new_result) = self.terminal_states[state] {
      final_result = Some(new_result);
    }
    final_result
  }

}

const EMPTY_CELL: usize = usize::MAX;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_chars_map() {
    let options = vec!("abc", "def", "aBC", "g");
    let intended_result = HashMap::from([
      ('a', 0), ('b', 1), ('c', 2), ('d', 3), ('e', 4), ('f', 5), ('B', 6), ('C', 7), ('g', 8),
    ]);
    assert_eq!(PrefixMatcher::build_chars_map(&options), intended_result);
  }

  #[test]
  fn test_empty_prefix_identifier() {
    let options: Vec<&'static str> = vec!();
    let matcher = PrefixMatcher::build(options.iter());
    assert_eq!(matcher.identify_prefix("abc"), None);
    assert_eq!(matcher.identify_prefix("def"), None);
    assert_eq!(matcher.identify_prefix(""), None);
    assert_eq!(matcher.identify_prefix("---"), None);
  }

  #[test]
  fn test_empty_string_prefix_identifier() {
    let options = vec!("");
    let matcher = PrefixMatcher::build(options.iter());
    assert_eq!(matcher.identify_prefix("abc"), Some(""));
    assert_eq!(matcher.identify_prefix("def"), Some(""));
    assert_eq!(matcher.identify_prefix(""), Some(""));
    assert_eq!(matcher.identify_prefix("---"), Some(""));
  }

  #[test]
  fn test_prefix_identifier_foobar() {
    let options = vec!("foo", "bar", "foobar");
    let matcher = PrefixMatcher::build(options.iter());
    assert_eq!(matcher.identify_prefix("abc"), None);
    assert_eq!(matcher.identify_prefix(""), None);
    assert_eq!(matcher.identify_prefix("foo"), Some("foo"));
    assert_eq!(matcher.identify_prefix("football"), Some("foo"));
    assert_eq!(matcher.identify_prefix("bar"), Some("bar"));
    assert_eq!(matcher.identify_prefix("barbecue"), Some("bar"));
    assert_eq!(matcher.identify_prefix("foobaz"), Some("foo"));
    assert_eq!(matcher.identify_prefix("foobar"), Some("foobar"));
    assert_eq!(matcher.identify_prefix("foobarbaz"), Some("foobar"));
  }

  #[test]
  fn test_prefix_identifier_foobar_with_empty() {
    // Empty string should act as a fallback if nothing else matches.
    let options = vec!("", "foo", "bar", "foobar");
    let matcher = PrefixMatcher::build(options.iter());
    assert_eq!(matcher.identify_prefix("abc"), Some(""));
    assert_eq!(matcher.identify_prefix(""), Some(""));
    assert_eq!(matcher.identify_prefix("foo"), Some("foo"));
    assert_eq!(matcher.identify_prefix("football"), Some("foo"));
    assert_eq!(matcher.identify_prefix("bar"), Some("bar"));
    assert_eq!(matcher.identify_prefix("barbecue"), Some("bar"));
    assert_eq!(matcher.identify_prefix("foobaz"), Some("foo"));
    assert_eq!(matcher.identify_prefix("foobar"), Some("foobar"));
    assert_eq!(matcher.identify_prefix("foobarbaz"), Some("foobar"));
  }

}
