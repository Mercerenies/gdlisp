
//! Data structures for representing and manipulating valid GDScript
//! code.

pub mod arglist;
pub mod decl;
pub mod expr;
pub mod expr_wrapper;
pub mod inner_class;
pub mod library;
pub mod literal;
pub mod metadata;
pub mod op;
pub mod pattern;
pub mod spacing;
pub mod stmt;

use regex::Regex;

use std::fmt;

/// Indent to the given position with spaces.
///
/// # Examples
///
/// ```
/// # use gdlisp::gdscript::indent;
/// let mut str = String::new();
/// indent(&mut str, 4);
/// assert_eq!(str, "    ");
/// ```
pub fn indent<W: fmt::Write>(w: &mut W, ind: u32) -> Result<(), fmt::Error> {
  let spaces = " ".repeat(ind as usize);
  write!(w, "{}", spaces)
}

/// Determines whether the string is a valid GDScript identifier, by
/// the rules of the language. A GDScript identifier consists only of
/// ASCII alphanumeric characters and underscore and cannot begin with
/// an underscore.
pub fn is_valid_identifier(s: &str) -> bool {
  lazy_static! {
    static ref RE: Regex = Regex::new(r#"\A[A-Za-z_][A-Za-z0-9_]*\z"#).unwrap();
  }
  RE.is_match(s)
}

/// Determines whether the string is a valid GDScript node path. A
/// valid GDScript node path is a sequence of one or more valid
/// identifiers, delimited by forward slashes.
///
/// Note: This function checks the *path* portion of the node path
/// syntax, so a string containing a `$` will not be accepted.
/// Additionally, this function is not designed to check the extended
/// quoted node path syntax, so a string containing `"` will not be
/// accepted.
pub fn is_valid_node_path(s: &str) -> bool {
  s.split('/').all(is_valid_identifier)
}

#[cfg(test)]
mod tests {
  use super::*;

  fn do_indent(ind: u32) -> String {
    let mut x = String::new();
    indent(&mut x, ind).unwrap();
    x
  }

  #[test]
  fn test_indent() {
    assert_eq!(do_indent(0), "");
    assert_eq!(do_indent(1), " ");
    assert_eq!(do_indent(2), "  ");
    assert_eq!(do_indent(3), "   ");
    assert_eq!(do_indent(4), "    ");
  }

  #[test]
  fn test_identifier() {

    // Valid
    assert_eq!(is_valid_identifier("foo"), true);
    assert_eq!(is_valid_identifier("foo99"), true);
    assert_eq!(is_valid_identifier("FOO99"), true);
    assert_eq!(is_valid_identifier("_"), true);
    assert_eq!(is_valid_identifier("_a_b_c"), true);
    assert_eq!(is_valid_identifier("_00_00_"), true);

    // Invalid
    assert_eq!(is_valid_identifier(""), false);
    assert_eq!(is_valid_identifier("0"), false);
    assert_eq!(is_valid_identifier("0.0"), false);
    assert_eq!(is_valid_identifier("a.b"), false);
    assert_eq!(is_valid_identifier("c/d"), false);
    assert_eq!(is_valid_identifier("-"), false);

  }

  #[test]
  fn test_path() {

    // Valid
    assert_eq!(is_valid_node_path("foo"), true);
    assert_eq!(is_valid_node_path("foo99"), true);
    assert_eq!(is_valid_node_path("_"), true);
    assert_eq!(is_valid_node_path("_a_b_c"), true);
    assert_eq!(is_valid_node_path("_00_00_"), true);
    assert_eq!(is_valid_node_path("a/b"), true);
    assert_eq!(is_valid_node_path("a/b/cdef"), true);
    assert_eq!(is_valid_node_path("A/B/C"), true);

    // Invalid
    assert_eq!(is_valid_node_path(""), false);
    assert_eq!(is_valid_node_path("0"), false);
    assert_eq!(is_valid_node_path("0.0"), false);
    assert_eq!(is_valid_node_path("a.b"), false);
    assert_eq!(is_valid_node_path("c//d"), false);
    assert_eq!(is_valid_node_path("/a"), false);
    assert_eq!(is_valid_node_path("a/"), false);
    assert_eq!(is_valid_node_path("/"), false);
    assert_eq!(is_valid_node_path("/0"), false);
    assert_eq!(is_valid_node_path("0/"), false);
    assert_eq!(is_valid_node_path("foo/bar/0"), false);
    assert_eq!(is_valid_node_path("-"), false);

  }

}
