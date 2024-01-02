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

//! Defines the [`SExpr`] type representing a Lisp S-expression.

use crate::literal::Literal;
use gdlisp_util::source::{SourceOffset, Sourced};
use gdlisp_util::extract_err;
use gdlisp_util::recursive::Recursive;

use std::fmt;
use std::cmp::max;
use std::convert::Infallible;

/// The basic type used for representing Lisp S-expressions.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum SExprF {
  /// An non-recursive S-expression containing a simple literal value.
  Atom(Literal),
  /// A pair of values. The first value is referred to as the car and
  /// the second as the cdr. All *proper* Lisp lists are made up of
  /// cons cells and [`Literal::Nil`]. Displays as `(car . cdr)`.
  Cons(Box<SExpr>, Box<SExpr>),
}

/// An `SExpr` is an [`SExprF`] together with information about the
/// offset in the source code of the S-expression.
///
/// See [`Sourced`](gdlisp_util::source::Sourced) for more details on
/// this representation.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct SExpr {
  pub value: SExprF,
  pub pos: SourceOffset,
}

fn fmt_list(a: &SExpr, b: &SExpr, f: &mut fmt::Formatter<'_>) -> fmt::Result {
  match &b.value {
    SExprF::Atom(Literal::Nil) =>
      // End of list; just print the known value
      write!(f, "{}", a),
    SExprF::Cons(b1, c1) => {
      // Another cons cell in cdr; continue printing list
      write!(f, "{} ", a)?;
      fmt_list(b1, c1, f)
    },
    _ =>
      // Dotted list; print with dot
      write!(f, "{} . {}", a, b)
  }
}

impl SExprF {

  /// The nil S-expression.
  pub const NIL: SExprF = SExprF::Atom(Literal::Nil);

}

impl SExpr {

  /// A new `SExpr` containing the given value and position.
  /// Equivalent to `SExpr { value, pos }`.
  pub fn new(value: SExprF, pos: SourceOffset) -> Self {
    Self { value, pos }
  }

  /// A [`Literal::Nil`] S-expression with source information.
  pub fn nil(pos: SourceOffset) -> Self {
    Self { value: SExprF::Atom(Literal::Nil), pos }
  }

  /// A [`Literal::Symbol`] with the given value.
  pub fn symbol<S>(name: S, pos: SourceOffset) -> Self
  where String : From<S> {
    Self { value: SExprF::Atom(Literal::symbol(name)), pos }
  }

  /// A [`Literal::String`] with the given value.
  pub fn string<S>(name: S, pos: SourceOffset) -> Self
  where String : From<S> {
    Self { value: SExprF::Atom(Literal::string(name)), pos }
  }

  /// A [`Literal::Int`] with the given value.
  pub fn int(value: i32, pos: SourceOffset) -> Self {
    Self { value: SExprF::Atom(Literal::from(value)), pos }
  }

  /// A [`Literal::Float`] with the given value.
  pub fn float(value: f32, pos: SourceOffset) -> Self {
    Self { value: SExprF::Atom(Literal::from(value)), pos }
  }

  /// An [`SExprF::Cons`] with the given value.
  pub fn cons(car: SExpr, cdr: SExpr, pos: SourceOffset) -> Self {
    Self::new(SExprF::Cons(Box::new(car), Box::new(cdr)), pos)
  }

  fn _recurse<'a, 'b, F1, F2, E>(&'a self, func: &mut F1, default: &mut F2) -> Result<(), E>
  where F1 : FnMut(&'b SExpr) -> Result<(), E>,
        F2 : FnMut() -> Result<(), E>,
        'a : 'b {
    match &self.value {
      SExprF::Cons(car, cdr) => {
        func(car)?;
        func(cdr)?;
      }
      SExprF::Atom(_) => {
        default()?;
      }
    }
    Ok(())
  }

  fn _recurse_mut<'a, 'b, F1, F2, E>(&'a mut self, func: &mut F1, default: &mut F2) -> Result<(), E>
  where F1 : FnMut(&'b mut SExpr) -> Result<(), E>,
        F2 : FnMut() -> Result<(), E>,
        'a : 'b {
    match &mut self.value {
      SExprF::Cons(car, cdr) => {
        func(&mut *car)?;
        func(&mut *cdr)?;
      }
      SExprF::Atom(_) => {
        default()?;
      }
    }
    Ok(())
  }

  fn _walk_preorder<'a, 'b, F, E>(&'a self, func: &mut F) -> Result<(), E>
  where F: FnMut(&'b SExpr) -> Result<(), E>,
        'a: 'b {
    func(self)?;
    self._recurse(&mut |x| x._walk_preorder(func), &mut || Ok(()))
  }

  fn _walk_preorder_mut<'a, F, E>(&'a mut self, func: &mut F) -> Result<(), E>
  where F: for<'b> FnMut(&'b mut SExpr) -> Result<(), E> {
    func(self)?;
    self._recurse_mut(&mut |x| x._walk_preorder_mut(func), &mut || Ok(()))
  }

  fn _walk_postorder<'a, 'b, F, E>(&'a self, func: &mut F) -> Result<(), E>
  where F: FnMut(&'b SExpr) -> Result<(), E>,
        'a: 'b {
    self._recurse(&mut |x| x._walk_postorder(func), &mut || Ok(()))?;
    func(self)
  }

  fn _walk_postorder_mut<'a, F, E>(&'a mut self, func: &mut F) -> Result<(), E>
  where F: for<'b> FnMut(&'b mut SExpr) -> Result<(), E> {
    self._recurse_mut(&mut |x| x._walk_postorder_mut(func), &mut || Ok(()))?;
    func(self)
  }

  /// Walk the S-expression, calling a function on the node itself and
  /// every child recursively. That includes both elements of an
  /// [`SExprF::Cons`] recursively. The function will be called on the
  /// current node *before* recursing on its children.
  ///
  /// Any error that occurs during walking will be propagated to the
  /// caller.
  pub fn walk_preorder<'a, 'b, F, E>(&'a self, mut func: F) -> Result<(), E>
  where F: FnMut(&'b SExpr) -> Result<(), E>,
        'a: 'b {
    self._walk_preorder(&mut func)
  }

  /// As [`SExpr::walk_preorder`], but with a mutable `self`.
  pub fn walk_preorder_mut<'a, F, E>(&'a mut self, mut func: F) -> Result<(), E>
  where F: for<'b> FnMut(&'b mut SExpr) -> Result<(), E> {
    self._walk_preorder_mut(&mut func)
  }

  /// Walk the S-expression, calling a function on the node itself and
  /// every child recursively. That includes both elements of an
  /// [`SExprF::Cons`] recursively. The function will be called on the
  /// current node only *after* recursing on its children.
  ///
  /// Any error that occurs during walking will be propagated to the
  /// caller.
  pub fn walk_postorder<'a, 'b, F, E>(&'a self, mut func: F) -> Result<(), E>
  where F: FnMut(&'b SExpr) -> Result<(), E>,
        'a: 'b {
    self._walk_postorder(&mut func)
  }

  /// As [`SExpr::walk_postorder`], but with a mutable `self`.
  pub fn walk_postorder_mut<'a, F, E>(&'a mut self, mut func: F) -> Result<(), E>
  where F: for<'b> FnMut(&'b mut SExpr) -> Result<(), E> {
    self._walk_postorder_mut(&mut func)
  }

  /// Walk the S-expression, transforming all of the [`SourceOffset`]
  /// tags using the given function. The walk is performed using
  /// [`SExpr::walk_preorder_mut`].
  pub fn each_source_mut<F>(&mut self, mut func: F)
  where F: FnMut(SourceOffset) -> SourceOffset {
    let result = self.walk_preorder_mut(|ast| {
      ast.pos = func(ast.pos);
      Ok(())
    });
    extract_err(result)
  }

  /// As [`SExpr::each_source_mut`], but returns a new object.
  pub fn map_source<F>(&self, func: F) -> SExpr
  where F: FnMut(SourceOffset) -> SourceOffset {
    let mut ast = self.clone();
    ast.each_source_mut(func);
    ast
  }

  /// Walk the `SExpr`, producing a list of all symbols that appear
  /// (as [`Literal::Symbol`]) anywhere in the tree. The symbols will
  /// appear in the resulting list in the order they appear in the
  /// `SExpr`, and any duplicates will be represented multiple times,
  /// once for each appearance.
  pub fn all_symbols<'a>(&'a self) -> Vec<&'a str> {
    let mut result: Vec<&'a str> = Vec::new();
    let err = self.walk_preorder::<_, Infallible>(|x| {
      if let SExprF::Atom(Literal::Symbol(x)) = &x.value {
        result.push(x);
      }
      Ok(())
    });
    extract_err(err);
    result
  }

  /// In Lisp, we generally think of a *dotted list* as a sequence of
  /// zero or more cons cells, where the cdr of each cell is the next
  /// cons cell, eventually terminated by some non-cons value. For
  /// instance, `(1 . (2 . (3 . 4)))` would be a dotted list where the
  /// values in the "list" portion are `1`, `2`, and `3`, and the
  /// terminator is `4`.
  ///
  /// We call a dotted list which terminates in `()` (i.e.
  /// [`Literal::Nil`]) a *proper list*. Some sources explicitly
  /// define a dotted list to *not* be a proper list, but this
  /// documentation does not make that distinction.
  ///
  /// This function constructs an [`SExprF`] value from a sequence of
  /// values `vec` and a terminator `terminal`. For each value in the
  /// sequence, a [`SExprF::Cons`] cell will be constructed, and the
  /// final cdr will be `terminal`.
  ///
  /// For the inverse operation of converted an [`SExprF`] *back* into a
  /// sequence and terminator, see [`crate::dotted::DottedExpr`].
  pub fn dotted_list(vec: Vec<SExpr>, terminal: SExpr) -> SExpr {
    vec.into_iter().rev().fold(terminal, SExpr::dotted_list_fold) // NOTE: Arguments reversed
  }

  fn dotted_list_fold(cdr: SExpr, car: SExpr) -> SExpr { // NOTE: Arguments reversed from the usual order
    let pos = car.pos;
    SExpr::new(SExprF::Cons(Box::new(car), Box::new(cdr)), pos)
  }

  /// A dotted list terminated by nil at the given source position.
  pub fn list(vec: Vec<SExpr>, nil_pos: SourceOffset) -> SExpr {
    SExpr::dotted_list(vec, SExpr::nil(nil_pos))
  }

  /// Uses a [`From`] instance of [`SExprF`] to construct an `SExpr`.
  pub fn from_value<T>(value: T, pos: SourceOffset) -> SExpr
  where SExprF : From<T> {
    SExpr::new(SExprF::from(value), pos)
  }

  /// If `self` stores a [`Literal::Symbol`], then a reference to the
  /// inside of that symbol is returned. Otherwise, `None` is
  /// returned.
  pub fn as_symbol_ref(&self) -> Option<&str> {
    if let SExprF::Atom(Literal::Symbol(s)) = &self.value {
      Some(s)
    } else {
      None
    }
  }

}

/// Pretty-print an S-expression, using a format compatible with
/// [`parser`](crate::parser). Cons cells whose cdr is a cons cell
/// will be pretty-printed as list prefixes.
impl fmt::Display for SExpr {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      SExprF::Atom(lit) => write!(f, "{}", lit),
      SExprF::Cons(a, b) => {
        write!(f, "(")?;
        fmt_list(a, b, f)?;
        write!(f, ")")
      }
    }
  }

}

impl Sourced for SExpr {
  type Item = SExprF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &SExprF {
    &self.value
  }

}

impl Recursive for SExpr {

  fn depth(&self) -> u32 {
    match &self.value {
      SExprF::Atom(_) => 1,
      SExprF::Cons(a, b) => 1 + max(a.depth(), b.depth()),
    }
  }

}

impl From<()> for SExprF {
  fn from(_: ()) -> SExprF {
    SExprF::Atom(Literal::Nil)
  }
}

impl From<i32> for SExprF {
  fn from(n: i32) -> SExprF {
    SExprF::Atom(Literal::from(n))
  }
}

impl From<bool> for SExprF {
  fn from(b: bool) -> SExprF {
    SExprF::Atom(Literal::from(b))
  }
}

impl From<f32> for SExprF {
  fn from(f: f32) -> SExprF {
    SExprF::Atom(Literal::from(f))
  }
}

impl From<String> for SExprF {
  fn from(s: String) -> SExprF {
    SExprF::Atom(Literal::String(s))
  }
}

impl<'a> From<&'a str> for SExprF {
  fn from(s: &'a str) -> SExprF {
    SExprF::Atom(Literal::String(String::from(s)))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::string::ToString;

  // A handful of helpers for the tests that don't care about
  // SourceOffset and are only testing the structure. These just fill
  // in SourceOffset::default() wherever necessary.

  fn int(n: i32) -> SExpr {
    SExpr::new(SExprF::from(n), SourceOffset::default())
  }

  fn nil() -> SExpr {
    SExpr::nil(SourceOffset::default())
  }

  fn cons(a: SExpr, b: SExpr) -> SExpr {
    SExpr::new(SExprF::Cons(Box::new(a), Box::new(b)), SourceOffset::default())
  }

  #[test]
  fn test_runtime_repr_numerical() {
    assert_eq!(int(150).to_string(), 150.to_string());
    assert_eq!(int(-99).to_string(), (-99).to_string());
    assert_eq!(SExpr::new(SExprF::from(0.83), SourceOffset::default()).to_string(), (0.83).to_string());
    assert_eq!(SExpr::new(SExprF::from(-1.2), SourceOffset::default()).to_string(), (-1.2).to_string());
  }

  #[test]
  fn test_runtime_repr_nil() {
    assert_eq!(SExpr::new(SExprF::Atom(Literal::Nil), SourceOffset::default()).to_string(), "()");
  }

  #[test]
  fn test_runtime_repr_string() {
    assert_eq!(SExpr::string("abc", SourceOffset::default()).to_string(), r#""abc""#);
    assert_eq!(SExpr::string("abc\"d", SourceOffset::default()).to_string(), r#""abc\"d""#);
    assert_eq!(SExpr::string("\\foo\"bar\\", SourceOffset::default()).to_string(), r#""\\foo\"bar\\""#);
  }

  #[test]
  fn test_runtime_repr_symbol() {
    assert_eq!(SExpr::symbol("foo", SourceOffset::default()).to_string(), "foo");
    assert_eq!(SExpr::symbol("bar", SourceOffset::default()).to_string(), "bar");
  }

  #[test]
  fn test_runtime_repr_cons() {
    assert_eq!(cons(int(1), int(2)).to_string(), "(1 . 2)");
    assert_eq!(cons(int(1), cons(int(2), int(3))).to_string(), "(1 2 . 3)");
    assert_eq!(cons(int(1), cons(int(2), cons(int(3), nil()))).to_string(), "(1 2 3)");
  }

  #[test]
  fn test_runtime_repr_list() {
    assert_eq!(SExpr::dotted_list(vec!(int(1), int(2), int(3)), nil()).to_string(), "(1 2 3)");
    assert_eq!(SExpr::dotted_list(vec!(int(1), int(2), int(3)), int(4)).to_string(), "(1 2 3 . 4)");
    assert_eq!(SExpr::list(vec!(int(1), int(2), int(3), int(4)), SourceOffset::default()).to_string(), "(1 2 3 4)");
  }

  #[test]
  fn test_get_all_symbols() {
    assert_eq!(nil().all_symbols(), Vec::<&str>::new());
    assert_eq!(int(3).all_symbols(), Vec::<&str>::new());
    assert_eq!(SExpr::symbol("abc", SourceOffset::default()).all_symbols(), vec!("abc"));

    let foo = SExpr::symbol("foo", SourceOffset::default());
    let bar = SExpr::symbol("bar", SourceOffset::default());
    assert_eq!(cons(foo.clone(), bar.clone()).all_symbols(), vec!("foo", "bar"));
    assert_eq!(SExpr::dotted_list(vec!(foo.clone(), bar.clone()), nil()).all_symbols(), vec!("foo", "bar"));
  }

  #[test]
  fn test_each_source() {
    let example1 = SExpr::symbol("foo", SourceOffset(3));
    let example2 = SExpr::symbol("foo", SourceOffset(13));
    assert_eq!(example1.map_source(add10), example2);
  }

  fn add10(x: SourceOffset) -> SourceOffset {
    (usize::from(x) + 10).into()
  }

  #[test]
  fn test_depth_atomic() {
    assert_eq!(int(3).depth(), 1);
    assert_eq!(nil().depth(), 1);
    assert_eq!(SExpr::symbol("my-symbol", SourceOffset(0)).depth(), 1);
    assert_eq!(SExpr::string("my-string", SourceOffset(0)).depth(), 1);
    assert_eq!(SExpr::float(0.0, SourceOffset(0)).depth(), 1);
  }

  #[test]
  fn test_depth_nested() {
    assert_eq!(cons(int(1), int(2)).depth(), 2);
    assert_eq!(cons(int(1), cons(int(2), int(3))).depth(), 3);
    assert_eq!(cons(cons(int(1), int(2)), cons(int(3), int(4))).depth(), 3);
    assert_eq!(cons(cons(int(1), int(2)), int(3)).depth(), 3);
    assert_eq!(cons(cons(int(1), int(2)), cons(int(3), nil())).depth(), 3);
  }

  #[test]
  fn test_walk_preorder() {
    let mut values = Vec::<String>::new();
    let sexpr = cons(cons(int(1), int(2)), int(3));
    sexpr.walk_preorder(|sexpr| {
      values.push(sexpr.to_string());
      Ok::<(), Infallible>(())
    }).unwrap();
    assert_eq!(values, vec!["((1 . 2) . 3)", "(1 . 2)", "1", "2", "3"]);
  }

  #[test]
  fn test_walk_preorder_mut() {
    let mut values = Vec::<String>::new();
    let mut sexpr = cons(cons(int(1), int(2)), int(3));
    sexpr.walk_preorder_mut(|sexpr| {
      values.push(sexpr.to_string());
      Ok::<(), Infallible>(())
    }).unwrap();
    assert_eq!(values, vec!["((1 . 2) . 3)", "(1 . 2)", "1", "2", "3"]);
  }

  #[test]
  fn test_walk_postorder() {
    let mut values = Vec::<String>::new();
    let sexpr = cons(cons(int(1), int(2)), int(3));
    sexpr.walk_postorder(|sexpr| {
      values.push(sexpr.to_string());
      Ok::<(), Infallible>(())
    }).unwrap();
    assert_eq!(values, vec!["1", "2", "(1 . 2)", "3", "((1 . 2) . 3)"]);
  }

  #[test]
  fn test_walk_postorder_mut() {
    let mut values = Vec::<String>::new();
    let mut sexpr = cons(cons(int(1), int(2)), int(3));
    sexpr.walk_postorder_mut(|sexpr| {
      values.push(sexpr.to_string());
      Ok::<(), Infallible>(())
    }).unwrap();
    assert_eq!(values, vec!["1", "2", "(1 . 2)", "3", "((1 . 2) . 3)"]);
  }

  #[test]
  fn test_as_symbol_ref() {
    assert!(SExpr::string("foo", SourceOffset(0)).as_symbol_ref().is_none());
    assert_eq!(SExpr::symbol("xxx", SourceOffset(0)).as_symbol_ref(), Some("xxx"));
  }

}
