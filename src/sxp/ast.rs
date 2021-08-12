
//! Defines the basic [`AST`] type.

use crate::pipeline::source::{SourceOffset, Sourced};
use crate::util::extract_err;

use ordered_float::OrderedFloat;

use std::fmt;
use std::convert::Infallible;

/// The basic type used for representing Lisp S-expressions.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum ASTF {
  /// A nil value, or `()`. This is comparable to `null` in some
  /// languages but also functions as the empty list.
  Nil,
  /// A pair of values. The first value is referred to as the car and
  /// the second as the cdr. All Lisp lists are made up of cons cells
  /// and [`ASTF::Nil`]. Displays as `(car . cdr)`.
  Cons(Box<AST>, Box<AST>),
  /// A literal array of values. Whereas a list in Lisp is a linked
  /// list made of cons cells, an array is a constant-time sequential
  /// chunk of memory. Displays as `[x0 x1 ... xn]`
  Array(Vec<AST>),
  /// A constant-time array of pairs. Note that, while this structure
  /// *compiles* to a dictionary in GDScript, the [`AST`] structure
  /// itself is *not* an associative container. It can contain
  /// duplicate keys and preserves the order in which the keys are
  /// entered. Displays as `{k1 v1 k2 v2 ... kn vn}`
  Dictionary(Vec<(AST, AST)>),
  /// A literal 32-bit integer value.
  Int(i32),
  /// A literal Boolean value.
  Bool(bool),
  /// A literal floating-point value. For AST purposes, we do not use
  /// standard IEEE comparison semantics and instead use
  /// [`OrderedFloat`], whose ordering and equality relations satisfy
  /// convenient abstract mathematical properties.
  Float(OrderedFloat<f32>),
  /// A literal string.
  String(String),
  /// A literal symbol.
  Symbol(String),
}

/// An `AST` is an [`ASTF`] together with information about the offset
/// in the source code of the S-expression.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct AST {
  pub value: ASTF,
  pub pos: SourceOffset,
}

fn fmt_list(a: &AST, b: &AST, f: &mut fmt::Formatter<'_>) -> fmt::Result {
  match &b.value {
    ASTF::Nil =>
      // End of list; just print the known value
      write!(f, "{}", a),
    ASTF::Cons(b1, c1) => {
      // Another cons cell in cdr; continue printing list
      write!(f, "{} ", a)?;
      fmt_list(&b1, &c1, f)
    },
    _ =>
      // Dotted list; print with dot
      write!(f, "{} . {}", a, b)
  }
}

impl ASTF {

  /// An [`ASTF::Cons`] cell. This is more convenient than calling the
  /// constructor directly, as you needn't explicitly box the values.
  pub fn cons(car: AST, cdr: AST) -> ASTF {
    ASTF::Cons(Box::new(car), Box::new(cdr))
  }

  /// An [`ASTF::String`]. Copies the string argument into a new
  /// [`ASTF`] value.
  pub fn string(s: &str) -> ASTF {
    ASTF::String(s.to_owned())
  }

  /// An [`ASTF::Symbol`]. Copies the string argument into a new
  /// [`ASTF`] value.
  pub fn symbol(s: &str) -> ASTF {
    ASTF::Symbol(s.to_string())
  }

}

impl AST {

  /// A new `AST` with the given value and position. Equivalent to
  /// `AST { value, pos }`.
  pub fn new(value: ASTF, pos: SourceOffset) -> AST {
    AST { value, pos }
  }

  /// An [`ASTF::Nil`] wrapped in `AST` with the given source offset.
  /// Equivalent to `AST::new(ASTF::Nil, pos)`.
  pub fn nil(pos: SourceOffset) -> AST {
    AST::new(ASTF::Nil, pos)
  }

  /// An [`ASTF::Symbol`] with the given value.
  pub fn symbol(name: &str, pos: SourceOffset) -> AST {
    AST::new(ASTF::symbol(name), pos)
  }

  /// An [`ASTF::String`] with the given value.
  pub fn string(name: &str, pos: SourceOffset) -> AST {
    AST::new(ASTF::string(name), pos)
  }

  /// An [`ASTF::Int`] with the given value.
  pub fn int(value: i32, pos: SourceOffset) -> AST {
    AST::new(ASTF::Int(value), pos)
  }

  /// An [`ASTF::Float`] with the given value.
  pub fn float(value: f32, pos: SourceOffset) -> AST {
    AST::new(ASTF::Float(value.into()), pos)
  }

  /// An [`ASTF::Cons`] with the given value.
  pub fn cons(car: AST, cdr: AST, pos: SourceOffset) -> AST {
    AST::new(ASTF::cons(car, cdr), pos)
  }

  fn _recurse<'a, 'b, F1, F2, E>(&'a self, func: &mut F1, default: &mut F2) -> Result<(), E>
  where F1 : FnMut(&'b AST) -> Result<(), E>,
        F2 : FnMut() -> Result<(), E>,
        'a : 'b {
    match &self.value {
      ASTF::Cons(car, cdr) => {
        func(&*car)?;
        func(&*cdr)?;
      }
      ASTF::Array(arr) => {
        for x in arr {
          func(&x)?;
        }
      }
      ASTF::Dictionary(d) => {
        for (k, v) in d {
          func(&k)?;
          func(&v)?;
        }
      }
      ASTF::Nil | ASTF::Int(_) | ASTF::Bool(_) | ASTF::Float(_) | ASTF::String(_) | ASTF::Symbol(_) => {
        default()?;
      }
    }
    Ok(())
  }

  fn _recurse_mut<'a, 'b, F1, F2, E>(&'a mut self, func: &mut F1, default: &mut F2) -> Result<(), E>
  where F1 : FnMut(&'b mut AST) -> Result<(), E>,
        F2 : FnMut() -> Result<(), E>,
        'a : 'b {
    match &mut self.value {
      ASTF::Cons(car, cdr) => {
        func(&mut *car)?;
        func(&mut *cdr)?;
      }
      ASTF::Array(arr) => {
        for x in arr {
          func(x)?;
        }
      }
      ASTF::Dictionary(d) => {
        for (k, v) in d {
          func(k)?;
          func(v)?;
        }
      }
      ASTF::Nil | ASTF::Int(_) | ASTF::Bool(_) | ASTF::Float(_) | ASTF::String(_) | ASTF::Symbol(_) => {
        default()?;
      }
    }
    Ok(())
  }

  fn _walk_preorder<'a, 'b, F, E>(&'a self, func: &mut F) -> Result<(), E>
  where F: FnMut(&'b AST) -> Result<(), E>,
        'a: 'b {
    func(self)?;
    self._recurse(&mut |x| x._walk_preorder(func), &mut || Ok(()))
  }

  fn _walk_preorder_mut<'a, F, E>(&'a mut self, func: &mut F) -> Result<(), E>
  where F: for<'b> FnMut(&'b mut AST) -> Result<(), E> {
    func(self)?;
    self._recurse_mut(&mut |x| x._walk_preorder_mut(func), &mut || Ok(()))
  }

  fn _walk_postorder<'a, 'b, F, E>(&'a self, func: &mut F) -> Result<(), E>
  where F: FnMut(&'b AST) -> Result<(), E>,
        'a: 'b {
    self._recurse(&mut |x| x._walk_postorder(func), &mut || Ok(()))?;
    func(self)
  }

  fn _walk_postorder_mut<'a, F, E>(&'a mut self, func: &mut F) -> Result<(), E>
  where F: for<'b> FnMut(&'b mut AST) -> Result<(), E> {
    self._recurse_mut(&mut |x| x._walk_postorder_mut(func), &mut || Ok(()))?;
    func(self)
  }

  /// Walk the `AST`, calling a function on the node itself and every
  /// child recursively. That includes both elements of an
  /// [`ASTF::Cons`], all elements of an [`ASTF::Array`], and any
  /// other children of nodes. The function will be called on the
  /// current node *before* recursing on its children.
  ///
  /// Any error that occurs during walking will be propagated to the
  /// caller.
  pub fn walk_preorder<'a, 'b, F, E>(&'a self, mut func: F) -> Result<(), E>
  where F: FnMut(&'b AST) -> Result<(), E>,
        'a: 'b {
    self._walk_preorder(&mut func)
  }

  /// As [`AST::walk_preorder`], but with a mutable `self`.
  pub fn walk_preorder_mut<'a, F, E>(&'a mut self, mut func: F) -> Result<(), E>
  where F: for<'b> FnMut(&'b mut AST) -> Result<(), E> {
    self._walk_preorder_mut(&mut func)
  }

  /// Walk the `AST`, calling a function on the node itself and every
  /// child recursively. That includes both elements of an
  /// [`ASTF::Cons`], all elements of an [`ASTF::Array`], and any
  /// other children of nodes. The function will be called on the
  /// current node only *after* recursing on its children.
  ///
  /// Any error that occurs during walking will be propagated to the
  /// caller.
  pub fn walk_postorder<'a, 'b, F, E>(&'a self, mut func: F) -> Result<(), E>
  where F: FnMut(&'b AST) -> Result<(), E>,
        'a: 'b {
    self._walk_postorder(&mut func)
  }

  /// As [`AST::walk_postorder`], but with a mutable `self`.
  pub fn walk_postorder_mut<'a, F, E>(&'a mut self, mut func: F) -> Result<(), E>
  where F: for<'b> FnMut(&'b mut AST) -> Result<(), E> {
    self._walk_postorder_mut(&mut func)
  }

  /// Walk the `AST`, transforming all of the [`SourceOffset`] tags
  /// using the given function. The walk is performed using
  /// [`AST::walk_preorder_mut`].
  pub fn each_source_mut<F>(&mut self, mut func: F)
  where F: FnMut(SourceOffset) -> SourceOffset {
    let result = self.walk_preorder_mut(|ast| {
      ast.pos = func(ast.pos);
      Ok(())
    });
    extract_err(result)
  }

  /// As [`AST::each_source_mut`], but returns a new object.
  pub fn each_source<F>(&self, func: F) -> AST
  where F: FnMut(SourceOffset) -> SourceOffset {
    let mut ast = self.clone();
    ast.each_source_mut(func);
    ast
  }

  /// Walk the `AST`, producing a list of all symbols that appear (as
  /// [`ASTF::Symbol`]) anywhere in the tree. The symbols will appear
  /// in the resulting list in the order they appear in the `AST`, and
  /// any duplicates will be represented multiple times, once for each
  /// appearance.
  pub fn all_symbols<'a>(&'a self) -> Vec<&'a str> {
    let mut result: Vec<&'a str> = Vec::new();
    let err = self.walk_preorder::<_, Infallible>(|x| {
      if let ASTF::Symbol(x) = &x.value {
        result.push(&x);
      }
      Ok(())
    });
    let () = extract_err(err);
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
  /// [`ASTF::Nil`]) a *proper list*. Some sources explicitly define a
  /// dotted list to *not* be a proper list, but this documentation
  /// does not make that distinction.
  ///
  /// This function constructs an [`ASTF`] value from a sequence of
  /// values `vec` and a terminator `terminal`. For each value in the
  /// sequence, a [`ASTF::Cons`] cell will be constructed, and the
  /// final cdr will be `terminal`.
  ///
  /// For the inverse operation of converted an [`ASTF`] *back* into a
  /// sequence and terminator, see [`super::dotted::DottedExpr`].
  pub fn dotted_list(vec: Vec<AST>, terminal: AST) -> AST {
    vec.into_iter().rev().fold(terminal, AST::dotted_list_fold) // NOTE: Arguments reversed
  }

  fn dotted_list_fold(cdr: AST, car: AST) -> AST { // NOTE: Arguments reversed from the usual order
    let pos = car.pos;
    AST::new(ASTF::cons(car, cdr), pos)
  }

  /// A dotted list terminated by nil at the given source position.
  pub fn list(vec: Vec<AST>, nil_pos: SourceOffset) -> AST {
    AST::dotted_list(vec, AST::nil(nil_pos))
  }

  /// An [`ASTF::Array`] at the given position.
  pub fn array(vec: Vec<AST>, pos: SourceOffset) -> AST {
    AST::new(ASTF::Array(vec), pos)
  }

  /// An [`ASTF::Dictionary`] at the given position.
  pub fn dictionary(vec: Vec<(AST, AST)>, pos: SourceOffset) -> AST {
    AST::new(ASTF::Dictionary(vec), pos)
  }

}

/// Pretty-print an AST, using a format compatible with [`parser`](crate::parser).
/// Cons cells whose cdr is a cons cell will be pretty-printed as list
/// prefixes.
impl fmt::Display for AST {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.value {
      ASTF::Nil => write!(f, "()"),
      ASTF::Int(n) => write!(f, "{}", n),
      ASTF::Bool(true) => write!(f, "#t"),
      ASTF::Bool(false) => write!(f, "#f"),
      ASTF::Float(x) => write!(f, "{}", x),
      ASTF::String(s) => write!(f, "{:?}", s), // TODO Proper string escaping here
      ASTF::Symbol(s) => write!(f, "{}", s), // TODO Proper escaping here too
      ASTF::Cons(a, b) => {
        write!(f, "(")?;
        fmt_list(&*a, &*b, f)?;
        write!(f, ")")
      }
      ASTF::Array(vec) => {
        write!(f, "[")?;
        let mut first = true;
        for x in vec {
          if !first {
            write!(f, " ")?;
          }
          write!(f, "{}", x)?;
          first = false;
        }
        write!(f, "]")
      }
      ASTF::Dictionary(vec) => {
        write!(f, "{{")?;
        let mut first = true;
        for (k, v) in vec {
          if !first {
            write!(f, " ")?;
          }
          write!(f, "{} {}", k, v)?;
          first = false;
        }
        write!(f, "}}")
      }
    }
  }

}

impl Sourced for AST {
  type Item = ASTF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &ASTF {
    &self.value
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use std::string::ToString;

  // A handful of helpers for the tests that don't care about
  // SourceOffset and are only testing the structure. These just fill
  // in SourceOffset::default() wherever necessary.

  fn int(n: i32) -> AST {
    AST::new(ASTF::Int(n), SourceOffset::default())
  }

  fn nil() -> AST {
    AST::nil(SourceOffset::default())
  }

  fn cons(a: AST, b: AST) -> AST {
    AST::new(ASTF::cons(a, b), SourceOffset::default())
  }

  #[test]
  fn runtime_repr_numerical() {
    assert_eq!(int(150).to_string(), 150.to_string());
    assert_eq!(int(-99).to_string(), (-99).to_string());
    assert_eq!(AST::new(ASTF::Float((0.83).into()), SourceOffset::default()).to_string(), (0.83).to_string());
    assert_eq!(AST::new(ASTF::Float((-1.2).into()), SourceOffset::default()).to_string(), (-1.2).to_string());
  }

  #[test]
  fn runtime_repr_nil() {
    assert_eq!(AST::new(ASTF::Nil, SourceOffset::default()).to_string(), "()");
  }

  #[test]
  fn runtime_repr_string() {
    assert_eq!(AST::string("abc", SourceOffset::default()).to_string(), r#""abc""#);
    assert_eq!(AST::string("abc\"d", SourceOffset::default()).to_string(), r#""abc\"d""#);
    assert_eq!(AST::string("\\foo\"bar\\", SourceOffset::default()).to_string(), r#""\\foo\"bar\\""#);
  }

  #[test]
  fn runtime_repr_symbol() {
    assert_eq!(AST::symbol("foo", SourceOffset::default()).to_string(), "foo");
    assert_eq!(AST::symbol("bar", SourceOffset::default()).to_string(), "bar");
  }

  #[test]
  fn runtime_repr_cons() {
    assert_eq!(cons(int(1), int(2)).to_string(), "(1 . 2)");
    assert_eq!(cons(int(1), cons(int(2), int(3))).to_string(), "(1 2 . 3)");
    assert_eq!(cons(int(1), cons(int(2), cons(int(3), nil()))).to_string(), "(1 2 3)");
  }

  #[test]
  fn runtime_repr_list() {
    assert_eq!(AST::dotted_list(vec!(int(1), int(2), int(3)), nil()).to_string(), "(1 2 3)");
    assert_eq!(AST::dotted_list(vec!(int(1), int(2), int(3)), int(4)).to_string(), "(1 2 3 . 4)");
  }

  #[test]
  fn runtime_repr_vec() {
    assert_eq!(AST::new(ASTF::Array(vec!()), SourceOffset::default()).to_string(), "[]");
    assert_eq!(AST::new(ASTF::Array(vec!(int(1), int(2), int(3))), SourceOffset::default()).to_string(), "[1 2 3]");
  }

  #[test]
  fn get_all_symbols() {
    assert_eq!(nil().all_symbols(), Vec::<&str>::new());
    assert_eq!(int(3).all_symbols(), Vec::<&str>::new());
    assert_eq!(AST::symbol("abc", SourceOffset::default()).all_symbols(), vec!("abc"));

    let foo = AST::symbol("foo", SourceOffset::default());
    let bar = AST::symbol("bar", SourceOffset::default());
    assert_eq!(cons(foo.clone(), bar.clone()).all_symbols(), vec!("foo", "bar"));
    assert_eq!(AST::dotted_list(vec!(foo.clone(), bar.clone()), nil()).all_symbols(), vec!("foo", "bar"));
  }

  #[test]
  fn each_source() {
    let example1_1 = AST::symbol("foo", SourceOffset(3));
    let example1_2 = AST::symbol("foo", SourceOffset(13));
    assert_eq!(example1_1.each_source(add10), example1_2);

    let example2_1 = AST::new(ASTF::Array(vec!(AST::symbol("foo", SourceOffset(3)), AST::symbol("foo", SourceOffset(4)))), SourceOffset(9));
    let example2_2 = AST::new(ASTF::Array(vec!(AST::symbol("foo", SourceOffset(13)), AST::symbol("foo", SourceOffset(14)))), SourceOffset(19));
    assert_eq!(example2_1.each_source(add10), example2_2);
  }

  fn add10(x: SourceOffset) -> SourceOffset {
    (usize::from(x) + 10).into()
  }

}
