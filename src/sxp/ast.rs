
//! Defines the basic [`AST`] type.

use ordered_float::OrderedFloat;

use std::fmt;
use std::convert::Infallible;

/// The basic type used for representing Lisp S-expressions.
#[derive(PartialEq, Eq, Hash, Clone)]
pub enum AST {
  /// A nil value, or `()`. This is comparable to `null` in some
  /// languages but also functions as the empty list.
  Nil,
  /// A pair of values. The first value is referred to as the car and
  /// the second as the cdr. All Lisp lists are made up of cons cells
  /// and [`AST::Nil`]. Displays as `(car . cdr)`.
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

fn fmt_list(a: &AST, b: &AST, f: &mut fmt::Formatter<'_>) -> fmt::Result {
  match b {
    AST::Nil =>
      // End of list; just print the known value
      write!(f, "{}", a),
    AST::Cons(b1, c1) => {
      // Another cons cell in cdr; continue printing list
      write!(f, "{} ", a)?;
      fmt_list(b1, c1, f)
    },
    _ =>
      // Dotted list; print with dot
      write!(f, "{} . {}", a, b)
  }
}

impl AST {

  /// An [`AST::Cons`] cell. This is more convenient than calling the
  /// constructor directly, as you needn't explicitly box the values.
  pub fn cons(car: AST, cdr: AST) -> AST {
    AST::Cons(Box::new(car), Box::new(cdr))
  }

  /// An [`AST::String`]. Copies the string argument into a new
  /// [`AST`] value.
  pub fn string(s: &str) -> AST {
    AST::String(s.to_owned())
  }

  /// An [`AST::Symbol`]. Copies the string argument into a new
  /// [`AST`] value.
  pub fn symbol(s: &str) -> AST {
    AST::Symbol(s.to_string())
  }

  /// In Lisp, we generally think of a *dotted list* as a sequence of
  /// zero or more cons cells, where the cdr of each cell is the next
  /// cons cell, eventually terminated by some non-cons value. For
  /// instance, `(1 . (2 . (3 . 4)))` would be a dotted list where the
  /// values in the "list" portion are `1`, `2`, and `3`, and the
  /// terminator is `4`.
  ///
  /// We call a dotted list which terminates in `()` (i.e.
  /// [`AST::Nil`]) a *proper list*. Some sources explicitly define a
  /// dotted list to *not* be a proper list, but this documentation
  /// does not make that distinction.
  ///
  /// This function constructs an [`AST`] value from a sequence of
  /// values `vec` and a terminator `terminal`. For each value in the
  /// sequence, a [`AST::Cons`] cell will be constructed, and the
  /// final cdr will be `terminal`.
  ///
  /// For the inverse operation of converted an [`AST`] *back* into a
  /// sequence and terminator, see [`super::dotted::DottedExpr`].
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::sxp::ast::AST;
  /// let value = AST::dotted_list(vec!(AST::Int(1), AST::Int(2)), AST::Int(3));
  /// assert_eq!(value, AST::cons(AST::Int(1), AST::cons(AST::Int(2), AST::Int(3))));
  /// ```
  pub fn dotted_list(vec: Vec<AST>, terminal: AST) -> AST {
    vec.into_iter().rev().fold(terminal, |cdr, car| AST::cons(car, cdr)) // NOTE: Arguments reversed
  }

  /// A dotted list terminated by [`AST::Nil`].
  ///
  /// Equivalent to:
  /// ```rust,ignore
  /// AST::dotted_list(vec, AST::Nil)
  /// ```
  pub fn list(vec: Vec<AST>) -> AST {
    AST::dotted_list(vec, AST::Nil)
  }

  fn _recurse<'a, 'b, F1, F2, E>(&'a self, func: &mut F1, default: &mut F2) -> Result<(), E>
  where F1 : FnMut(&'b AST) -> Result<(), E>,
        F2 : FnMut() -> Result<(), E>,
        'a : 'b {
    match self {
      AST::Cons(car, cdr) => {
        func(&*car)?;
        func(&*cdr)?;
      }
      AST::Array(arr) => {
        for x in arr {
          func(&*x)?;
        }
      }
      AST::Dictionary(d) => {
        for (k, v) in d {
          func(&*k)?;
          func(&*v)?;
        }
      }
      AST::Nil | AST::Int(_) | AST::Bool(_) | AST::Float(_) | AST::String(_) | AST::Symbol(_) => {
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

  fn _walk_postorder<'a, 'b, F, E>(&'a self, func: &mut F) -> Result<(), E>
  where F: FnMut(&'b AST) -> Result<(), E>,
        'a: 'b {
    self._recurse(&mut |x| x._walk_postorder(func), &mut || Ok(()))?;
    func(self)
  }

  pub fn walk_preorder<'a, 'b, F, E>(&'a self, mut func: F) -> Result<(), E>
  where F: FnMut(&'b AST) -> Result<(), E>,
        'a: 'b {
    self._walk_preorder(&mut func)
  }

  pub fn walk_postorder<'a, 'b, F, E>(&'a self, mut func: F) -> Result<(), E>
  where F: FnMut(&'b AST) -> Result<(), E>,
        'a: 'b {
    self._walk_postorder(&mut func)
  }

  fn extract_err<T>(res: Result<T, Infallible>) -> T {
    match res {
      Ok(x) => x,
      Err(contra) => match contra {}
    }
  }

  pub fn all_symbols<'a>(&'a self) -> Vec<&'a str> {
    let mut result: Vec<&'a str> = Vec::new();
    let err = self.walk_preorder::<_, Infallible>(|x| {
      if let AST::Symbol(x) = x {
        result.push(&x);
      }
      Ok(())
    });
    let () = AST::extract_err(err);
    result
  }

}

impl fmt::Display for AST {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      AST::Nil => write!(f, "()"),
      AST::Int(n) => write!(f, "{}", n),
      AST::Bool(true) => write!(f, "#t"),
      AST::Bool(false) => write!(f, "#f"),
      AST::Float(x) => write!(f, "{}", x),
      AST::String(s) => write!(f, "{:?}", s), // TODO Proper string escaping here
      AST::Symbol(s) => write!(f, "{}", s), // TODO Proper escaping here too
      AST::Cons(a, b) => {
        write!(f, "(")?;
        fmt_list(a, b, f)?;
        write!(f, ")")
      }
      AST::Array(vec) => {
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
      AST::Dictionary(vec) => {
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

impl fmt::Debug for AST {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // The fmt::Display for AST is already pretty unambiguous and is
    // probably more readable than the derived struct printout.
    fmt::Display::fmt(self, f)
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use std::string::ToString;

  #[test]
  fn runtime_repr_numerical() {
    assert_eq!(AST::Int(150).to_string(), 150.to_string());
    assert_eq!(AST::Int(-99).to_string(), (-99).to_string());
    assert_eq!(AST::Float((0.83).into()).to_string(), (0.83).to_string());
    assert_eq!(AST::Float((-1.2).into()).to_string(), (-1.2).to_string());
  }

  #[test]
  fn runtime_repr_nil() {
    assert_eq!(AST::Nil.to_string(), "()");
  }

  #[test]
  fn runtime_repr_string() {
    assert_eq!(AST::string("abc").to_string(), r#""abc""#);
    assert_eq!(AST::string("abc\"d").to_string(), r#""abc\"d""#);
    assert_eq!(AST::string("\\foo\"bar\\").to_string(), r#""\\foo\"bar\\""#);
  }

  #[test]
  fn runtime_repr_symbol() {
    assert_eq!(AST::symbol("foo").to_string(), "foo");
    assert_eq!(AST::symbol("bar").to_string(), "bar");
  }

  #[test]
  fn runtime_repr_cons() {
    assert_eq!(AST::cons(AST::Int(1), AST::Int(2)).to_string(), "(1 . 2)");
    assert_eq!(AST::cons(AST::Int(1), AST::cons(AST::Int(2), AST::Int(3))).to_string(), "(1 2 . 3)");
    assert_eq!(AST::cons(AST::Int(1), AST::cons(AST::Int(2), AST::cons(AST::Int(3), AST::Nil))).to_string(), "(1 2 3)");
  }

  #[test]
  fn runtime_repr_list() {
    assert_eq!(AST::list(vec!(AST::Int(1), AST::Int(2), AST::Int(3))).to_string(), "(1 2 3)");
    assert_eq!(AST::dotted_list(vec!(AST::Int(1), AST::Int(2), AST::Int(3)), AST::Int(4)).to_string(), "(1 2 3 . 4)");
  }

  #[test]
  fn runtime_repr_vec() {
    assert_eq!(AST::Array(vec!()).to_string(), "[]");
    assert_eq!(AST::Array(vec!(AST::Int(1), AST::Int(2), AST::Int(3))).to_string(), "[1 2 3]");
  }

  #[test]
  fn get_all_symbols() {
    assert_eq!(AST::Nil.all_symbols(), Vec::<&str>::new());
    assert_eq!(AST::Int(3).all_symbols(), Vec::<&str>::new());
    assert_eq!(AST::Symbol(String::from("abc")).all_symbols(), vec!("abc"));

    let foo = AST::Symbol(String::from("foo"));
    let bar = AST::Symbol(String::from("bar"));
    assert_eq!(AST::cons(foo.clone(), bar.clone()).all_symbols(), vec!("foo", "bar"));
    assert_eq!(AST::list(vec!(foo.clone(), bar.clone())).all_symbols(), vec!("foo", "bar"));
  }

}
