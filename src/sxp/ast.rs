
use std::fmt;

// TODO Implement a sensible Eq that bypasses the floating-point (==) shenanigans.
#[derive(PartialEq)]
pub enum AST {
  Nil,
  Cons(Box<AST>, Box<AST>),
  Int(i64),
  Float(f64),
  String(String),
  Symbol(String),
}

pub fn cons(car: AST, cdr: AST) -> AST {
  AST::Cons(Box::new(car), Box::new(cdr))
}

pub fn string(s: &str) -> AST {
  AST::String(s.to_string())
}

pub fn symbol(s: &str) -> AST {
  AST::Symbol(s.to_string())
}

pub fn dotted_list(vec: Vec<AST>, terminal: AST) -> AST {
  vec.into_iter().rev().fold(terminal, |cdr, car| cons(car, cdr))
}

pub fn list(vec: Vec<AST>) -> AST {
  dotted_list(vec, AST::Nil)
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

impl fmt::Display for AST {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      AST::Nil => write!(f, "()"),
      AST::Int(n) => write!(f, "{}", n),
      AST::Float(x) => write!(f, "{}", x),
      AST::String(s) => write!(f, "{:?}", s), // TODO Proper string escaping here
      AST::Symbol(s) => write!(f, "{}", s), // TODO Proper escaping here too
      AST::Cons(a, b) => {
        write!(f, "(")?;
        fmt_list(a, b, f)?;
        write!(f, ")")
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
    assert_eq!(AST::Float(0.83).to_string(), (0.83).to_string());
    assert_eq!(AST::Float(-1.2).to_string(), (-1.2).to_string());
  }

  #[test]
  fn runtime_repr_nil() {
    assert_eq!(AST::Nil.to_string(), "()");
  }

  #[test]
  fn runtime_repr_string() {
    assert_eq!(string("abc").to_string(), r#""abc""#);
    assert_eq!(string("abc\"d").to_string(), r#""abc\"d""#);
    assert_eq!(string("\\foo\"bar\\").to_string(), r#""\\foo\"bar\\""#);
  }

  #[test]
  fn runtime_repr_symbol() {
    assert_eq!(symbol("foo").to_string(), "foo");
    assert_eq!(symbol("bar").to_string(), "bar");
  }

  #[test]
  fn runtime_repr_cons() {
    assert_eq!(cons(AST::Int(1), AST::Int(2)).to_string(), "(1 . 2)");
    assert_eq!(cons(AST::Int(1), cons(AST::Int(2), AST::Int(3))).to_string(), "(1 2 . 3)");
    assert_eq!(cons(AST::Int(1), cons(AST::Int(2), cons(AST::Int(3), AST::Nil))).to_string(), "(1 2 3)");
  }

  #[test]
  fn runtime_repr_list() {
    assert_eq!(list(vec!(AST::Int(1), AST::Int(2), AST::Int(3))).to_string(), "(1 2 3)");
    assert_eq!(dotted_list(vec!(AST::Int(1), AST::Int(2), AST::Int(3)), AST::Int(4)).to_string(), "(1 2 3 . 4)");
  }

}
