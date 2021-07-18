
//! GDScript patterns for use in pattern matching.
//!
//! This module defines a [datatype](Pattern) for representing
//! patterns in the GDScript language, as well as [`Pattern::to_gd`]
//! for converting to GDScript syntax.

use crate::gdscript::literal::Literal;

/// The type of GDScript patterns.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
  Literal(Literal),
  Var(String),
  Wildcard,
  BindingVar(String),
  Array(Vec<Pattern>, Wildcard),
  Dictionary(Vec<(Literal, Pattern)>, Wildcard),
}

/// This type is isomorphic to [`bool`] and indicates whether or not a
/// wildcard was supplied to a pattern.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub enum Wildcard {
  NoWildcard, Wildcard,
}

impl Pattern {

  /// Convert a GDScript pattern to a string. The result will contain
  /// valid GDScript syntax.
  pub fn to_gd(&self) -> String {
    match self {
      Pattern::Literal(lit) => lit.to_gd(),
      Pattern::Var(s) => s.clone(),
      Pattern::Wildcard => String::from("_"), // TODO Make sure to handle the case of a variable called _, as that's technically weirdly ambiguous here.
      Pattern::BindingVar(s) => format!("var {}", s),
      Pattern::Array(ptns, wild) => {
        let mut inner = ptns.iter().map(|ptn| ptn.to_gd()).collect::<Vec<_>>();
        if *wild == Wildcard::Wildcard {
          inner.push(String::from(".."));
        }
        format!("[{}]", inner.join(", "))
      },
      Pattern::Dictionary(d, wild) => {
        let mut inner = d.iter().map(|x| format!("{}: {}", x.0.to_gd(), x.1.to_gd())).collect::<Vec<_>>();
        if *wild == Wildcard::Wildcard {
          inner.push(String::from(".."));
        }
        format!("{{{}}}", inner.join(", "))
      },
    }
  }

}

impl From<Wildcard> for bool {
  fn from(w: Wildcard) -> bool {
    w == Wildcard::Wildcard
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn atomic_patterns() {
    assert_eq!(Pattern::Literal(Literal::Int(3)).to_gd(), "3");
    assert_eq!(Pattern::Var(String::from("var_name")).to_gd(), "var_name");
    assert_eq!(Pattern::BindingVar(String::from("var_name")).to_gd(), "var var_name");
    assert_eq!(Pattern::Wildcard.to_gd(), "_");
  }

  #[test]
  fn compound_patterns() {
    let lit1 = Literal::Int(100);
    let lit2 = Literal::Int(200);

    let ptn1 = Pattern::Literal(Literal::Int(1));
    let ptn2 = Pattern::Literal(Literal::Int(2));

    assert_eq!(Pattern::Array(vec!(), Wildcard::NoWildcard).to_gd(), "[]");
    assert_eq!(Pattern::Array(vec!(), Wildcard::Wildcard).to_gd(), "[..]");
    assert_eq!(Pattern::Array(vec!(ptn1.clone()), Wildcard::NoWildcard).to_gd(), "[1]");
    assert_eq!(Pattern::Array(vec!(ptn1.clone()), Wildcard::Wildcard).to_gd(), "[1, ..]");
    assert_eq!(Pattern::Array(vec!(ptn1.clone(), ptn2.clone()), Wildcard::NoWildcard).to_gd(), "[1, 2]");
    assert_eq!(Pattern::Array(vec!(ptn1.clone(), ptn2.clone()), Wildcard::Wildcard).to_gd(), "[1, 2, ..]");

    assert_eq!(Pattern::Dictionary(vec!(), Wildcard::NoWildcard).to_gd(), "{}");
    assert_eq!(Pattern::Dictionary(vec!(), Wildcard::Wildcard).to_gd(), "{..}");
    assert_eq!(Pattern::Dictionary(vec!((lit1.clone(), ptn1.clone())), Wildcard::NoWildcard).to_gd(), "{100: 1}");
    assert_eq!(Pattern::Dictionary(vec!((lit1.clone(), ptn1.clone())), Wildcard::Wildcard).to_gd(), "{100: 1, ..}");
    assert_eq!(Pattern::Dictionary(vec!((lit1.clone(), ptn1.clone()), (lit2.clone(), ptn2.clone())), Wildcard::NoWildcard).to_gd(), "{100: 1, 200: 2}");
    assert_eq!(Pattern::Dictionary(vec!((lit1.clone(), ptn1.clone()), (lit2.clone(), ptn2.clone())), Wildcard::Wildcard).to_gd(), "{100: 1, 200: 2, ..}");

  }

}
