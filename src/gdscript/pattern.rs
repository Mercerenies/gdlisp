
use crate::gdscript::literal::Literal;

#[derive(Debug, Clone)]
pub enum Pattern {
  Literal(Literal),
  Var(String),
  Wildcard,
  BindingVar(String),
  Array(Vec<Pattern>, Wildcard),
  Dictionary(Vec<(Literal, Pattern)>, Wildcard),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub enum Wildcard {
  NoWildcard, Wildcard,
}

impl Pattern {

  pub fn to_gd(&self) -> String {
    match self {
      Pattern::Literal(lit) => lit.to_gd(),
      Pattern::Var(s) => s.clone(),
      Pattern::Wildcard => String::from("_"), // TODO Make sure to handle the case of a variable called _, as that's technically weirdly ambiguous here.
      Pattern::BindingVar(s) => format!("var {}", s),
      Pattern::Array(ptns, wild) => {
        let mut result = String::new();
        result.push_str("[");
        let mut first = true;
        for ptn in ptns {
          if !first {
            result.push_str(", ");
          }
          first = false;
          result.push_str(&ptn.to_gd());
        }
        if *wild == Wildcard::Wildcard {
          if !first {
            result.push_str(", ");
          }
          result.push_str("..");
        }
        result.push_str("]");
        result
      },
      Pattern::Dictionary(d, wild) => {
        let mut result = String::new();
        result.push_str("{");
        let mut first = true;
        for (lit, ptn) in d {
          if !first {
            result.push_str(", ");
          }
          first = false;
          result.push_str(&format!("{}: {}", lit.to_gd(), ptn.to_gd()));
        }
        if *wild == Wildcard::Wildcard {
          if !first {
            result.push_str(", ");
          }
          result.push_str("..");
        }
        result.push_str("}");
        result
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
