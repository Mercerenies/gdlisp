
//! Convenience functions for checking the type of thing we're looking
//! at, for instance whether a given [`AST`] is a declaration or an
//! expression.

use crate::sxp::ast::{AST, ASTF};
use crate::sxp::dotted::DottedExpr;
use crate::pipeline::source::SourceOffset;

use std::convert::TryInto;
use std::borrow::Borrow;

/// Here, we list the heads for all valid declaration types. Note that
/// `progn` is specifically not included here; `progn` is an
/// expression-level special form which is also a deeply magical
/// construct treated in a special way by the compiler during parsing.
/// It is *not* a declaration, even though it can look like one
/// syntactically.
pub const DECL_HEADS: [&'static str; 9] = [
  "defn", "defmacro", "defconst", "defclass", "defenum", "sys/declare",
  "define-symbol-macro", "sys/bootstrap", "sys/min-godot-version",
];

/// A simple check to see whether a given AST should be parsed as a
/// declaration or not.
///
/// There are many contexts (including the very top-level of a file)
/// where either a declaration or an expression is acceptable. This
/// function is used to determine whether the AST should be parsed as
/// a declaration or an expression. Note that a `true` result for
/// `is_decl` is *not* a guarantee that parsing as a declaration will
/// be error-free; it is merely an indication that the declaration
/// parse should be attempted.
///
/// If `decl` is not a proper list (as per the definition in
/// [`DottedExpr`](crate::sxp::dotted::DottedExpr)), then `is_decl`
/// returns false. Otherwise, the first term of the list is checked
/// against several known symbol values ([`DECL_HEADS`]) to determine
/// if the AST represents a declaration.
pub fn is_decl(decl: &AST) -> bool {
  let vec: Result<Vec<&AST>, _> = DottedExpr::new(decl).try_into();
  if let Ok(vec) = vec {
    if let Some(AST { value: ASTF::Symbol(head), pos: _ }) = vec.get(0) {
      return DECL_HEADS.contains(&head.borrow());
    }
  }
  false
}

/// Given the body of a [`FnDecl`](super::decl::FnDecl) or a possible
/// [`ConstructorDecl`](super::decl::ConstructorDecl), check to see if
/// the first expression present is a "super" call.
///
/// If a "super" call is present, its arguments, as well as a slice of
/// the rest of the body, is returned. Otherwise, the whole slice is
/// returned untouched.
pub fn detect_super<'a, 'b>(body: &'a [&'b AST]) -> (Option<(Vec<&'b AST>, SourceOffset)>, &'a [&'b AST]) {
  if !body.is_empty() {
    let first: Result<Vec<_>, _> = DottedExpr::new(body[0]).try_into();
    if let Ok(mut first) = first {
      if !first.is_empty() && first[0].value == ASTF::Symbol(String::from("super")) {
        let super_symbol = first.remove(0);
        let super_args = first;
        return (Some((super_args, super_symbol.pos)), &body[1..]);
      }
    }
  }
  (None, body)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::AST_PARSER;

  fn parse_ast(input: &str) -> AST {
    AST_PARSER.parse(input).unwrap()
  }

  #[test]
  fn is_decl_test() {
    assert!(is_decl(&parse_ast("(defn foo ())")));
    assert!(is_decl(&parse_ast("(defclass Example (Reference) (defn bar (x)))")));
    assert!(is_decl(&parse_ast("(defmacro foo ())")));
    assert!(is_decl(&parse_ast("(defconst MY_CONST 3)")));
    assert!(is_decl(&parse_ast("(defenum MyEnum A B C)")));
    assert!(is_decl(&parse_ast("(define-symbol-macro my-macro 3)")));
    assert!(is_decl(&parse_ast("(sys/declare value xyz)")));
    assert!(is_decl(&parse_ast("(sys/min-godot-version 3030000)")));
  }

  #[test]
  fn is_not_decl_test() {
    assert!(!is_decl(&parse_ast("100")));
    assert!(!is_decl(&parse_ast("((defn foo ()))")));
    assert!(!is_decl(&parse_ast("abc")));
    assert!(!is_decl(&parse_ast("(progn 1 2 3)")));
    assert!(!is_decl(&parse_ast("(progn (defconst MY_CONST 3))")));
    assert!(!is_decl(&parse_ast("#t")));
  }

}
