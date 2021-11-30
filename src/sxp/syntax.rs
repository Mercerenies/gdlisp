
//! This module provides several helpers for syntactic forms which
//! need to be special-cased in the parser.
//!
//! This includes forms such as `(quote x)`, which is generated by the
//! `'x` syntax.

use super::ast::AST;
use crate::pipeline::source::SourceOffset;

/// Produces an arbitrary syntactic expression of the form `(a b)`,
/// where `a` is a symbol and `b` is an arbitrary [`AST`].
pub fn unary(head: &str, value: AST, pos: SourceOffset) -> AST {
  AST::list(vec!(
    AST::symbol(head, pos),
    value
  ), pos)
}

/// Produces an arbitrary syntactic expression of the form `(a b c)`,
/// where `a` is a symbol, and `b` and `c` are arbitrary [`AST`]
/// values.
pub fn binary(head: &str, b: AST, c: AST, pos: SourceOffset) -> AST {
  AST::list(vec!(
    AST::symbol(head, pos),
    b,
    c,
  ), pos)
}

/// Produces an arbitrary syntactic expression of the form `(a b c)`,
/// where `a` is a symbol, and `b`, `c`, and `d` are arbitrary [`AST`]
/// values.
pub fn trinary(head: &str, b: AST, c: AST, d: AST, pos: SourceOffset) -> AST {
  AST::list(vec!(
    AST::symbol(head, pos),
    b,
    c,
    d,
  ), pos)
}

/// Produces the syntactic form `(quote value)`.
pub fn quote(value: AST, pos: SourceOffset) -> AST {
  unary("quote", value, pos)
}

/// Produces the syntactic form `(function value)`.
pub fn function(value: AST, pos: SourceOffset) -> AST {
  unary("function", value, pos)
}

/// Produces the syntactic form `(quasiquote value)`.
pub fn quasiquote(value: AST, pos: SourceOffset) -> AST {
  unary("quasiquote", value, pos)
}

/// Produces the syntactic form `(unquote value)`.
pub fn unquote(value: AST, pos: SourceOffset) -> AST {
  unary("unquote", value, pos)
}

/// Produces the syntactic form `(unquote-spliced value)`.
pub fn unquote_spliced(value: AST, pos: SourceOffset) -> AST {
  unary("unquote-spliced", value, pos)
}

/// Produces the syntactic form `(vector x y)`
pub fn vector2(x: AST, y: AST, pos: SourceOffset) -> AST {
  binary("vector", x, y, pos)
}

/// Produces the syntactic form `(vector x y z)`
pub fn vector3(x: AST, y: AST, z: AST, pos: SourceOffset) -> AST {
  trinary("vector", x, y, z, pos)
}

/// Produces the syntactic form `(access-slot x y)`
pub fn access_slot(x: AST, y: AST, pos: SourceOffset) -> AST {
  binary("access-slot", x, y, pos)
}

/// Produces the form `(x:get-node y)`.
///
/// Written in full generality, this is the form `((access-slot x
/// get-node) y)`.
pub fn get_node_on(x: AST, y: AST, pos: SourceOffset) -> AST {
  AST::list(vec!(
    AST::symbol("sys/get-node", pos),
    x,
    y,
  ), pos)
}
