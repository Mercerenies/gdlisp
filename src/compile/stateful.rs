
//! Types for expressing whether or not an [`Expr`] has side effects.

use super::factory;
use super::stmt_wrapper::{self, StmtWrapper};
use super::names::generator::NameGenerator;
use super::body::builder::StmtBuilder;
use crate::gdscript::expr::{Expr, ExprF};
use crate::ir::access_type::AccessType;
use crate::pipeline::source::SourceOffset;

/// An `StExpr` is an expression coupled with a declaration of that
/// expression's side effects.
///
/// Note that such a declaration is context-sensitive, so there is no
/// general-purpose function for converting an `Expr` into an
/// `StExpr`. For example, simple variable access `foobar` could be
/// read-only, assuming the variable is a local variable, but it could
/// also be read-write, if the variable is an instance variable which
/// has a `setget` modifier. Likewise, a function call is generally
/// read-write, but if we can prove that the call is free of side
/// effects, we can reduce this.
#[derive(Debug, Clone)]
pub struct StExpr {
  pub expr: Expr,
  pub side_effects: SideEffects
}

/// A declaration of side effects.
///
/// `SideEffects` has an [`Ord`] instance, where more invasive effects
/// are considered greater than less invasive ones. This makes it
/// possible to use [`std::cmp::max`] to take the more invasive of two
/// side effects.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum SideEffects {
  /// The expression has no side effects. An expression with this
  /// declaration modifies nothing and reads no mutable state. It
  /// should be possible to move the expression, for inlining
  /// purposes, to anywhere else within the same scope, or anywhere
  /// else which contains the appropriate constant values necessary
  /// for evaluation. An expression with `SideEffects::None` can
  /// safely be evaluated several times, or have several identical
  /// evaluations reduced to one. The optimizer has a great deal of
  /// control over these expressions.
  None,
  /// The expression does not modify any state, but it may read from
  /// mutable state. An expression with `SideEffects::ReadsState` may
  /// be evaluated multiple times, or may have multiple evaluations
  /// reduced to one, provided that no [`SideEffects::ModifiesState`]
  /// calls happen in between the evaluations. Such expressions may
  /// also be reordered relative to other `SideEffects::ReadsState` or
  /// `SideEffects::None` expressions.
  ReadsState,
  /// The expression may read or write to ambient state. It is not
  /// safe to reorder multiple `SideEffects::ModifiesState`
  /// expressions relative to one another, and evaluating such a
  /// statement multiple times is different than evaluating it only
  /// once. The optimizer is very limited in this case.
  ModifiesState,
}

/// A Boolean-isomorphic type indicating whether a result is required.
///
/// An argument of type `NeedsResult` is used in functions such as
/// [`CompilerFrame::compile_expr`](super::frame::CompilerFrame::compile_expr)
/// to indicate whether or not the result is going to be used for
/// anything. There are some expressions that can compile to a more
/// efficient form if we know the result is unneeded. For instance,
/// `if` expressions in general need to create a local variable to
/// store the result in, but if we know the result is going to be
/// discarded, then we can safely skip that step.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NeedsResult { No, Yes }

impl From<NeedsResult> for bool {
  fn from(s: NeedsResult) -> bool {
    s == NeedsResult::Yes
  }
}

impl From<bool> for NeedsResult {
  fn from(b: bool) -> NeedsResult {
    if b { NeedsResult::Yes } else { NeedsResult::No }
  }
}

impl NeedsResult {

  /// Returns `Yes` iff either `self` or `other` is `Yes`.
  pub fn or(self, other: NeedsResult) -> NeedsResult {
    NeedsResult::from(self == NeedsResult::Yes || other == NeedsResult::Yes)
  }

  /// Returns `Yes` iff both `self` and `other` are `Yes`.
  pub fn and(self, other: NeedsResult) -> NeedsResult {
    NeedsResult::from(self == NeedsResult::Yes && other == NeedsResult::Yes)
  }

  /// Construct an appropriate [`StmtWrapper`] that can store the
  /// result of an expression, if needed.
  ///
  /// If `self` is `Yes`, then this function declares a variable (into
  /// `builder`) with a local, generated name (using `prefix` as the
  /// prefix) and then returns a `StmtWrapper` which assigns a value
  /// to that variable. The `Expr`, in this case, is the variable
  /// name.
  ///
  /// If `self` is `No`, then this function returns a vacuous
  /// `StmtWrapper` and [`Expr::null()`] as the expression.
  ///
  /// In general, the returned `StmtWrapper` should be supplied with
  /// the Godot expression which is, semantically, the result of the
  /// IR expression, and the returned `Expr` can be used to reference
  /// it later. If `self` is `No`, then the returned `Expr` is nil, as
  /// we chose not to store the result anywhere.
  pub fn into_destination(self,
                          gen: &mut impl NameGenerator,
                          builder: &mut StmtBuilder,
                          prefix: &str,
                          pos: SourceOffset)
                          -> (Box<dyn StmtWrapper>, Expr) {
    if self.into() {
      let var_name = factory::declare_var(gen, builder, prefix, None, pos);
      let destination = Box::new(stmt_wrapper::assign_to_var(var_name.clone(), pos)) as Box<dyn StmtWrapper>;
      (destination, Expr::new(ExprF::Var(var_name), pos))
    } else {
      let destination = Box::new(stmt_wrapper::Vacuous) as Box<dyn StmtWrapper>;
      (destination, Expr::null(pos))
    }
  }

}

impl SideEffects {

  /// Whether or not `self` is *at least* `SideEffects::ReadsState`.
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::compile::stateful::SideEffects;
  /// assert!(!SideEffects::None.reads_state());
  /// assert!(SideEffects::ReadsState.reads_state());
  /// assert!(SideEffects::ModifiesState.reads_state());
  /// ```
  pub fn reads_state(&self) -> bool {
    *self >= SideEffects::ReadsState
  }

  /// Whether or not `self` is *at least*
  /// `SideEffects::ModifiesState`.
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::compile::stateful::SideEffects;
  /// assert!(!SideEffects::None.modifies_state());
  /// assert!(!SideEffects::ReadsState.modifies_state());
  /// assert!(SideEffects::ModifiesState.modifies_state());
  /// ```
  pub fn modifies_state(&self) -> bool {
    *self >= SideEffects::ModifiesState
  }
}

/// If we're writing to a variable, then the side effect is obviously
/// ModifiesState and this From instance should not be used. If we're
/// reading from a variable, then we can convert the AccessType of
/// that variable into a SideEffects by determining whether or not the
/// variable is ever modified (x.is_written_to) to figure out if we're
/// querying mutable state or simply accessing a constant value.
impl From<AccessType> for SideEffects {
  fn from(x: AccessType) -> SideEffects {
    if x.is_written_to() {
      SideEffects::ReadsState
    } else {
      SideEffects::None
    }
  }
}
