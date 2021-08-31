
//! Provides the [`SpecialRef`] type and associated values.

use super::expr::ExprF;

/// A `SpecialRef` is a type of [`Expr`](super::expr::Expr) which is
/// superficially a literal form but which acquires some state from
/// its surrounding environment or context. That is, a special
/// reference can be thought of as a 0-ary function which is not, in
/// the mathematical sense, a pure function.
///
/// Special references are converted to [`Expr`](super::expr::Expr)
/// either via the explicit constructor
/// [`ExprF::SpecialRef`](super::expr::ExprF::SpecialRef) or via the
/// general-purpose helper
/// [`Expr::from_value`](super::expr::Expr::from_value).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpecialRef {
  /// `ThisFile` will compile into a `load` expression which loads and
  /// returns a reference to the current file, as a GDScript script
  /// object.
  ThisFile,
  /// `ThisFileName` will compile into a string literal expression
  /// which refers to the current (GDScript) filename. If called
  /// during a macro expansion, this will expand to the virtual name
  /// of the macro file.
  ThisFileName,
  /// `ThisTrueFileName` will compile into a string literal expression
  /// which refers to the current (GDScript) filename. If called
  /// during macro expansion, this will expand into the *true* final
  /// name of the file, not the name of the virtual macro file.
  ThisTrueFileName,
}

impl From<SpecialRef> for ExprF {
  fn from(special_ref: SpecialRef) -> ExprF {
    ExprF::SpecialRef(special_ref)
  }
}
