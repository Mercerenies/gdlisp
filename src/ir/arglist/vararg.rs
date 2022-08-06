
use serde::{Serialize, Deserialize};

/// The type of "rest" argument which accumulates any extra arguments
/// to a function call.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum VarArg {
  /// A `&rest` argument accumulates the arguments into a GDLisp list.
  RestArg,
  /// An `&arr` argument accumulates the arguments into a Godot array.
  ArrArg,
}

impl VarArg {

  /// An [`i32`] constant representing no variable argument at all.
  pub const NONE: i32 = 0;

  /// Converts `self` into a numerical value, suitable for
  /// communication with GDScript. This method is guaranteed to never
  /// return [`VarArg::NONE`].
  pub fn into_constant(self) -> i32 {
    match self {
      VarArg::RestArg => 1,
      VarArg::ArrArg => 2,
    }
  }

  /// Returns [`VarArg::NONE`] if `opt` is `None`, or calls
  /// [`VarArg::into_constant`] otherwise.
  pub fn arg_to_const(opt: Option<VarArg>) -> i32 {
    opt.map_or(VarArg::NONE, VarArg::into_constant)
  }

}
