
//! GDLisp call magic.
//!
//! Call magic can be thought of as a sort of optimization and
//! bootstrapping technique. In essence, a function call like `(+ a
//! b)` in GDLisp should, in full generality, compile to a GDScript
//! function call of the form `GDLisp.plus(GDLisp.cons(a,
//! GDLisp.cons(b, null)))`. However, for obvious reasons, we would
//! prefer that this simply compile to `a + b`.
//!
//! This is exactly what call magic accomplishes. The function `+` in
//! GDLisp is marked with a special flag which indicates that it is
//! eligible for call magic. There is a `GDLisp.gd` implementation of
//! `+` in full generality which will get called if the programmer
//! ever uses `+` as a first-class function (i.e. `(function +)` or
//! `#'+`). However, if the programmer ever calls `+` directly, then
//! the call magic is guaranteed to fire and replace the call with the
//! correct syntax.
//!
//! As mentioned above, this is not *merely* an optimization. The `+`
//! function in GDLisp could ostensibly be implemented in terms of
//! itself. Since call magic is guaranteed behavior, this bootstrapped
//! function would be guaranteed to compile to actual addition and not
//! be recursive.

use crate::compile::symbol_table::call_magic::{CallMagic, Assoc};
use crate::compile::symbol_table::call_magic::table::MagicTable;
use crate::gdscript::op;
use crate::gdscript::literal::Literal;

/// Bind all GDLisp call magic to the magic table given.
pub fn bind_magic(table: &mut MagicTable) {

  // Default magic (used by default for all user-defined functions and
  // for any builtins which don't request other magic)
  table.set(String::from("DEFAULT"), CallMagic::DefaultCall);

  // Addition (+)
  table.set(String::from("ADDITION"),
            CallMagic::CompileToBinOp(Literal::from(0), op::BinaryOp::Add, Assoc::Left));

  // Multiplication (*)
  table.set(String::from("MULTIPLICATION"),
            CallMagic::CompileToBinOp(Literal::from(1), op::BinaryOp::Times, Assoc::Left));

  // Subtraction (-)
  table.set(String::from("SUBTRACTION"), CallMagic::MinusOperation);

  // Division (/)
  table.set(String::from("DIVISION"), CallMagic::DivOperation);

  // Integer Division (div)
  table.set(String::from("INTEGER-DIVISION"), CallMagic::IntDivOperation);

  // Modulo Division (mod)
  table.set(String::from("MODULO"), CallMagic::ModOperation);

  // Min function (min)
  table.set(String::from("MIN-FUNCTION"), CallMagic::MinFunction);

  // Max function (max)
  table.set(String::from("MAX-FUNCTION"), CallMagic::MaxFunction);

  // Equality (=)
  table.set(String::from("EQUAL"), CallMagic::CompileToTransCmp(op::BinaryOp::Eq));

  // Less Than (<)
  table.set(String::from("LESS-THAN"), CallMagic::CompileToTransCmp(op::BinaryOp::LT));

  // Greater Than (>)
  table.set(String::from("GREATER-THAN"), CallMagic::CompileToTransCmp(op::BinaryOp::GT));

  // Less Than or Equal (<=)
  table.set(String::from("LESS-THAN-OR-EQUAL"), CallMagic::CompileToTransCmp(op::BinaryOp::LE));

  // Greater Than or Equal (>=)
  table.set(String::from("GREATER-THAN-OR-EQUAL"), CallMagic::CompileToTransCmp(op::BinaryOp::GE));

  // Not Equal (/=)
  table.set(String::from("NOT-EQUAL"),
            CallMagic::NEqOperation(Box::new(CallMagic::DefaultCall)));

  // Boolean Not (not)
  table.set(String::from("BOOLEAN-NOT"), CallMagic::BooleanNotOperation);

  // List (list)
  table.set(String::from("LIST"), CallMagic::ListOperation);

  // Vector (vector)
  table.set(String::from("VECTOR"), CallMagic::VectorOperation);

  // Array Subscript (elt)
  table.set(String::from("ARRAY-SUBSCRIPT"), CallMagic::ArraySubscript);

  // Array Subscript Assignment (set-elt)
  table.set(String::from("ARRAY-SUBSCRIPT-ASSIGNMENT"), CallMagic::ArraySubscriptAssign);

  // Direct Instance Check (sys/instance_direct?)
  table.set(String::from("DIRECT-INSTANCE-CHECK"), CallMagic::InstanceOf);

  // Array Membership Check (member?)
  table.set(String::from("ARRAY-MEMBER-CHECK"), CallMagic::ElementOf);

  // Node access (sys/get-node)
  table.set(String::from("GET-NODE-SYNTAX"), CallMagic::GetNodeSyntax);

  // str function
  table.set(String::from("VARARG-STR"), CallMagic::CompileToVarargCall(String::from("str")));

  // printerr function
  table.set(String::from("VARARG-PRINTERR"), CallMagic::CompileToVarargCall(String::from("printerr")));

  // printraw function
  table.set(String::from("VARARG-PRINTRAW"), CallMagic::CompileToVarargCall(String::from("printraw")));

  // print-debug function
  table.set(String::from("VARARG-PRINTDEBUG"), CallMagic::CompileToVarargCall(String::from("print_debug")));

  // printt function
  table.set(String::from("VARARG-PRINTT"), CallMagic::CompileToVarargCall(String::from("printt")));

  // prints function
  table.set(String::from("VARARG-PRINTS"), CallMagic::CompileToVarargCall(String::from("prints")));

  // print function
  table.set(String::from("VARARG-PRINT"), CallMagic::CompileToVarargCall(String::from("print")));

}

/// Produce a new [`MagicTable`] with all of the magic bound as though
/// via [`bind_magic`].
pub fn standard_magic_table() -> MagicTable {
  let mut table = MagicTable::new();
  bind_magic(&mut table);
  table
}
