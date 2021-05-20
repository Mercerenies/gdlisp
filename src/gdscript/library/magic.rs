
use crate::compile::symbol_table::call_magic;
use crate::compile::symbol_table::call_magic::table::MagicTable;
use crate::gdscript::op;
use crate::gdscript::expr::Expr;

pub fn bind_magic(table: &mut MagicTable) {

  // Default magic (used by default for all user-defined functions and
  // for any builtins which don't request other magic)
  table.set(String::from("DEFAULT"), Box::new(call_magic::DefaultCall));

  // Addition (+)
  table.set(String::from("ADDITION"),
            Box::new(
              call_magic::CompileToBinOp {
                zero: Expr::from(0),
                bin: op::BinaryOp::Add,
                assoc: call_magic::Assoc::Left,
              }
            ));

  // Multiplication (*)
  table.set(String::from("MULTIPLICATION"),
            Box::new(
              call_magic::CompileToBinOp {
                zero: Expr::from(1),
                bin: op::BinaryOp::Times,
                assoc: call_magic::Assoc::Left,
              }
            ));

  // Subtraction (-)
  table.set(String::from("SUBTRACTION"), Box::new(call_magic::MinusOperation));

  // Division (/)
  table.set(String::from("DIVISION"), Box::new(call_magic::DivOperation));

  // Integer Division (div)
  table.set(String::from("INTEGER-DIVISION"), Box::new(call_magic::IntDivOperation));

  // Modulo Division (mod)
  table.set(String::from("MODULO"), Box::new(call_magic::ModOperation));

  // Equality (=)
  table.set(String::from("EQUAL"), Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::Eq }));

  // Less Than (<)
  table.set(String::from("LESS-THAN"), Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LT }));

  // Greater Than (>)
  table.set(String::from("GREATER-THAN"), Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GT }));

  // Less Than or Equal (<=)
  table.set(String::from("LESS-THAN-OR-EQUAL"), Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LE }));

  // Greater Than or Equal (>=)
  table.set(String::from("GREATER-THAN-OR-EQUAL"), Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GE }));

  // Not Equal (/=)
  table.set(String::from("NOT-EQUAL"),
            Box::new(call_magic::NEqOperation { fallback: Box::new(call_magic::DefaultCall) }));

  // Boolean Not (not)
  table.set(String::from("BOOLEAN-NOT"), Box::new(call_magic::BooleanNotOperation));

  // List (list)
  table.set(String::from("LIST"), Box::new(call_magic::ListOperation));

  // Vector (vector)
  table.set(String::from("VECTOR"), Box::new(call_magic::VectorOperation));

  // Array Subscript (elt)
  table.set(String::from("ARRAY-SUBSCRIPT"), Box::new(call_magic::ArraySubscript));

  // Array Subscript Assignment (set-elt)
  table.set(String::from("ARRAY-SUBSCRIPT-ASSIGNMENT"), Box::new(call_magic::ArraySubscriptAssign));

  // Direct Instance Check (sys/instance_direct?)
  table.set(String::from("DIRECT-INSTANCE-CHECK"), Box::new(call_magic::InstanceOf));

  // Array Membership Check (member?)
  table.set(String::from("ARRAY-MEMBER-CHECK"), Box::new(call_magic::ElementOf));

}

pub fn new_magic_table() -> MagicTable {
  let mut table = MagicTable::new();
  bind_magic(&mut table);
  table
}
