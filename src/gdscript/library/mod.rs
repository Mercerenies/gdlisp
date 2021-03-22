
// Convenient access to the builtins in GDLisp.gd

pub mod classes;
pub mod macros;

use super::expr::Expr;
use super::literal::Literal;
use super::op;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::{LocalVar, VarScope, ValueHint};
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use crate::compile::symbol_table::call_magic;
use crate::ir::arglist::{ArgList, VarArg};
use crate::ir::identifier::{Id, Namespace};
use crate::ir::locals::AccessType;
use crate::ir::macros::MacroData;
use crate::runner::macro_server::named_file_server::MacroID;
use classes::GDSCRIPT_CLASS_NAMES;

use std::collections::{HashSet, HashMap};

pub const GDLISP_NAME: &str = "GDLisp";
pub const CELL_CONTENTS: &str = "contents";
pub const CONSTRUCTOR_NAME: &str = "_init";

pub fn gdlisp_root() -> Expr {
  Expr::Var(String::from(GDLISP_NAME))
}

pub fn on_gdlisp_root(name: String) -> Expr {
  Expr::Attribute(Box::new(gdlisp_root()), name)
}

pub fn nil() -> Expr {
  Expr::null()
}

pub fn cons_class() -> Expr {
  on_gdlisp_root(String::from("Cons"))
}

pub fn symbol_class() -> Expr {
  on_gdlisp_root(String::from("Symbol"))
}

pub fn cell_class() -> Expr {
  on_gdlisp_root(String::from("Cell"))
}

pub fn construct_list(vec: Vec<Expr>) -> Expr {
  vec.into_iter().rev().fold(nil(), |rest, first| {
    Expr::Call(Some(Box::new(cons_class())), String::from("new"), vec!(first, rest))
  })
}

pub fn construct_cell(expr: Expr) -> Expr {
  Expr::Call(Some(Box::new(cell_class())), String::from("new"), vec!(expr))
}

pub fn bind_builtins(table: &mut SymbolTable) {

  // All built-in global class names
  for name in &GDSCRIPT_CLASS_NAMES {
    table.set_var((*name).to_owned(), LocalVar::global((*name).to_owned()));
  }

  // TODO Do we need to bind built-in macros here? Macros should have
  // no runtime presence so that makes me think no, but at the same
  // time we do bind user-defined macros to the symbol table.

  // nil
  table.set_var("nil".to_owned(),
                LocalVar {
                  name: nil(),
                  access_type: AccessType::ClosedRead,
                  scope: VarScope::GlobalVar,
                  assignable: false,
                  value_hint: Some(ValueHint::Literal(Literal::Null)),
                });

  // Cons
  table.set_fn("cons".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, None), FnScope::Global, cons_class(), "new".to_owned()),
               Box::new(call_magic::DefaultCall));

  // intern
  table.set_fn("intern".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, symbol_class(), "new".to_owned()),
               Box::new(call_magic::DefaultCall));

  // Length
  table.set_fn("length".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, gdlisp_root(), "length".to_owned()),
               Box::new(call_magic::DefaultCall));

  // Funcall
  table.set_fn("funcall".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "funcall".to_owned()),
               Box::new(call_magic::DefaultCall));

  // + (Addition)
  table.set_fn("+".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "plus".to_owned()),
               Box::new(
                 call_magic::CompileToBinOp {
                   zero: Expr::from(0),
                   bin: op::BinaryOp::Add,
                   assoc: call_magic::Assoc::Left,
                 }));

  // * (Multiplication)
  table.set_fn("*".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "times".to_owned()),
               Box::new(
                 call_magic::CompileToBinOp {
                   zero: Expr::from(1),
                   bin: op::BinaryOp::Times,
                   assoc: call_magic::Assoc::Left,
                 }));

  // - (Subtraction)
  table.set_fn("-".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "minus".to_owned()),
               Box::new(call_magic::MinusOperation));

  // / (Division)
  table.set_fn("/".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "div".to_owned()),
               Box::new(call_magic::DivOperation));

  // div (Integer Division)
  table.set_fn("div".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "intdiv".to_owned()),
               Box::new(call_magic::IntDivOperation));

  // TODO Unify mod and fmod somehow

  // mod (Modulo)
  table.set_fn("mod".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, None), FnScope::Global, gdlisp_root(), "mod".to_owned()),
               Box::new(call_magic::ModOperation));

  // fmod (Modulo)
  table.set_fn("fmod".to_owned(),
               FnCall::unqualified(FnSpecs::new(2, 0, None), FnScope::Global, "fmod".to_owned()),
               Box::new(call_magic::DefaultCall));

  // = (Equality)
  table.set_fn("=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "eq".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::Eq }));

  // < (Less Than)
  table.set_fn("<".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "lt".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LT }));

  // > (Greater Than)
  table.set_fn(">".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "gt".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GT }));

  // <= (Less Than or Equal)
  table.set_fn("<=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "le".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LE }));

  // >= (Greater Than or Equal)
  table.set_fn(">=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "ge".to_owned()),
               Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GE }));

  // /= (Not Equal)
  table.set_fn("/=".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "ne".to_owned()),
               Box::new(call_magic::NEqOperation { fallback: Box::new(call_magic::DefaultCall) }));

  // not
  table.set_fn("not".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, gdlisp_root(), "not_".to_owned()),
               Box::new(call_magic::BooleanNotOperation));

  // list
  table.set_fn("list".to_owned(),
               FnCall::qualified(FnSpecs::new(0, 0, Some(VarArg::RestArg)), FnScope::Global, gdlisp_root(), "list".to_owned()),
               Box::new(call_magic::ListOperation));

  // vector
  table.set_fn("vector".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 1, None), FnScope::Global, gdlisp_root(), "vector".to_owned()),
               Box::new(call_magic::VectorOperation));

  // list->array
  table.set_fn("list->array".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, gdlisp_root(), "list_to_array".to_owned()),
               Box::new(call_magic::DefaultCall));

  // array->list
  table.set_fn("array->list".to_owned(),
               FnCall::qualified(FnSpecs::new(1, 0, None), FnScope::Global, gdlisp_root(), "array_to_list".to_owned()),
               Box::new(call_magic::DefaultCall));

  // elt (Array element)
  table.set_fn("elt".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, None), FnScope::Global, gdlisp_root(), "elt".to_owned()),
               Box::new(call_magic::ArraySubscript));

  // instance?
  table.set_fn("instance?".to_owned(),
               FnCall::qualified(FnSpecs::new(2, 0, None), FnScope::Global, gdlisp_root(), "istype".to_owned()),
               Box::new(call_magic::DefaultCall)); ////

  // ---- GDScript built-ins that we use unmodified ----

  table.set_fn("str".to_owned(),
               FnCall::unqualified(FnSpecs::new(1, 0, None), FnScope::Global, "str".to_owned()),
               Box::new(call_magic::DefaultCall));

  table.set_fn("int".to_owned(),
               FnCall::unqualified(FnSpecs::new(1, 0, None), FnScope::Global, "int".to_owned()),
               Box::new(call_magic::DefaultCall));

  table.set_fn("bool".to_owned(),
               FnCall::unqualified(FnSpecs::new(1, 0, None), FnScope::Global, "bool".to_owned()),
               Box::new(call_magic::DefaultCall));

  table.set_fn("randomize".to_owned(),
               FnCall::unqualified(FnSpecs::new(0, 0, None), FnScope::Global, "randomize".to_owned()),
               Box::new(call_magic::DefaultCall));

  // (TODO Should we wrap this and the other random functions and make a nice interface to them?)
  table.set_fn("randi".to_owned(),
               FnCall::unqualified(FnSpecs::new(0, 0, None), FnScope::Global, "randi".to_owned()),
               Box::new(call_magic::DefaultCall));

  table.set_fn("rand-range".to_owned(),
               FnCall::unqualified(FnSpecs::new(2, 0, None), FnScope::Global, "rand_range".to_owned()),
               Box::new(call_magic::DefaultCall));

  table.set_fn("clamp".to_owned(),
               FnCall::unqualified(FnSpecs::new(3, 0, None), FnScope::Global, "clamp".to_owned()),
               Box::new(call_magic::DefaultCall));

  table.set_fn("abs".to_owned(),
               FnCall::unqualified(FnSpecs::new(1, 0, None), FnScope::Global, "abs".to_owned()),
               Box::new(call_magic::DefaultCall));

  // TODO Eventually we'll want this to be a multimethod which works
  // on lists as well as arrays. (And possibly elt as well?)
  table.set_fn("len".to_owned(),
               FnCall::unqualified(FnSpecs::new(1, 0, None), FnScope::Global, "len".to_owned()),
               Box::new(call_magic::DefaultCall));

  table.set_var("PI".to_owned(), LocalVar::global("PI".to_owned()));

  // TYPE_* Constants
  table.set_var("Null".to_owned(), LocalVar::global("TYPE_NIL".to_owned()));
  table.set_var("Bool".to_owned(), LocalVar::global("TYPE_BOOL".to_owned()));
  table.set_var("Int".to_owned(), LocalVar::global("TYPE_INT".to_owned()));
  table.set_var("Float".to_owned(), LocalVar::global("TYPE_REAL".to_owned()));
  table.set_var("String".to_owned(), LocalVar::global("TYPE_STRING".to_owned()));
  table.set_var("Vector2".to_owned(), LocalVar::global("TYPE_VECTOR2".to_owned()));
  table.set_var("Rect2".to_owned(), LocalVar::global("TYPE_RECT2".to_owned()));
  table.set_var("Vector3".to_owned(), LocalVar::global("TYPE_VECTOR3".to_owned()));
  table.set_var("Transform2D".to_owned(), LocalVar::global("TYPE_TRANSFORM2D".to_owned()));
  table.set_var("Plane".to_owned(), LocalVar::global("TYPE_PLANE".to_owned()));
  table.set_var("Quat".to_owned(), LocalVar::global("TYPE_QUAT".to_owned()));
  table.set_var("AABB".to_owned(), LocalVar::global("TYPE_AABB".to_owned()));
  table.set_var("Basis".to_owned(), LocalVar::global("TYPE_BASIS".to_owned()));
  table.set_var("Transform".to_owned(), LocalVar::global("TYPE_TRANSFORM".to_owned()));
  table.set_var("Color".to_owned(), LocalVar::global("TYPE_COLOR".to_owned()));
  table.set_var("NodePath".to_owned(), LocalVar::global("TYPE_NODE_PATH".to_owned()));
  table.set_var("RID".to_owned(), LocalVar::global("TYPE_RID".to_owned()));
  table.set_var("Object".to_owned(), LocalVar::global("TYPE_OBJECT".to_owned()));
  table.set_var("Dictionary".to_owned(), LocalVar::global("TYPE_DICTIONARY".to_owned()));
  table.set_var("Array".to_owned(), LocalVar::global("TYPE_ARRAY".to_owned()));
  table.set_var("PoolByteArray".to_owned(), LocalVar::global("TYPE_RAW_ARRAY".to_owned()));
  table.set_var("PoolIntArray".to_owned(), LocalVar::global("TYPE_INT_ARRAY".to_owned()));
  table.set_var("PoolRealArray".to_owned(), LocalVar::global("TYPE_REAL_ARRAY".to_owned()));
  table.set_var("PoolStringArray".to_owned(), LocalVar::global("TYPE_STRING_ARRAY".to_owned()));
  table.set_var("PoolVector2Array".to_owned(), LocalVar::global("TYPE_VECTOR2_ARRAY".to_owned()));
  table.set_var("PoolVector3Array".to_owned(), LocalVar::global("TYPE_VECTOR3_ARRAY".to_owned()));
  table.set_var("PoolColorArray".to_owned(), LocalVar::global("TYPE_COLOR_ARRAY".to_owned()));
  table.set_var("TYPE_MAX".to_owned(), LocalVar::global("TYPE_MAX".to_owned()));


}

pub fn all_builtin_names() -> HashSet<Id> {
  // This is a *really* roundabout way of doing this, but whatever.
  // The canonical list is given in bind_builtins, so for the sake of
  // DRY we'll delegate to that function.
  let mut table = SymbolTable::new();
  bind_builtins(&mut table);
  let mut names = HashSet::new();
  for (func, _, _) in table.fns() {
    names.insert(Id::new(Namespace::Function, func.to_owned()));
  }
  for (var, _) in table.vars() {
    names.insert(Id::new(Namespace::Value, var.to_owned()));
  }
  names
}

pub fn bind_builtin_macros(macros: &mut HashMap<String, MacroData>) {

  // or
  macros.insert(String::from("or"),
                MacroData { id: MacroID(macros::ID_OR_FUNCTION), args: ArgList::rest(), imported: true });

  // and
  macros.insert(String::from("and"),
                MacroData { id: MacroID(macros::ID_AND_FUNCTION), args: ArgList::rest(), imported: true });

  // let*
  macros.insert(String::from("let*"),
                MacroData { id: MacroID(macros::ID_LETSTAR_FUNCTION), args: ArgList::rest(), imported: true });

  // defvars
  macros.insert(String::from("defvars"),
                MacroData { id: MacroID(macros::ID_DEFVARS_FUNCTION), args: ArgList::rest(), imported: true });

}
