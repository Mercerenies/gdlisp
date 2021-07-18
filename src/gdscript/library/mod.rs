
//! Convenient access to the builtins in `GDLisp.gd`.
//!
//! The most commonly-used functions from this module are
//! [`bind_builtins`], [`bind_builtin_macros`], and
//! [`all_builtin_names`], though the module also provides several
//! simpler helper functions for commonly-used GDLisp builtins.

pub mod classes;
pub mod macros;
pub mod keys;
pub mod magic;

use super::expr::Expr;
use super::literal::Literal;
use super::op;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::{LocalVar, ValueHint};
use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
use crate::compile::symbol_table::call_magic;
use crate::ir::arglist::{ArgList, VarArg};
use crate::ir::identifier::{Id, Namespace};
use crate::ir::macros::MacroData;
use crate::runner::macro_server::named_file_server::MacroID;
use classes::GDSCRIPT_CLASS_NAMES;

use std::collections::{HashSet, HashMap};

/// The name of the top-level GDLisp singleton object.
pub const GDLISP_NAME: &str = "GDLisp";

/// The name of the sole field in the `Cell` class.
pub const CELL_CONTENTS: &str = "contents";

/// The name of a GDScript constructor.
pub const CONSTRUCTOR_NAME: &str = "_init";

/// An expression which accesses the global GDLisp singleton object.
pub fn gdlisp_root() -> Expr {
  Expr::Var(String::from(GDLISP_NAME))
}

/// An expression which accesses a specific field on [`gdlisp_root`].
pub fn on_gdlisp_root(name: String) -> Expr {
  Expr::Attribute(Box::new(gdlisp_root()), name)
}

fn toplevel_enum(name: &str, values: &[&str]) -> LocalVar {
  let hint = ValueHint::enumeration(values.iter().copied());
  let mut var = LocalVar::file_constant(name.to_owned()).with_hint(hint);
  var.name = var.name.into_imported(GDLISP_NAME.to_owned());
  var
}

fn gdlisp_function(name: &str, specs: FnSpecs) -> FnCall {
  let mut func = FnCall::file_constant(specs, FnScope::Global, String::from(name));
  func.object = func.object.into_imported(GDLISP_NAME.to_owned());
  func
}

/// The GDScript `null` value.
pub fn nil() -> Expr {
  Expr::null()
}

/// An expression representing the GDLisp `Cons` class.
pub fn cons_class() -> Expr {
  on_gdlisp_root(String::from("Cons"))
}

/// An expression representing the GDLisp `Symbol` class.
pub fn symbol_class() -> Expr {
  on_gdlisp_root(String::from("Symbol"))
}

/// An expression representing the GDLisp `Cell` class.
pub fn cell_class() -> Expr {
  on_gdlisp_root(String::from("Cell"))
}

/// Given a vector of expressions `vec`, produce an expression which
/// produces a GDLisp list containing those expressions in order.
pub fn construct_list(vec: Vec<Expr>) -> Expr {
  vec.into_iter().rev().fold(nil(), |rest, first| {
    Expr::Call(Some(Box::new(cons_class())), String::from("new"), vec!(first, rest))
  })
}

/// Given a GDLisp expression, produce an expression which constructs
/// a cell containing it.
pub fn construct_cell(expr: Expr) -> Expr {
  Expr::Call(Some(Box::new(cell_class())), String::from("new"), vec!(expr))
}

/// Bind all GDLisp and GDScript built-in names to the given symbol
/// table.
///
/// This function does *not* bind built-in macros.
/// [`bind_builtin_macros`] is a separate function provided for that
/// behavior.
pub fn bind_builtins(table: &mut SymbolTable, minimalist: bool) {

  // All built-in global class names
  for name in &GDSCRIPT_CLASS_NAMES {
    table.set_var((*name).to_owned(), LocalVar::superglobal((*name).to_owned()));
  }

  // TODO Do we need to bind built-in macros here? Macros should have
  // no runtime presence so that makes me think no, but at the same
  // time we do bind user-defined macros to the symbol table.

  if !minimalist {

    // nil
    table.set_var("nil".to_owned(),
                  LocalVar::superglobal(String::from("null")).with_hint(ValueHint::Literal(Literal::Null)));

    // Cons
    table.set_fn("cons".to_owned(),
                 gdlisp_function("cons", FnSpecs::new(2, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // intern
    table.set_fn("intern".to_owned(),
                 gdlisp_function("intern", FnSpecs::new(1, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // Length
    table.set_fn("length".to_owned(),
                 gdlisp_function("length", FnSpecs::new(1, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // Funcall
    table.set_fn("funcall".to_owned(),
                 gdlisp_function("funcall", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::DefaultCall));

    // + (Addition)
    table.set_fn("+".to_owned(),
                 gdlisp_function("plus", FnSpecs::new(0, 0, Some(VarArg::RestArg))),
                 Box::new(
                   call_magic::CompileToBinOp {
                     zero: Expr::from(0),
                     bin: op::BinaryOp::Add,
                     assoc: call_magic::Assoc::Left,
                   }));

    // * (Multiplication)
    table.set_fn("*".to_owned(),
                 gdlisp_function("times", FnSpecs::new(0, 0, Some(VarArg::RestArg))),
                 Box::new(
                   call_magic::CompileToBinOp {
                     zero: Expr::from(1),
                     bin: op::BinaryOp::Times,
                     assoc: call_magic::Assoc::Left,
                   }));

    // - (Subtraction)
    table.set_fn("-".to_owned(),
                 gdlisp_function("minus", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::MinusOperation));

    // / (Division)
    table.set_fn("/".to_owned(),
                 gdlisp_function("div", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::DivOperation));

    // div (Integer Division)
    table.set_fn("div".to_owned(),
                 gdlisp_function("intdiv", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::IntDivOperation));

    // TODO Unify mod and fmod somehow

    // mod (Modulo)
    table.set_fn("mod".to_owned(),
                 gdlisp_function("mod", FnSpecs::new(2, 0, None)),
                 Box::new(call_magic::ModOperation));

    // fmod (Modulo)
    table.set_fn("fmod".to_owned(),
                 FnCall::superglobal(FnSpecs::new(2, 0, None), FnScope::Superglobal, "fmod".to_owned()),
                 Box::new(call_magic::DefaultCall));

    // = (Equality)
    table.set_fn("=".to_owned(),
                 gdlisp_function("eq", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::Eq }));

    // < (Less Than)
    table.set_fn("<".to_owned(),
                 gdlisp_function("lt", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LT }));

    // > (Greater Than)
    table.set_fn(">".to_owned(),
                 gdlisp_function("gt", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GT }));

    // <= (Less Than or Equal)
    table.set_fn("<=".to_owned(),
                 gdlisp_function("le", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::LE }));

    // >= (Greater Than or Equal)
    table.set_fn(">=".to_owned(),
                 gdlisp_function("ge", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::CompileToTransCmp { bin: op::BinaryOp::GE }));

    // /= (Not Equal)
    table.set_fn("/=".to_owned(),
                 gdlisp_function("ne", FnSpecs::new(1, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::NEqOperation { fallback: Box::new(call_magic::DefaultCall) }));

    // not
    table.set_fn("not".to_owned(),
                 gdlisp_function("not_", FnSpecs::new(1, 0, None)),
                 Box::new(call_magic::BooleanNotOperation));

    // list
    table.set_fn("list".to_owned(),
                 gdlisp_function("list", FnSpecs::new(0, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::ListOperation));

    // vector
    table.set_fn("vector".to_owned(),
                 gdlisp_function("vector", FnSpecs::new(1, 1, None)),
                 Box::new(call_magic::VectorOperation));

    // list->array
    table.set_fn("list->array".to_owned(),
                 gdlisp_function("list_to_array", FnSpecs::new(1, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // array->list
    table.set_fn("array->list".to_owned(),
                 gdlisp_function("array_to_list", FnSpecs::new(1, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // elt (Array element)
    table.set_fn("elt".to_owned(),
                 gdlisp_function("elt", FnSpecs::new(2, 0, None)),
                 Box::new(call_magic::ArraySubscript));

    table.set_fn("set-elt".to_owned(),
                 gdlisp_function("set_elt", FnSpecs::new(3, 0, None)),
                 Box::new(call_magic::ArraySubscriptAssign));

    // instance? (TODO This can be a multimethod, or if we decide not to go that route then we can call-magic away some of the checks if we know we're looking at a class name)
    table.set_fn("instance?".to_owned(),
                 gdlisp_function("istype", FnSpecs::new(2, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // sys/instance-direct? (Always uses `is` directly; should be primitive and magicked away)
    table.set_fn("sys/instance-direct?".to_owned(),
                 gdlisp_function("istype_direct", FnSpecs::new(2, 0, None)),
                 Box::new(call_magic::InstanceOf));

    // gensym
    table.set_fn("gensym".to_owned(),
                 gdlisp_function("gensym", FnSpecs::new(0, 1, None)),
                 Box::new(call_magic::DefaultCall));

    // member? (Array has element)
    table.set_fn("member?".to_owned(),
                 gdlisp_function("elt", FnSpecs::new(2, 0, None)),
                 Box::new(call_magic::ElementOf));

    // map (For lists and arrays)
    table.set_fn("map".to_owned(),
                 gdlisp_function("map", FnSpecs::new(2, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // filter (For lists and arrays)
    table.set_fn("filter".to_owned(),
                 gdlisp_function("filter", FnSpecs::new(2, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // append (For lists)
    table.set_fn("append".to_owned(),
                 gdlisp_function("append", FnSpecs::new(0, 0, Some(VarArg::RestArg))),
                 Box::new(call_magic::DefaultCall));

    // sys/qq-smart-list (Used in quasiquote expansion)
    table.set_fn("sys/qq-smart-list".to_owned(),
                 gdlisp_function("qq_smart_list", FnSpecs::new(1, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // sys/qq-smart-array (Used in quasiquote expansion)
    table.set_fn("sys/qq-smart-array".to_owned(),
                 gdlisp_function("qq_smart_array", FnSpecs::new(1, 0, None)),
                 Box::new(call_magic::DefaultCall));

    // ---- GDScript built-ins that we use unmodified ----

    // TODO sys/declare these in GDLisp_bootstrapping.lisp

    table.set_fn("str".to_owned(),
                 FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Superglobal, "str".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("int".to_owned(),
                 FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Superglobal, "int".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("bool".to_owned(),
                 FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Superglobal, "bool".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("randomize".to_owned(),
                 FnCall::superglobal(FnSpecs::new(0, 0, None), FnScope::Superglobal, "randomize".to_owned()),
                 Box::new(call_magic::DefaultCall));

    // (TODO Should we wrap this and the other random functions and make a nice interface to them?)
    table.set_fn("randi".to_owned(),
                 FnCall::superglobal(FnSpecs::new(0, 0, None), FnScope::Superglobal, "randi".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("randf".to_owned(),
                 FnCall::superglobal(FnSpecs::new(0, 0, None), FnScope::Superglobal, "randf".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("rand-range".to_owned(),
                 FnCall::superglobal(FnSpecs::new(2, 0, None), FnScope::Superglobal, "rand_range".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("clamp".to_owned(),
                 FnCall::superglobal(FnSpecs::new(3, 0, None), FnScope::Superglobal, "clamp".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("abs".to_owned(),
                 FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Superglobal, "abs".to_owned()),
                 Box::new(call_magic::DefaultCall));

    // TODO Eventually we'll want this to be a multimethod which works
    // on lists as well as arrays. (And possibly elt as well?)
    table.set_fn("len".to_owned(),
                 FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Superglobal, "len".to_owned()),
                 Box::new(call_magic::DefaultCall));

    // TODO Definitely want to wrap this (and all of the mouse functions) in a nice namespace or module or something.
    table.set_fn("get-global-mouse-position".to_owned(),
                 FnCall::superglobal(FnSpecs::new(0, 0, None), FnScope::Superglobal, "get_global_mouse_position".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("push-error".to_owned(),
                 FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Superglobal, "push_error".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("push-warning".to_owned(),
                 FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Superglobal, "push_warning".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_fn("typeof".to_owned(),
                 FnCall::superglobal(FnSpecs::new(1, 0, None), FnScope::Superglobal, "typeof".to_owned()),
                 Box::new(call_magic::DefaultCall));

    table.set_var("PI".to_owned(), LocalVar::superglobal("PI".to_owned()));
    table.set_var("SPKEY".to_owned(), LocalVar::superglobal("SPKEY".to_owned()));

    // TYPE_* Constants
    table.set_var("Null".to_owned(), LocalVar::superglobal("TYPE_NIL".to_owned()));
    table.set_var("Bool".to_owned(), LocalVar::superglobal("TYPE_BOOL".to_owned()));
    table.set_var("Int".to_owned(), LocalVar::superglobal("TYPE_INT".to_owned()));
    table.set_var("Float".to_owned(), LocalVar::superglobal("TYPE_REAL".to_owned()));
    table.set_var("String".to_owned(), LocalVar::superglobal("TYPE_STRING".to_owned()));
    table.set_var("Vector2".to_owned(), LocalVar::superglobal("TYPE_VECTOR2".to_owned()));
    table.set_var("Rect2".to_owned(), LocalVar::superglobal("TYPE_RECT2".to_owned()));
    table.set_var("Vector3".to_owned(), LocalVar::superglobal("TYPE_VECTOR3".to_owned()));
    table.set_var("Transform2D".to_owned(), LocalVar::superglobal("TYPE_TRANSFORM2D".to_owned()));
    table.set_var("Plane".to_owned(), LocalVar::superglobal("TYPE_PLANE".to_owned()));
    table.set_var("Quat".to_owned(), LocalVar::superglobal("TYPE_QUAT".to_owned()));
    table.set_var("AABB".to_owned(), LocalVar::superglobal("TYPE_AABB".to_owned()));
    table.set_var("Basis".to_owned(), LocalVar::superglobal("TYPE_BASIS".to_owned()));
    table.set_var("Transform".to_owned(), LocalVar::superglobal("TYPE_TRANSFORM".to_owned()));
    table.set_var("Color".to_owned(), LocalVar::superglobal("TYPE_COLOR".to_owned()));
    table.set_var("NodePath".to_owned(), LocalVar::superglobal("TYPE_NODE_PATH".to_owned()));
    table.set_var("RID".to_owned(), LocalVar::superglobal("TYPE_RID".to_owned()));
    table.set_var("Object".to_owned(), LocalVar::superglobal("TYPE_OBJECT".to_owned()));
    table.set_var("Dictionary".to_owned(), LocalVar::superglobal("TYPE_DICTIONARY".to_owned()));
    table.set_var("Array".to_owned(), LocalVar::superglobal("TYPE_ARRAY".to_owned()));
    table.set_var("PoolByteArray".to_owned(), LocalVar::superglobal("TYPE_RAW_ARRAY".to_owned()));
    table.set_var("PoolIntArray".to_owned(), LocalVar::superglobal("TYPE_INT_ARRAY".to_owned()));
    table.set_var("PoolRealArray".to_owned(), LocalVar::superglobal("TYPE_REAL_ARRAY".to_owned()));
    table.set_var("PoolStringArray".to_owned(), LocalVar::superglobal("TYPE_STRING_ARRAY".to_owned()));
    table.set_var("PoolVector2Array".to_owned(), LocalVar::superglobal("TYPE_VECTOR2_ARRAY".to_owned()));
    table.set_var("PoolVector3Array".to_owned(), LocalVar::superglobal("TYPE_VECTOR3_ARRAY".to_owned()));
    table.set_var("PoolColorArray".to_owned(), LocalVar::superglobal("TYPE_COLOR_ARRAY".to_owned()));
    table.set_var("TYPE_MAX".to_owned(), LocalVar::superglobal("TYPE_MAX".to_owned()));

    // BUTTON_* Constants
    table.set_var("Mouse".to_owned(),
                  toplevel_enum("Mouse", &["LEFT", "RIGHT", "MIDDLE", "XBUTTON1",
                                           "XBUTTON2", "WHEEL_UP", "WHEEL_DOWN",
                                           "WHEEL_LEFT", "WHEEL_RIGHT", "MASK_LEFT",
                                           "MASK_RIGHT", "MASK_MIDDLE", "MASK_XBUTTON1",
                                           "MASK_XBUTTON2"]));

    // MARGIN_* Constants
    table.set_var("Margin".to_owned(),
                  toplevel_enum("Margin", &["LEFT", "TOP", "RIGHT", "BOTTOM"]));

    // CORNER_* Constants
    table.set_var("Corner".to_owned(),
                  toplevel_enum("Corner", &["TOP_LEFT", "TOP_RIGHT", "BOTTOM_RIGHT", "BOTTOM_LEFT"]));

    // ORIENTATION_* Constants
    table.set_var("Orientation".to_owned(),
                  toplevel_enum("Orientation", &["VERTICAL", "HORIZONTAL"]));

    // HALIGN_* Constants
    table.set_var("HAlign".to_owned(),
                  toplevel_enum("HAlign", &["LEFT", "CENTER", "RIGHT"]));

    // VALIGN_* Constants
    table.set_var("VAlign".to_owned(),
                  toplevel_enum("VAlign", &["TOP", "CENTER", "BOTTOM"]));

    // KEY_* Constants
    table.set_var("Key".to_owned(),
                  toplevel_enum("Key", &keys::GDSCRIPT_KEY_NAMES));

    // KEY_MASK_* Constants
    table.set_var("KeyMask".to_owned(),
                  toplevel_enum("KeyMask", &["CODE_MASK", "MODIFIER_MASK", "SHIFT", "ALT",
                                             "META", "CTRL", "CMD", "KPAD", "GROUP_SWITCH"]));

    // JOY_* Constants
    table.set_var("Joy".to_owned(),
                  toplevel_enum("Joy", &["BUTTON_0", "BUTTON_1", "BUTTON_2",
                                         "BUTTON_3", "BUTTON_4", "BUTTON_5", "BUTTON_6", "BUTTON_7",
                                         "BUTTON_8", "BUTTON_9", "BUTTON_10", "BUTTON_11", "BUTTON_12",
                                         "BUTTON_13", "BUTTON_14", "BUTTON_15", "BUTTON_MAX", "SONY_CIRCLE",
                                         "SONY_X", "SONY_SQUARE", "SONY_TRIANGLE", "XBOX_B", "XBOX_A",
                                         "XBOX_X", "XBOX_Y", "DS_A", "DS_B", "DS_X", "DS_Y", "VR_GRIP",
                                         "VR_PAD", "VR_TRIGGER", "OCULUS_AX", "OCULUS_BY", "OCULUS_MENU",
                                         "OPENVR_MENU", "SELECT", "START", "DPAD_UP", "DPAD_DOWN", "DPAD_LEFT",
                                         "DPAD_RIGHT", "L", "L2", "L3", "R", "R2", "R3", "AXIS_0", "AXIS_1",
                                         "AXIS_2", "AXIS_3", "AXIS_4", "AXIS_5", "AXIS_6", "AXIS_7", "AXIS_8",
                                         "AXIS_9", "AXIS_MAX", "ANALOG_LX", "ANALOG_LY", "ANALOG_RX", "ANALOG_RY",
                                         "ANALOG_L2", "ANALOG_R2", "VR_ANALOG_TRIGGER", "VR_ANALOG_GRIP",
                                         "OPENVR_TOUCHPADX", "OPENVR_TOUCHPADY"]));

    // MIDI_MESSAGE_* Constants
    table.set_var("MIDIMessage".to_owned(),
                  toplevel_enum("MIDIMessage", &["NOTE_OFF", "NOTE_ON", "AFTERTOUCH", "CONTROL_CHANGE",
                                                 "PROGRAM_CHANGE", "CHANNEL_PRESSURE", "PITCH_BEND"]));

  }

  // TODO The remaining constants from @GlobalScope need to be copied over here

}

/// Get a collection of all of the built-in names in GDLisp and
/// GDScript, in no particular order.
pub fn all_builtin_names(minimalist: bool) -> HashSet<Id> {
  // This is a *really* roundabout way of doing this, but whatever.
  // The canonical list is given in bind_builtins, so for the sake of
  // DRY we'll delegate to that function.
  let mut table = SymbolTable::new();
  bind_builtins(&mut table, minimalist);
  let mut names = HashSet::new();
  for (func, _, _) in table.fns() {
    names.insert(Id::new(Namespace::Function, func.to_owned()));
  }
  for (var, _) in table.vars() {
    names.insert(Id::new(Namespace::Value, var.to_owned()));
  }
  names
}

/// Bind all of the built-in GDLisp macros to the macro table given.
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

  // when
  macros.insert(String::from("when"),
                MacroData { id: MacroID(macros::ID_WHEN_FUNCTION), args: ArgList::rest(), imported: true });

  // unless
  macros.insert(String::from("unless"),
                MacroData { id: MacroID(macros::ID_UNLESS_FUNCTION), args: ArgList::rest(), imported: true });

  // if
  macros.insert(String::from("if"),
                MacroData { id: MacroID(macros::ID_IF_FUNCTION), args: ArgList::rest(), imported: true });

  // yield*
  macros.insert(String::from("yield*"),
                MacroData { id: MacroID(macros::ID_YIELDSTAR_FUNCTION), args: ArgList::rest(), imported: true });

}
