
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
use crate::compile::Compiler;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::LocalVar;
use crate::ir::arglist::ArgList;
use crate::ir::identifier::{Id, Namespace};
use crate::ir::macros::MacroData;
use crate::runner::macro_server::named_file_server::MacroID;
use crate::pipeline::Pipeline;
use crate::pipeline::translation_unit::TranslationUnit;
use crate::pipeline::config::ProjectConfig;
use classes::GDSCRIPT_CLASS_NAMES;

use std::collections::{HashSet, HashMap};
use std::path::PathBuf;
use std::ptr;
use std::sync::Once;

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

// fn toplevel_enum(name: &str, values: &[&str]) -> LocalVar {
//   let hint = ValueHint::enumeration(values.iter().copied());
//   let mut var = LocalVar::file_constant(name.to_owned()).with_hint(hint);
//   var.name = var.name.into_imported(GDLISP_NAME.to_owned());
//   var
// }

// fn gdlisp_function(name: &str, specs: FnSpecs) -> FnCall {
//   let mut func = FnCall::file_constant(specs, FnScope::Global, String::from(name));
//   func.object = func.object.into_imported(GDLISP_NAME.to_owned());
//   func
// }

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

/// An appropriate [`ProjectConfig`] for the `GDLisp.gd` source file.
pub fn gdlisp_project_config() -> ProjectConfig {
  ProjectConfig {
    root_directory: PathBuf::from("."),
    optimizations: true,
  }
}

fn get_stdlib() -> &'static TranslationUnit {
  // TODO This is a nasty hack that uses global variables to get what
  // I want. To be honest, I'm not fully sure how we're going to fix
  // it, but it needs to not stay this way long-term.
  static mut UNIT: *const TranslationUnit = ptr::null();
  static ONCE: Once = Once::new();

  unsafe {
    ONCE.call_once(|| {
      // We don't want to keep a Pipeline in static storage. Pipeline
      // contains a macro server child process, and if Drop doesn't
      // get called, then that process will remain even after the
      // compiler terminates. Thus, we use a Pipeline locally here,
      // get the translation unit, detach the translation unit from
      // the pipeline, and then drop the pipeline. The detach function
      // is only safe as long as the standard library doesn't define
      // any non-reserved macros. But this definition is tautological,
      // because all macros defined in the standard library are, by
      // definition, reserved macros with special behavior. Hence,
      // detaching the translation unit is tautologically safe.
      let mut pipeline = Pipeline::new(gdlisp_project_config());
      let stdlib = load_stdlib_file(&mut pipeline);
      let unit = Box::new(stdlib.clone_detached());
      UNIT = Box::into_raw(unit);
    });
    &*UNIT
  }
}

fn load_stdlib_file(pipeline: &mut Pipeline) -> &TranslationUnit {
  pipeline.load_file(&PathBuf::from("GDLisp.lisp")).expect("Error loading standard library")
}

/// Ensure that the standard library `GDLisp.lisp` has been compiled.
/// If it has not, then this function compiles it. As with other
/// `library` functions, this will panic in case of error. After this
/// function returns, a file named `GDLisp.gd` will exist in the
/// project directory.
pub fn ensure_stdlib_loaded() {
  let _ = get_stdlib();
}

/// Bind all GDLisp and GDScript built-in names to the given symbol
/// table.
///
/// This function does *not* bind built-in macros.
/// [`bind_builtin_macros`] is a separate function provided for that
/// behavior.
///
/// If `minimalist` is true, then the GDLisp standard library will not
/// be bound. Note that this does *not* make `bind_builtins` a no-op;
/// GDScript global names like top-level classes (`Reference`, `Node`,
/// etc.) will be bound regardless of `minimalist`, but the GDLisp
/// standard library will not be loaded. `minimalist` should generally
/// be false and is intended as a means of compiling the standard
/// library itself.
pub fn bind_builtins(table: &mut SymbolTable, minimalist: bool) {
  let unit =
    if minimalist {
      None
    } else {
      Some(get_stdlib())
    };
  bind_builtins_unchecked(table, unit);
}

fn bind_builtins_unchecked(table: &mut SymbolTable, unit: Option<&TranslationUnit>) {

  // (Assumes minimalist compile iff unit is None)

  // All built-in global class names
  for name in &GDSCRIPT_CLASS_NAMES {
    table.set_var((*name).to_owned(), LocalVar::superglobal((*name).to_owned()));
  }

  // TODO Do we need to bind built-in macros here? Macros should have
  // no runtime presence so that makes me think no, but at the same
  // time we do bind user-defined macros to the symbol table.

  if let Some(unit) = unit {
    for id in &unit.exports {
      match id.namespace {
        Namespace::Value => {
          let value = unit.table.get_var(&id.name).expect(&format!("Exported value name {} does not appear", id.name));
          let mut value = value.to_owned();
          value.name = value.name.into_imported(String::from("GDLisp"));
          table.set_var(id.name.to_owned(), value);
        }
        Namespace::Function => {
          let (call, magic) = unit.table.get_fn(&id.name).expect(&format!("Exported function name {} does not appear", id.name));
          let call = Compiler::translate_call(String::from("GDLisp"), call.clone());
          let magic = dyn_clone::clone_box(magic);
          table.set_fn(id.name.to_owned(), call, magic);
        }
      }
    }
  }

  // TODO The remaining constants from @GlobalScope need to be copied over here
}

/// Get a collection of all of the built-in names in GDLisp and
/// GDScript, in no particular order.
///
/// `minimalist` is passed onto [`bind_builtins`] as-is and will be
/// interpreted in the same way it is in that function.
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

// TODO Move this to MacroId as a special value.
const RESERVED_MACRO_ID: u32 = 1;

/// Bind all of the built-in GDLisp macros to the macro table given.
///
/// If the standard library has not been loaded, this function loads
/// the standard library. As such, this function should not be called
/// in the process of loading the standard library, as that will
/// result in a double lock on the stdlib mutex.
pub fn bind_builtin_macros(macros: &mut HashMap<String, MacroData>) {

  let unit = get_stdlib();
  for (name, func, _magic) in unit.table.fns() {
    if func.is_macro {
      let data = MacroData {
        id: MacroID(RESERVED_MACRO_ID),
        args: ArgList::from_specs(func.specs),
        imported: true,
      };
      macros.insert(name.to_owned(), data);
    }
  }

}
