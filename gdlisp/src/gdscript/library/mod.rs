// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! Convenient access to the builtins in `GDLisp.gd`.
//!
//! The most commonly-used functions from this module are
//! [`bind_builtins`], [`bind_builtin_macros`], and
//! [`all_builtin_names`], though the module also provides several
//! simpler helper functions for commonly-used GDLisp builtins.

pub mod cell;
pub mod class_loader;
pub mod constant_loader;
pub mod gdnative;
pub mod magic;

use super::expr::{Expr, ExprF};
use crate::compile::Compiler;
use crate::compile::names;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::VarName;
use crate::ir::identifier::{Id, Namespace};
use crate::ir::macros::MacroData;
use crate::runner::version::{VersionInfo, get_godot_version};
use crate::pipeline::Pipeline;
use crate::pipeline::translation_unit::TranslationUnit;
use crate::pipeline::stdlib_unit::StdlibUnit;
use crate::pipeline::config::ProjectConfig;
use crate::pipeline::source::{SourceOffset, SourcedValue};

use rmp_serde::{encode, decode};

use std::collections::{HashSet, HashMap};
use std::path::PathBuf;
use std::fs::File;
use std::env::current_exe;

/// The name of the top-level GDLisp singleton object.
pub const GDLISP_NAME: &str = "GDLisp";

/// The name of a GDScript constructor.
pub const CONSTRUCTOR_NAME: &str = "_init";

/// The name of a GDScript _ready function which runs when a node is
/// added to the scene tree.
pub const READY_NAME: &str = "_ready";

/// The name of the default superclass, when one is not specified.
pub const REFERENCE_NAME: &str = "Reference";

/// An expression which accesses the global GDLisp singleton object.
pub fn gdlisp_root(pos: SourceOffset) -> Expr {
  Expr::var(GDLISP_NAME, pos)
}

/// A variable name which accesses the global GDLisp singleton object.
pub fn gdlisp_root_var_name() -> VarName {
  VarName::Superglobal(GDLISP_NAME.to_owned())
}

/// An expression which accesses a specific field on [`gdlisp_root`].
pub fn on_gdlisp_root(name: String, pos: SourceOffset) -> Expr {
  Expr::new(ExprF::Attribute(Box::new(gdlisp_root(pos)), name), pos)
}

/// Given a vector of expressions `vec`, produce an expression which
/// produces a GDLisp list containing those expressions in order.
pub fn construct_list(vec: Vec<Expr>, pos: SourceOffset) -> Expr {
  vec.into_iter().rev().fold(Expr::null(pos), |rest, first| {
    Expr::call(Some(gdlisp_root(pos)), "cons", vec!(first, rest), pos)
  })
}

/// An appropriate [`ProjectConfig`] for the `GDLisp.gd` source file.
pub fn gdlisp_project_config(godot_version: VersionInfo) -> ProjectConfig {
  ProjectConfig {
    root_directory: PathBuf::from("."),
    optimizations: true,
    godot_version: godot_version,
  }
}

fn get_stdlib() -> StdlibUnit {
  let exe_path = current_exe().expect("Could not locate current executable");
  let exe_dir = exe_path.parent().expect("Could not locate executable path");
  let file = File::open(exe_dir.join("GDLisp.msgpack")).expect("I/O error reading GDLisp.msgpack (this file should have been built as part of the GDLisp build process; are you sure you have compiled GDLisp fully?)");
  decode::from_read(file).expect("Error deserializing stdlib (this is likely a GDLisp error and should be reported as a bug)")
}

pub fn load_stdlib_to_file() -> StdlibUnit {
  let godot_version = get_godot_version().expect("I/O error loading stdlib");
  let mut pipeline = Pipeline::new(gdlisp_project_config(godot_version));
  let unit = load_stdlib_file(&mut pipeline);
  let stdlib = StdlibUnit::from(unit.clone_detached());
  let mut file = File::create("GDLisp.msgpack").expect("I/O error loading stdlib (does GDLisp have write permissions for this folder?)");
  encode::write(&mut file, &stdlib).expect("Error during serialization of stdlib");
  stdlib
}

fn load_stdlib_file(pipeline: &mut Pipeline) -> &TranslationUnit {
  match pipeline.load_file(&PathBuf::from("GDLisp.lisp"), SourceOffset(0)) {
    Ok(unit) => unit,
    Err(err) => {
      // This is basically .expect() but with a much better error
      // message.
      let err = SourcedValue::from_file(&err, "GDLisp.lisp").expect("Error loading stdlib (failed to get further diagnostic information)");
      panic!("Error loading stdlib: {}", err);
    }
  }
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
  bind_builtins_unchecked(table, unit.as_ref());
}

fn bind_builtins_unchecked(table: &mut SymbolTable, unit: Option<&StdlibUnit>) {

  // (Assumes minimalist compile iff unit is None)

  // TODO Do we need to bind built-in macros here? Macros should have
  // no runtime presence so that makes me think no, but at the same
  // time we do bind user-defined macros to the symbol table.

  if let Some(unit) = unit {
    for id in &unit.exports {
      match id.namespace {
        Namespace::Value => {
          let value = unit.table.get_var(&id.name).unwrap_or_else(|| panic!("Exported value name {} does not appear", id.name));
          let mut value = value.to_owned();
          value.name = value.name.into_imported(String::from("GDLisp"));
          table.set_var(id.name.to_owned(), value);
        }
        Namespace::Function => {
          let (call, magic) = unit.table.get_fn(&id.name).unwrap_or_else(|| panic!("Exported function name {} does not appear", id.name));
          let call = Compiler::translate_call(String::from("GDLisp"), call.clone());
          let magic = magic.clone();
          table.set_fn(id.name.to_owned(), call, magic);
        }
      }
    }
  }

}

/// Get a collection of all of the built-in names in GDLisp and
/// GDScript, in no particular order.
///
/// `minimalist` is passed onto [`bind_builtins`] as-is and will be
/// interpreted in the same way it is in that function.
pub fn all_builtin_names(minimalist: bool) -> HashSet<Id> {
  // bind_builtins is the single source of truth for built-in names.
  // So if we want them in a HashSet, we should let bind_builtins
  // build a symbol table, and then we should convert that into a
  // HashSet.
  let mut table = SymbolTable::new();
  bind_builtins(&mut table, minimalist);
  table.into_keys().collect()
}

/// Bind all of the built-in GDLisp macros and symbol macros to the
/// macro table given.
///
/// If the standard library has not been loaded, this function loads
/// the standard library. As such, this function should not be called
/// in the process of loading the standard library, as that will
/// result in a double lock on the stdlib mutex.
pub fn bind_builtin_macros(macros: &mut HashMap<Id, MacroData>,
                           pipeline: &mut Pipeline) {
  let unit = get_stdlib();

  for name in &unit.exports {
    if let Some(data) = unit.macros.get(name) {
      macros.insert(name.to_owned(), data.to_imported());
      pipeline.get_server_mut().add_reserved_macro(names::lisp_to_gd(&name.name));
    }
  }

}
