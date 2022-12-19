
//! Helpers for using GDScript object and script metadata.
//!
//! Every object in Godot has metadata fields that can be modified.
//! These fields exist independently of the usual instance variables
//! on an object and can even be defined on built-in class instances
//! such as `GDScript` objects themselves. GDLisp uses this capability
//! to define various data needed for more advanced languages
//! features.
//!
//! This module provides helpers to read, write, and check whether
//! metadata exists. It also provides naming conventions for managing
//! the names of GDLisp-useful metadata, to avoid conflicts. All
//! GDLisp metadata keys will always begin with the prefix `__gdlisp`
//! followed by some more specific string. The specific strings used
//! are implementation details and should not be relied upon in GDLisp
//! code.

use super::expr::Expr;
use crate::pipeline::source::SourceOffset;

/// The prefix applied to all GDLisp metadata.
pub const PREFIX: &str = "__gdlisp";

/// The metadata field that exists (and is truthy) on all cons cells
/// in GDLisp.
pub const CONS_META: &str = "__gdlisp_Primitive_Cons";

/// The metadata field that exists (and is truthy) on all symbol
/// values in GDLisp.
pub const SYMBOL_META: &str = "__gdlisp_Primitive_Symbol";

/// The name of the global function we define in a REPL file to
/// indicate the REPL code to be run.
pub const REPL_FUNCTION_NAME: &str = "__gdlisp_Repl_runner";

/// Given a GDScript function name, prefix it appropriately for a
/// symbol macro with the given name.
pub fn symbol_macro(name: &str) -> String {
  format!("{}_SymbolMacroFunction_{}", PREFIX, name)
}

/// Given a GDLisp lazy val, this is the name of the metadata with
/// suffix `name`.
///
/// Note that, under the current implementation, the `deflazy` macro
/// does *not* use the GDScript name but rather a gensym to store the
/// metadata. This behavior may be changed in the future.
pub fn lazy(name: &str) -> String {
  format!("{}_Lazy_{}", PREFIX, name)
}

/// A `MetadataCompiler` is used as a helper for compiling calls to
/// GDScript metadata functions such as `get_meta`, `has_meta`, and
/// `set_meta`.
#[derive(Clone, Debug)]
pub struct MetadataCompiler {
  object_ref: Expr,
}

impl MetadataCompiler {

  /// A new `MetadataCompiler` which will compile to metadata calls on
  /// the given expression.
  pub fn new(expr: Expr) -> MetadataCompiler {
    MetadataCompiler { object_ref: expr }
  }

  fn get_pos(&self) -> SourceOffset {
    self.object_ref.pos
  }

  /// Compiles to a `get_meta` GDScript call.
  pub fn get_meta(&self, meta_name: &str) -> Expr {
    let meta_name_expr = Expr::from_value(meta_name, self.get_pos());
    Expr::call(Some(self.object_ref.clone()), "get_meta", vec!(meta_name_expr), self.get_pos())
  }

  /// Compiles to a `set_meta` GDScript call.
  pub fn set_meta(&self, meta_name: &str, value: Expr) -> Expr {
    let meta_name_expr = Expr::from_value(meta_name, self.get_pos());
    Expr::call(Some(self.object_ref.clone()), "set_meta", vec!(meta_name_expr, value), self.get_pos())
  }

  /// Compiles to a `has_meta` GDScript call.
  pub fn has_meta(&self, meta_name: &str) -> Expr {
    let meta_name_expr = Expr::from_value(meta_name, self.get_pos());
    Expr::call(Some(self.object_ref.clone()), "has_meta", vec!(meta_name_expr), self.get_pos())
  }

  /// Compiles to a `remove_meta` GDScript call.
  pub fn remove_meta(&self, meta_name: &str) -> Expr {
    let meta_name_expr = Expr::from_value(meta_name, self.get_pos());
    Expr::call(Some(self.object_ref.clone()), "remove_meta", vec!(meta_name_expr), self.get_pos())
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::expr::ExprF;

  #[test]
  fn naming_test() {
    assert_eq!(lazy("ABC"), "__gdlisp_Lazy_ABC");
    assert_eq!(lazy("Foobar"), "__gdlisp_Lazy_Foobar");
  }

  #[test]
  fn compiler_test() {
    let expr = Expr::var("reference", SourceOffset(0));
    let value_expr = Expr::from_value(100, SourceOffset(0));
    let compiler = MetadataCompiler::new(expr);

    // Basic compilation
    assert_eq!(compiler.get_meta("foobar").to_gd(), "reference.get_meta(\"foobar\")");
    assert_eq!(compiler.has_meta("foobar").to_gd(), "reference.has_meta(\"foobar\")");
    assert_eq!(compiler.remove_meta("foobar").to_gd(), "reference.remove_meta(\"foobar\")");
    assert_eq!(compiler.set_meta("foobar", value_expr.clone()).to_gd(), "reference.set_meta(\"foobar\", 100)");

  }

  #[test]
  fn compiler_offset_test() {
    let expr = Expr::var("reference", SourceOffset(42));
    let value_expr = Expr::from_value(100, SourceOffset(10));
    let compiler = MetadataCompiler::new(expr);

    // Check that the generated source offsets are correct
    let result_expr = compiler.set_meta("foobar", value_expr);
    assert_eq!(result_expr.pos, SourceOffset(42));
    match result_expr {
      Expr { value: ExprF::Call(Some(lhs), _, args), pos: _ } => {
        assert_eq!(lhs.pos, SourceOffset(42));
        assert_eq!(args.len(), 2);
        assert_eq!(args[0].pos, SourceOffset(42));
        assert_eq!(args[1].pos, SourceOffset(10));
      }
      _ => {
        panic!("Wrong shape of result, got {:?}", result_expr);
      }
    }
  }

}
