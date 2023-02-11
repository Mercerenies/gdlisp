
//! Provides the [`TranslationUnit`] structure.

use crate::compile::symbol_table::SymbolTable;
use crate::ir::decl::TopLevel;
use crate::ir::identifier::Id;
use crate::ir::macros::MacroData;
use crate::gdscript::decl::TopLevelClass;

use std::path::PathBuf;
use std::collections::HashMap;

/// A translation unit is the result of compiling a single file of
/// GDLisp code. As we compile a Godot project, the
/// [`Pipeline`](super::Pipeline) keeps an index of loaded files in
/// the form of translation units. If the same file is required
/// multiple times, the latter loads can simply query the cached file.
/// As such, this structure necessarily keeps all of the information
/// needed to reconstruct the entire relevant compilation process for
/// import purposes.
#[derive(Debug)]
pub struct TranslationUnit {
  /// The path to the file this translation unit references.
  pub filename: PathBuf,
  /// The compiler symbol table for the top-level of this translation
  /// unit. This includes all top-level declarations which are
  /// available at runtime.
  ///
  /// Note that, at present, this symbol table includes the names of
  /// macros defined in the file. This may change in the future, if
  /// macros become unavailable at runtime.
  pub table: SymbolTable,
  /// The intermediate representation used during compilation of the
  /// translation unit.
  pub ir: TopLevel,
  /// The compiled GDScript code, the final result of compilation.
  pub gdscript: TopLevelClass,
  /// A vector of exported identifiers. This includes any names which
  /// should be available from imports and explicitly *excludes* those
  /// names marked private or those generated for local use, such as
  /// lambda closure classes.
  pub exports: Vec<Id>,
  /// A map of all of the macros defined in the translation unit.
  ///
  /// This map should exclude macros which were imported into the
  /// unit's scope but were not defined there, so
  /// [`MacroData::imported`] should be false for every value in this
  /// map.
  pub macros: HashMap<Id, MacroData>,
}

impl TranslationUnit {

  /// Convenience function for constructing translation units.
  pub fn new(filename: PathBuf,
             table: SymbolTable,
             ir: TopLevel,
             gdscript: TopLevelClass,
             exports: Vec<Id>,
             macros: HashMap<Id, MacroData>)
             -> TranslationUnit {
    TranslationUnit { filename, table, ir, gdscript, exports, macros }
  }

  /// Clone the [`TranslationUnit`].
  ///
  /// `TranslationUnit` does not directly implement [`Clone`], and for
  /// good reason. The identifiers in a given translation unit (for
  /// example, those in [`TranslationUnit::macros`]) apply to a
  /// particular [`Pipeline`](super::Pipeline). Associating a
  /// translation unit with a different pipeline can result in
  /// unintended consequences. Thus, we provide this explicit
  /// mechanism for cloning a translation unit. This method should
  /// only be called if you *definitely* know what you're doing.
  ///
  /// In particular, `clone_detached` is safe on translation units
  /// which do not define any macros, or which define only macros
  /// using reserved identifiers. That means that `clone_detached` is
  /// safe to use on the translation unit describing the GDLisp
  /// standard library.
  pub fn clone_detached(&self) -> TranslationUnit {
    TranslationUnit {
      filename: self.filename.clone(),
      table: self.table.clone(),
      ir: self.ir.clone(),
      gdscript: self.gdscript.clone(),
      exports: self.exports.clone(),
      macros: self.macros.clone(),
    }
  }

}

impl From<TranslationUnit> for TopLevelClass {

  fn from(trans: TranslationUnit) -> TopLevelClass {
    trans.gdscript
  }

}
