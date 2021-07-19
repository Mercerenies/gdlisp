
use crate::compile::symbol_table::SymbolTable;
use crate::ir::decl::TopLevel;
use crate::ir::identifier::Id;
use crate::ir::macros::MacroData;
use crate::gdscript::decl::TopLevelClass;

use std::path::PathBuf;
use std::collections::HashMap;

pub struct TranslationUnit {
  pub filename: PathBuf,
  pub table: SymbolTable,
  pub ir: TopLevel,
  pub gdscript: TopLevelClass,
  pub exports: Vec<Id>,
  pub macros: HashMap<String, MacroData>,
}

impl TranslationUnit {

  pub fn new(filename: PathBuf,
             table: SymbolTable,
             ir: TopLevel,
             gdscript: TopLevelClass,
             exports: Vec<Id>,
             macros: HashMap<String, MacroData>)
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
