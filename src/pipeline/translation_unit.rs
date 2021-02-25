
use crate::compile::symbol_table::SymbolTable;
use crate::ir::decl::TopLevel;
use crate::gdscript::decl::TopLevelClass;

use std::path::{PathBuf, Path};

pub struct TranslationUnit {
  filename: PathBuf,
  table: SymbolTable,
  ir: TopLevel,
  gdscript: TopLevelClass,
  exports: Vec<String>,
}

impl TranslationUnit {

  pub fn new(filename: PathBuf,
             table: SymbolTable,
             ir: TopLevel,
             gdscript: TopLevelClass,
             exports: Vec<String>)
             -> TranslationUnit {
    TranslationUnit { filename, table, ir, gdscript, exports }
  }

  pub fn filename(&self) -> &Path {
    &self.filename
  }

  pub fn table(&self) -> &SymbolTable {
    &self.table
  }

  pub fn ir(&self) -> &TopLevel {
    &self.ir
  }

  pub fn gdscript(&self) -> &TopLevelClass {
    &self.gdscript
  }

  pub fn exports(&self) -> &[String] {
    &self.exports
  }

}

impl From<TranslationUnit> for TopLevelClass {

  fn from(trans: TranslationUnit) -> TopLevelClass {
    trans.gdscript
  }

}
