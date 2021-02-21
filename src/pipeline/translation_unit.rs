
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::decl::TopLevelClass;

pub struct TranslationUnit {
  filename: String,
  table: SymbolTable,
  gdscript: TopLevelClass,
}

impl TranslationUnit {

  pub fn new(filename: String, table: SymbolTable, gdscript: TopLevelClass) -> TranslationUnit {
    TranslationUnit { filename, table, gdscript }
  }

  pub fn filename(&self) -> &str {
    &self.filename
  }

  pub fn table(&self) -> &SymbolTable {
    &self.table
  }

  pub fn gdscript(&self) -> &TopLevelClass {
    &self.gdscript
  }

}

impl From<TranslationUnit> for TopLevelClass {

  fn from(trans: TranslationUnit) -> TopLevelClass {
    trans.gdscript
  }

}
