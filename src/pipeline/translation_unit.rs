
use crate::compile::symbol_table::SymbolTable;

pub struct TranslationUnit {
  filename: String,
  table: SymbolTable,
}

impl TranslationUnit {

  pub fn new(filename: String, table: SymbolTable) -> TranslationUnit {
    TranslationUnit { filename, table }
  }

  pub fn filename(&self) -> &str {
    &self.filename
  }

  pub fn table(&self) -> &SymbolTable {
    &self.table
  }

}
