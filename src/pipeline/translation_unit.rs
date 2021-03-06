
use crate::compile::symbol_table::SymbolTable;
use crate::ir::decl::{TopLevel, MacroDecl};
use crate::gdscript::decl::TopLevelClass;
use crate::runner::macro_server::named_file_server::MacroID;

use std::path::{PathBuf, Path};
use std::collections::HashMap;

// TODO Why are all of these private? I give basically free access to them all below.
pub struct TranslationUnit {
  filename: PathBuf,
  table: SymbolTable,
  ir: TopLevel,
  gdscript: TopLevelClass,
  exports: Vec<String>,
  macros: HashMap<String, (MacroID, MacroDecl)>,
}

impl TranslationUnit {

  pub fn new(filename: PathBuf,
             table: SymbolTable,
             ir: TopLevel,
             gdscript: TopLevelClass,
             exports: Vec<String>,
             macros: HashMap<String, (MacroID, MacroDecl)>)
             -> TranslationUnit {
    TranslationUnit { filename, table, ir, gdscript, exports, macros }
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

  pub fn macros(&self) -> &HashMap<String, (MacroID, MacroDecl)> {
    &self.macros
  }

}

impl From<TranslationUnit> for TopLevelClass {

  fn from(trans: TranslationUnit) -> TopLevelClass {
    trans.gdscript
  }

}
