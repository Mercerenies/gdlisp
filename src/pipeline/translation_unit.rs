
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

}

impl From<TranslationUnit> for TopLevelClass {

  fn from(trans: TranslationUnit) -> TopLevelClass {
    trans.gdscript
  }

}
