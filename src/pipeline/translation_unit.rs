
use crate::compile::symbol_table::SymbolTable;
use crate::ir::decl::TopLevel;
use crate::ir::identifier::Id;
use crate::ir::arglist::ArgList;
use crate::gdscript::decl::TopLevelClass;
use crate::runner::macro_server::named_file_server::MacroID;

use std::path::PathBuf;
use std::collections::HashMap;

pub struct TranslationUnit {
  pub filename: PathBuf,
  pub table: SymbolTable,
  pub ir: TopLevel,
  pub gdscript: TopLevelClass,
  pub exports: Vec<Id>,
  pub macros: HashMap<String, (MacroID, ArgList)>,
}

impl TranslationUnit {

  pub fn new(filename: PathBuf,
             table: SymbolTable,
             ir: TopLevel,
             gdscript: TopLevelClass,
             exports: Vec<Id>,
             macros: HashMap<String, (MacroID, ArgList)>)
             -> TranslationUnit {
    TranslationUnit { filename, table, ir, gdscript, exports, macros }
  }

}

impl From<TranslationUnit> for TopLevelClass {

  fn from(trans: TranslationUnit) -> TopLevelClass {
    trans.gdscript
  }

}
