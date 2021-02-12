
// Incremental compilation (supplies backbone for macro resolution)

use super::symbol_table::SymbolTable;

pub struct IncCompiler {
  symbols: SymbolTable,
}

impl IncCompiler {

  pub fn new() -> IncCompiler {
    IncCompiler { symbols: SymbolTable::new() }
  }

}

impl Default for IncCompiler {

  fn default() -> IncCompiler {
    IncCompiler::new()
  }

}
