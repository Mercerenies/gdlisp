
use crate::compile::Compiler;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::error::Error;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::decl;
use crate::gdscript::library;

type IRSymbolTable = super::symbol_table::SymbolTable;

use std::collections::HashSet;

pub fn create_macro_file(src_table: &IRSymbolTable, names: HashSet<String>) -> Result<(), Error> {
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table);

  let mut compiler = Compiler::new(FreshNameGenerator::new(vec!()));
  let decls = Vec::from(src_table.filter(|d| names.contains(d.name())));

  let mut builder = CodeBuilder::new(decl::ClassExtends::Named("Node".to_owned()));
  compiler.compile_decls(&mut builder, &table, &decls)?;
  let _result = builder.build(); /////
  //println!("{}", result.to_gd());
  Ok(())
}
