
use crate::compile::Compiler;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::decl;
use crate::gdscript::library;
use crate::runner::into_gd_file::IntoGDFile;
use crate::pipeline::error::{Error as PError};

use tempfile::{NamedTempFile, Builder};

use std::io::{self, Write};

type IRSymbolTable = super::symbol_table::SymbolTable;

use std::collections::HashSet;

fn make_tmp() -> io::Result<NamedTempFile> {
  Builder::new()
    .prefix("__gdlisp_macro")
    .suffix(".gd")
    .rand_bytes(5)
    .tempfile()
}

pub fn create_macro_file(src_table: &IRSymbolTable, names: HashSet<String>) -> Result<NamedTempFile, PError> {
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table);

  let mut compiler = Compiler::new(FreshNameGenerator::new(vec!()));
  let decls = Vec::from(src_table.filter(|d| names.contains(d.name())));

  let mut builder = CodeBuilder::new(decl::ClassExtends::Named("Node".to_owned()));
  compiler.compile_decls(&mut builder, &mut table, &decls)?; ///// Deal with imports
  let result = builder.build();

  // TODO Handle the error correctly
  let mut tmp_file = make_tmp().expect("IO Error");
  result.write_to_gd(&mut tmp_file).expect("IO Error");
  tmp_file.flush().expect("IO Error");
  Ok(tmp_file)

}
