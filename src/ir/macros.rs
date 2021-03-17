
use crate::compile::Compiler;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::decl;
use crate::gdscript::library;
use crate::runner::into_gd_file::IntoGDFile;
use crate::runner::macro_server::named_file_server::MacroID;
use crate::pipeline::error::{Error as PError};
use crate::pipeline::Pipeline;
use crate::ir;
use crate::ir::import::ImportDecl;
use crate::ir::decl::TopLevel;
use crate::ir::identifier::Id;
use crate::ir::arglist::ArgList;

use tempfile::{NamedTempFile, Builder};

use std::io::{self, Write};
use std::collections::HashSet;

type IRSymbolTable = super::symbol_table::SymbolTable;

#[derive(Clone, Debug)]
pub struct MacroData {
  pub id: MacroID,
  pub args: ArgList,
  pub imported: bool,
}

fn make_tmp() -> io::Result<NamedTempFile> {
  Builder::new()
    .prefix("__gdlisp_macro")
    .suffix(".gd")
    .rand_bytes(5)
    .tempfile()
}

impl MacroData {

  pub fn to_imported(&self) -> MacroData {
    let mut result = self.clone();
    result.imported = true;
    result
  }

}

pub fn create_macro_file(pipeline: &mut Pipeline, imports: Vec<ImportDecl>, src_table: &IRSymbolTable, names: HashSet<Id>) -> Result<NamedTempFile, PError> {
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table);

  let mut compiler = Compiler::new(FreshNameGenerator::new(vec!()), Box::new(pipeline.make_preload_resolver()));
  let decls = Vec::from(src_table.filter(|d| names.contains(&*d.id_like())));
  let toplevel = {
    let mut toplevel = TopLevel { imports, decls };
    // Strip main class qualifier; we don't need or want it during macro expansion.
    for d in &mut toplevel.decls {
      if let ir::decl::Decl::ClassDecl(cdecl) = d {
        cdecl.main_class = false;
      }
    }
    toplevel
  };

  let mut builder = CodeBuilder::new(decl::ClassExtends::named("Node".to_owned()));
  compiler.compile_toplevel(pipeline, &mut builder, &mut table, &toplevel)?;
  let result = builder.build();

  // TODO Handle the error correctly
  let mut tmp_file = make_tmp().expect("IO Error");
  result.write_to_gd(&mut tmp_file).expect("IO Error");
  tmp_file.flush().expect("IO Error");
  Ok(tmp_file)

}
