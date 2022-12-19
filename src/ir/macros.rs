
use crate::compile::Compiler;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::body::class_scope::OutsideOfClass;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::function_call::FnSpecs;
use crate::gdscript::library;
use crate::gdscript::class_extends::ClassExtends;
use crate::runner::into_gd_file::IntoGDFile;
use crate::runner::macro_server::named_file_server::MacroID;
use crate::pipeline::error::{PError, IOError};
use crate::pipeline::Pipeline;
use crate::pipeline::can_load::CanLoad;
use crate::pipeline::source::SourceOffset;
use crate::ir;
use crate::ir::import::ImportDecl;
use crate::ir::decl::TopLevel;
use crate::ir::identifier::Id;
use crate::ir::declaration_table::DeclarationTable;

use tempfile::{NamedTempFile, Builder};
use serde::{Serialize, Deserialize};

use std::io::{self, Write};
use std::collections::HashSet;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MacroData {
  pub id: MacroID,
  pub specs: FnSpecs,
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

pub fn create_macro_file(pipeline: &mut Pipeline, imports: Vec<ImportDecl>, src_table: &DeclarationTable, names: HashSet<Id>, existing_symbols: &SymbolTable, pos: SourceOffset, minimalist: bool) -> Result<NamedTempFile, PError> {
  let mut table = existing_symbols.clone();
  library::bind_builtins(&mut table, minimalist);

  let current_filename = pipeline.current_filename();
  let mut tmp_file = make_tmp().map_err(|err| IOError::new(err, pos))?;
  let mut resolver = pipeline.make_preload_resolver();
  // Replace the current file name with the macro file name.
  resolver.insert(current_filename.into_path(), tmp_file.path().to_owned());

  let mut compiler = Compiler::new(FreshNameGenerator::new(vec!()), Box::new(resolver), minimalist);
  let decls = Vec::from(src_table.filter(|d| names.contains(&*d.id_like())));
  let toplevel = {
    let mut toplevel = TopLevel { imports, decls, minimalist_flag: minimalist };
    // Strip main class qualifier; we don't need or want it during macro expansion.
    for d in &mut toplevel.decls {
      if let ir::decl::DeclF::ClassDecl(cdecl) = &mut d.value {
        cdecl.main_class = false;
      }
    }
    toplevel
  };

  let mut builder = CodeBuilder::new(ClassExtends::SimpleIdentifier("Node".to_owned()));
  compiler.frame(pipeline, &mut builder, &mut table, &mut OutsideOfClass).compile_toplevel(&toplevel)?;
  let result = builder.build();

  result.write_to_gd(&mut tmp_file).map_err(|err| IOError::new(err, pos))?;
  tmp_file.flush().map_err(|err| IOError::new(err, pos))?;
  Ok(tmp_file)

}
