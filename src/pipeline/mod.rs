
pub mod error;
pub mod translation_unit;
pub mod config;
pub mod loader;
pub mod resolver;
pub mod can_load;
pub mod source;

use translation_unit::TranslationUnit;
use config::ProjectConfig;
use error::{Error, IOError};
use source::SourceOffset;
use resolver::{NameResolver, DefaultNameResolver};
use crate::parser;
use crate::ir;
use crate::compile::Compiler;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::preload_resolver::{DefaultPreloadResolver, LookupPreloadResolver};
use crate::gdscript::library;
use crate::gdscript::decl;
use crate::util;
use crate::runner::macro_server::named_file_server::NamedFileServer;
use crate::runner::path::{RPathBuf, PathSrc};
use crate::optimize::gdscript::run_standard_passes;

use tempfile::Builder;

use std::io::Write;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::mem;

pub struct Pipeline {
  config: ProjectConfig,
  resolver: Box<dyn NameResolver>,
  known_files: HashMap<PathBuf, TranslationUnit>,
  known_files_paths: HashMap<PathBuf, PathBuf>,
  server: NamedFileServer,
  current_file_path: Option<RPathBuf>,
}

impl Pipeline {

  pub fn with_resolver(config: ProjectConfig, resolver: Box<dyn NameResolver>) -> Pipeline {
    Pipeline {
      config: config,
      resolver: resolver,
      known_files: HashMap::new(),
      known_files_paths: HashMap::new(),
      server: NamedFileServer::new(),
      current_file_path: None,
    }
  }

  pub fn new(config: ProjectConfig) -> Pipeline {
    Pipeline::with_resolver(config, Box::new(DefaultNameResolver))
  }

  pub fn compile_code<P : AsRef<Path> + ?Sized>(&mut self, filename: &P, input: &str)
                                                -> Result<TranslationUnit, Error> {

    let file_path = filename.as_ref().strip_prefix(&self.config.root_directory).expect("Non-local file load detected").to_owned(); // TODO Expect
    let mut old_file_path = Some(RPathBuf::new(PathSrc::Res, file_path).expect("Non-local file load detected")); // TODO Expect
    mem::swap(&mut old_file_path, &mut self.current_file_path);

    let parser = parser::SomeASTParser::new();
    let ast = parser.parse(input)?;

    let mut compiler = Compiler::new(FreshNameGenerator::new(ast.all_symbols()), Box::new(DefaultPreloadResolver));

    let (ir, macros) = ir::compile_toplevel(self, &ast)?;

    let mut table = SymbolTable::new();
    library::bind_builtins(&mut table, ir.minimalist_flag);

    let mut builder = CodeBuilder::new(decl::ClassExtends::named("Node".to_owned()));
    compiler.compile_toplevel(self, &mut builder, &mut table, &ir)?;
    let mut result = builder.build();
    if self.config.optimizations {
      run_standard_passes(&mut result)?;
    }

    mem::swap(&mut old_file_path, &mut self.current_file_path);

    let exports = ir::export::get_export_list(&ir.decls);
    Ok(TranslationUnit::new(filename.as_ref().to_owned(), table, ir, result, exports, macros))
  }

  fn load_file_unconditionally<'a, 'b, P>(&'a mut self, input_path: &'b P)
                                          -> Result<&'a TranslationUnit, Error>
  where P : AsRef<Path> + ?Sized {
    let input_path = input_path.as_ref();
    let output_path = input_to_output_filename(input_path);
    let mut input_file = self.resolver.resolve_input_path(input_path).map_err(|err| IOError::new(err, SourceOffset(0)))?;
    let mut output_file = self.resolver.resolve_output_path(&output_path).map_err(|err| IOError::new(err, SourceOffset(0)))?;

    let contents = util::read_to_end(&mut input_file).map_err(|err| IOError::new(err, SourceOffset(0)))?;
    let unit = self.compile_code(&input_path, &contents)?;
    write!(output_file, "{}", unit.gdscript.to_gd()).map_err(|err| IOError::new(err, SourceOffset(0)))?;

    let file_path = input_path.strip_prefix(&self.config.root_directory).expect("Non-local file load detected").to_owned(); // TODO Expect
    let mut old_file_path = Some(RPathBuf::new(PathSrc::Res, file_path).expect("Non-local file load detected")); // TODO Expect
    mem::swap(&mut old_file_path, &mut self.current_file_path);

    // Also output to a temporary file
    let mut tmpfile = Builder::new()
      .prefix("__gdlisp_file")
      .suffix(".gd")
      .rand_bytes(5)
      .tempfile()
      .map_err(|err| IOError::new(err, SourceOffset(0)))?;

    let mut input_path_store_name = input_path.strip_prefix(&self.config.root_directory).expect("Non-local file load detected").to_owned(); // TODO Expect
    input_path_store_name.set_extension("gd");
    self.known_files_paths.insert(input_path_store_name, tmpfile.path().to_owned());

    let parser = parser::SomeASTParser::new();
    let ast = parser.parse(&contents)?;
    let resolver = self.make_preload_resolver();
    let mut compiler = Compiler::new(FreshNameGenerator::new(ast.all_symbols()), Box::new(resolver));
    let mut table = SymbolTable::new();

    library::bind_builtins(&mut table, unit.ir.minimalist_flag);

    let mut builder = CodeBuilder::new(decl::ClassExtends::named("Node".to_owned()));
    compiler.compile_toplevel(self, &mut builder, &mut table, &unit.ir)?;
    let mut tmpresult = builder.build();
    if self.config.optimizations {
      run_standard_passes(&mut tmpresult)?;
    }

    if !unit.ir.minimalist_flag {
      write!(tmpfile, "{}", tmpresult.to_gd()).map_err(|err| IOError::new(err, SourceOffset(0)))?;
      tmpfile.flush().map_err(|err| IOError::new(err, SourceOffset(0)))?;
      self.server.stand_up_file(tmpfile).map_err(|err| IOError::new(err, SourceOffset(0)))?;
    }

    mem::swap(&mut old_file_path, &mut self.current_file_path);

    self.known_files.insert(input_path.to_owned(), unit);
    Ok(self.known_files.get(input_path).expect("Path not present in load_file"))
  }

  pub fn get_loaded_file<'a, 'b, P>(&'a self, input_path: &'b P)
                                    -> Option<&'a TranslationUnit>
  where P : AsRef<Path> + ?Sized {
    self.known_files.get(input_path.as_ref())
  }

  fn to_absolute_path<P>(&self, input_path: &P) -> PathBuf
  where P : AsRef<Path> + ?Sized {
    let input_path = input_path.as_ref();
    if input_path.is_absolute() {
      input_path.to_owned()
    } else {
      self.config.root_directory.join(input_path)
    }
  }

  pub fn load_file<'a, 'b, P>(&'a mut self, input_path: &'b P)
                              -> Result<&'a TranslationUnit, Error>
  where P : AsRef<Path> + ?Sized {
    let input_path = self.to_absolute_path(input_path);
    // if-let here causes Rust to complain due to lifetime rules, so
    // we use contains_key instead.
    if self.known_files.contains_key(&input_path) {
      Ok(self.get_loaded_file(&input_path).unwrap())
    } else {
      self.load_file_unconditionally(&input_path)
    }
  }

  pub fn get_file<'a, 'b, P>(&'a self, input_path: &'b P) -> Option<&'a TranslationUnit>
  where P :AsRef<Path> + ?Sized {
    self.get_loaded_file(input_path)
  }

  pub fn get_server(&self) -> &NamedFileServer {
    &self.server
  }

  pub fn get_server_mut(&mut self) -> &mut NamedFileServer {
    &mut self.server
  }

  pub fn make_preload_resolver(&self) -> LookupPreloadResolver {
    LookupPreloadResolver(self.known_files_paths.clone())
  }

  pub fn currently_loading_file(&self) -> Option<&RPathBuf> {
    self.current_file_path.as_ref()
  }

  // This is done automatically if you use any of the built-in compile
  // functions above. You can do it yourself manually with this
  // function for testing purposes.
  pub fn set_currently_loading_file(&mut self, path: RPathBuf) {
    self.current_file_path = Some(path);
  }

  pub fn config(&self) -> &ProjectConfig {
    &self.config
  }

}

pub fn input_to_output_filename<P : AsRef<Path> + ?Sized>(input: &P) -> PathBuf {
  let mut output_path = input.as_ref().to_owned();
  let renamed = output_path.set_extension("gd");
  assert!(renamed); // Make sure the path was actually to a file
  output_path
}
