
pub mod error;
pub mod translation_unit;
pub mod config;
pub mod loader;
pub mod resolver;

use translation_unit::TranslationUnit;
use config::ProjectConfig;
use error::Error;
use resolver::{NameResolver, DefaultNameResolver};
use crate::parser;
use crate::ir;
use crate::compile::Compiler;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::library;
use crate::gdscript::decl;
use crate::util;
use crate::runner::macro_server::named_file_server::NamedFileServer;

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::collections::HashMap;

pub struct Pipeline {
  config: ProjectConfig,
  resolver: Box<dyn NameResolver>,
  known_files: HashMap<PathBuf, TranslationUnit>,
  server: NamedFileServer,
}

impl Pipeline {

  pub fn with_resolver(config: ProjectConfig, resolver: Box<dyn NameResolver>) -> Pipeline {
    Pipeline {
      config: config,
      resolver: resolver,
      known_files: HashMap::new(),
      server: NamedFileServer::new(),
    }
  }

  pub fn new(config: ProjectConfig) -> Pipeline {
    Pipeline::with_resolver(config, Box::new(DefaultNameResolver))
  }

  pub fn compile_code<P : AsRef<Path> + ?Sized>(&mut self, filename: &P, input: &str)
                                                -> Result<TranslationUnit, Error> {
    let parser = parser::SomeASTParser::new();
    let ast = parser.parse(input)?;

    let mut compiler = Compiler::new(FreshNameGenerator::new(ast.all_symbols()));
    let mut table = SymbolTable::new();
    library::bind_builtins(&mut table);

    let ir = ir::compile_toplevel(self, &ast)?;
    let mut builder = CodeBuilder::new(decl::ClassExtends::Named("Node".to_owned()));
    compiler.compile_toplevel(self, &mut builder, &mut table, &ir)?;
    let result = builder.build();

    let exports = ir::export::get_export_list(&ir.decls);
    Ok(TranslationUnit::new(filename.as_ref().to_owned(), table, ir, result, exports))
  }

  pub fn compile_file_to_unit<P, R>(&mut self, filename: &P, input: &mut R)
                                    -> Result<TranslationUnit, Error>
  where P : AsRef<Path> + ?Sized,
        R : Read {
    let contents = util::read_to_end(input)?;
    self.compile_code(filename, &contents)
  }

  pub fn compile_file<P, R, W>(&mut self, filename: &P, input: &mut R, output: &mut W)
                            -> Result<(), Error>
  where P : AsRef<Path> + ?Sized,
        R : Read,
        W : Write {
    let result = decl::TopLevelClass::from(self.compile_file_to_unit(filename, input)?);
    write!(output, "{}", result.to_gd())?;
    Ok(())
  }

  fn load_file_unconditionally<'a, 'b, P>(&'a mut self, input_path: &'b P)
                                          -> Result<&'a TranslationUnit, Error>
  where P : AsRef<Path> + ?Sized { ///// Name translation rules (stand up a file on GDScript for each source file and make sure to translate the preloads appropriately, so we can get imports right)
    let input_path = input_path.as_ref();
    let output_path = input_to_output_filename(input_path);
    let mut input_file = self.resolver.resolve_input_path(input_path)?;
    let mut output_file = self.resolver.resolve_output_path(&output_path)?;

    let unit = self.compile_file_to_unit(input_path, &mut input_file)?;
    write!(output_file, "{}", unit.gdscript().to_gd())?;
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

}

pub fn input_to_output_filename<P : AsRef<Path> + ?Sized>(input: &P) -> PathBuf {
  let mut output_path = input.as_ref().to_owned();
  let renamed = output_path.set_extension("gd");
  assert!(renamed); // Make sure the path was actually to a file
  output_path
}
