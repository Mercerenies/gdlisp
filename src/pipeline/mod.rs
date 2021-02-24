
pub mod error;
pub mod translation_unit;
pub mod config;
pub mod loader;

use translation_unit::TranslationUnit;
use config::ProjectConfig;
use error::Error;
use loader::FileLoader;
use crate::parser;
use crate::ir;
use crate::compile::Compiler;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::library;
use crate::gdscript::decl;

use std::io::{self, Read, Write};
use std::fs;
use std::path::{Path, PathBuf};
use std::collections::HashMap;

pub struct Pipeline {
  config: ProjectConfig,
  known_files: HashMap<PathBuf, TranslationUnit>,
}

impl Pipeline {

  pub fn new(config: ProjectConfig) -> Pipeline {
    Pipeline { config: config, known_files: HashMap::new() }
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
    compiler.compile_decls(&mut builder, &mut table, &ir)?;
    let result = builder.build();

    Ok(TranslationUnit::new(filename.as_ref().to_owned(), table, result))
  }

  pub fn compile_file_to_unit<P, R>(&mut self, filename: &P, input: &mut R)
                                    -> Result<TranslationUnit, Error>
  where P : AsRef<Path> + ?Sized,
        R : Read {
    let contents = read_to_end(input)?;
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
  where P : AsRef<Path> + ?Sized {
    let input_path = input_path.as_ref();
    let output_path = input_to_output_filename(input_path);
    let mut input_file = io::BufReader::new(fs::File::open(input_path)?);
    let mut output_file = io::BufWriter::new(fs::File::create(output_path)?);

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

}

impl FileLoader for Pipeline {
  type Error = Error;

  fn load_file<'a, 'b, P>(&'a mut self, input_path: &'b P)
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

}

pub fn input_to_output_filename<P : AsRef<Path> + ?Sized>(input: &P) -> PathBuf {
  let mut output_path = input.as_ref().to_owned();
  let renamed = output_path.set_extension("gd");
  assert!(renamed); // Make sure the path was actually to a file
  output_path
}

fn read_to_end(input: &mut impl Read) -> io::Result<String> {
  let mut vec = Vec::new();
  input.read_to_end(&mut vec)?;
  String::from_utf8(vec).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}
