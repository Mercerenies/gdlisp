
// Some infrastructure for spoofing files to test import syntax.

use gdlisp::pipeline::translation_unit::TranslationUnit;
use gdlisp::pipeline::loader::FileLoader;
use gdlisp::pipeline::error::{Error as PError};
use gdlisp::gdscript::library;
use gdlisp::gdscript::decl;
use gdlisp::ir;
use gdlisp::parser;
use gdlisp::compile::Compiler;
use gdlisp::compile::names::fresh::FreshNameGenerator;
use gdlisp::compile::symbol_table::SymbolTable;
use gdlisp::compile::body::builder::CodeBuilder;

use std::collections::HashMap;
use std::path::Path;

#[derive(Default)]
pub struct MockFileLoader {
  files: HashMap<String, String>,
  loaded_files: HashMap<String, TranslationUnit>,
}

impl MockFileLoader {

  pub fn new() -> MockFileLoader {
    MockFileLoader::default()
  }

  pub fn add_file(&mut self, name: &str, contents: &str) {
    self.files.insert(name.to_owned(), contents.to_owned());
  }

}

impl FileLoader for MockFileLoader {
  // Doesn't actually throw any error (it panics on errors, since it's
  // exclusively used for testing), but the PError bound is required
  // by the contract for the compile phases.
  type Error = PError;

  fn load_file<'a, 'b, P>(&'a mut self, input_path: &'b P)
                          -> Result<&'a TranslationUnit, Self::Error>
  where P : AsRef<Path> + ?Sized {
    let input_path_str = input_path.as_ref().to_string_lossy().to_owned();

    if self.loaded_files.contains_key(&*input_path_str) {
      Ok(self.loaded_files.get(&*input_path_str).unwrap())
    } else {
      let contents = self.files.get(&*input_path_str).unwrap();

      let parser = parser::SomeASTParser::new();
      let ast = parser.parse(contents).unwrap();

      let mut compiler = Compiler::new(FreshNameGenerator::new(ast.all_symbols()));
      let mut table = SymbolTable::new();
      library::bind_builtins(&mut table);

      let ir = ir::compile_toplevel(self, &ast).unwrap();
      let mut builder = CodeBuilder::new(decl::ClassExtends::Named("Node".to_owned()));
      compiler.compile_toplevel(self, &mut builder, &mut table, &ir).unwrap();
      let result = builder.build();

      let exports = ir::export::get_export_list(&ir.decls);
      let unit = TranslationUnit::new(input_path.as_ref().to_owned(), table, ir, result, exports);
      self.loaded_files.insert(input_path_str.clone().into_owned(), unit);
      Ok(self.loaded_files.get(&*input_path_str).unwrap())
    }
  }

  fn get_file<'a, 'b, P>(&'a self, input_path: &'b P) -> Option<&'a TranslationUnit>
  where P :AsRef<Path> + ?Sized {
    let input_path_str = input_path.as_ref().to_string_lossy();
    self.loaded_files.get(&*input_path_str)
  }

}
