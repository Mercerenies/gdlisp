
pub mod error;

use crate::parser;
use crate::ir;
use crate::compile::Compiler;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::body::builder::CodeBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::library;
use crate::gdscript::decl;

use std::io::{self, Read, Write};
use error::Error;

pub fn compile_file<R, W>(input: &mut R, output: &mut W) -> Result<(), Error>
where R : Read,
      W : Write {

  let contents = read_to_end(input)?;
  let parser = parser::SomeASTParser::new();
  let ast = parser.parse(&contents)?;

  let mut compiler = Compiler::new(FreshNameGenerator::new(ast.all_symbols()));
  let mut table = SymbolTable::new();
  library::bind_builtins(&mut table);

  let ir = ir::compile_toplevel(&ast)?;
  let mut builder = CodeBuilder::new(decl::ClassExtends::Named("Node".to_owned()));
  compiler.compile_decls(&mut builder, &table, &ir)?;
  let result = builder.build();
  write!(output, "{}", result.to_gd())?;
  Ok(())

}

fn read_to_end(input: &mut impl Read) -> io::Result<String> {
  let mut vec = Vec::new();
  input.read_to_end(&mut vec)?;
  String::from_utf8(vec).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}
