
//! A compiler frame contains a [`Compiler`], as well as information
//! about the current stack frame data.

use super::Compiler;
use super::symbol_table::{SymbolTable, HasSymbolTable};
use super::names::fresh::FreshNameGenerator;
use crate::pipeline::Pipeline;

/// A `CompilerFrame` contains references to all of the pertinent
/// information about a particular frame (hence, scope) of a GDScript
/// body during compilation.
///
/// `CompilerFrame` never takes ownership of any of its fields; it
/// always mutably borrows every field. The type parameter `B` is the
/// type of the builder. This structure can be used in declaration
/// context
/// ([`CodeBuilder`](crate::compile::body::builder::CodeBuilder) or in
/// statement context
/// ([`StmtBuilder`](crate::compile::body::builder::StmtBuilder)), and
/// the type of the builder determines which functionality is
/// available.
pub struct CompilerFrame<'a, 'b, 'c, 'd, B> {
  pub compiler: &'a mut Compiler,
  pub pipeline: &'b mut Pipeline,
  pub builder: &'c mut B,
  pub table: &'d mut SymbolTable,
}

impl<'a, 'b, 'c, 'd, B> CompilerFrame<'a, 'b, 'c, 'd, B> {

  pub fn new(compiler: &'a mut Compiler,
             pipeline: &'b mut Pipeline,
             builder: &'c mut B,
             table: &'d mut SymbolTable)
             -> Self {
    CompilerFrame { compiler, pipeline, builder, table }
  }

  pub fn name_generator(&mut self) -> &mut FreshNameGenerator {
    self.compiler.name_generator()
  }

}

impl<'a, 'b, 'c, 'd, B> HasSymbolTable for CompilerFrame<'a, 'b, 'c, 'd, B> {

  fn get_symbol_table(&self) -> &SymbolTable {
    self.table
  }

  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable {
    self.table
  }

}
