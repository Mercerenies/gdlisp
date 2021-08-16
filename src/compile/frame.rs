
//! A compiler frame contains a [`Compiler`], as well as information
//! about the current stack frame data.

use super::Compiler;
use super::preload_resolver::PreloadResolver;
use super::symbol_table::{SymbolTable, HasSymbolTable};
use super::names::fresh::FreshNameGenerator;
use super::error::Error;
use super::stateful::{StExpr, NeedsResult};
use super::body::builder::StmtBuilder;
use super::stmt_wrapper::StmtWrapper;
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::ir;
use crate::gdscript::expr::Expr;

type IRExpr = ir::expr::Expr;

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

  pub fn preload_resolver(&self) -> &dyn PreloadResolver {
    self.compiler.preload_resolver()
  }

  pub fn compile_simple_expr(&mut self,
                             src_name: &str,
                             expr: &IRExpr,
                             needs_result: NeedsResult)
                             -> Result<Expr, Error> {
    self.compiler.compile_simple_expr(
      self.pipeline,
      self.table,
      src_name,
      expr,
      needs_result,
    )
  }

}

impl<'a, 'b, 'c, 'd> CompilerFrame<'a, 'b, 'c, 'd, StmtBuilder> {

  pub fn compile_stmts(&mut self,
                       stmts: &[&IRExpr],
                       needs_result: NeedsResult,
                       pos: SourceOffset)
                       -> Result<StExpr, Error> {
    self.compiler.compile_stmts(
      self.pipeline,
      self.builder,
      self.table,
      stmts,
      needs_result,
      pos,
    )
  }

  pub fn compile_stmt(&mut self,
                      destination: &dyn StmtWrapper,
                      stmt: &IRExpr)
                      -> Result<(), Error> {
    self.compiler.compile_stmt(
      self.pipeline,
      self.builder,
      self.table,
      destination,
      stmt,
    )
  }

  pub fn compile_expr(&mut self,
                      expr: &IRExpr,
                      needs_result: NeedsResult)
                      -> Result<StExpr, Error> {
    self.compiler.compile_expr(
      self.pipeline,
      self.builder,
      self.table,
      expr,
      needs_result,
    )
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
