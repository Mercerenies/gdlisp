
//! A compiler frame contains a [`Compiler`], as well as information
//! about the current stack frame data.

#![allow(deprecated)]

use super::Compiler;
use super::preload_resolver::PreloadResolver;
use super::symbol_table::{SymbolTable, HasSymbolTable};
use super::names::fresh::FreshNameGenerator;
use super::error::{Error, ErrorF};
use super::stateful::{StExpr, NeedsResult};
use super::body::builder::StmtBuilder;
use super::stmt_wrapper::{self, StmtWrapper};
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

  pub fn with_builder<B1, R, F>(&mut self, new_builder: &mut B1, block: F) -> R
  where F : FnOnce(&mut CompilerFrame<B1>) -> R {
    let mut new_frame = CompilerFrame::new(self.compiler, self.pipeline, new_builder, self.table);
    block(&mut new_frame)
  }

  pub fn compile_simple_expr(&mut self,
                             src_name: &str,
                             expr: &IRExpr,
                             needs_result: NeedsResult)
                             -> Result<Expr, Error> {
    let mut tmp_builder = StmtBuilder::new();
    let value = self.with_builder(&mut tmp_builder, |frame| {
      frame.compile_expr(expr, needs_result).map(|x| x.expr)
    })?;
    let (stmts, decls) = tmp_builder.build();
    if stmts.is_empty() && decls.is_empty() {
      Ok(value)
    } else {
      Err(Error::new(ErrorF::NotConstantEnough(String::from(src_name)), expr.pos))
    }
  }

}

impl<'a, 'b, 'c, 'd> CompilerFrame<'a, 'b, 'c, 'd, StmtBuilder> {

  pub fn compile_stmts(&mut self,
                       stmts: &[&IRExpr],
                       needs_result: NeedsResult,
                       pos: SourceOffset)
                       -> Result<StExpr, Error> {
    if stmts.is_empty() {
      Ok(Compiler::nil_expr(pos))
    } else {
      let prefix = &stmts[..stmts.len()-1];
      let end = &stmts[stmts.len()-1];
      for x in prefix {
        self.compile_stmt(&stmt_wrapper::Vacuous, x)?;
      }
      self.compile_expr(end, needs_result)
    }
  }

  pub fn compile_stmt(&mut self,
                      destination: &dyn StmtWrapper,
                      stmt: &IRExpr)
                      -> Result<(), Error> {
    let needs_result = NeedsResult::from(!destination.is_vacuous());
    let expr = self.compile_expr(stmt, needs_result)?;
    destination.wrap_to_builder(self.builder, expr);
    Ok(())
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
