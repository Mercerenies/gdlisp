
//! A compiler frame contains a [`Compiler`], as well as information
//! about the current stack frame data.

#![allow(deprecated)]

use super::Compiler;
use super::preload_resolver::PreloadResolver;
use super::symbol_table::{SymbolTable, HasSymbolTable};
use super::names::fresh::FreshNameGenerator;
use super::error::{Error, ErrorF};
use super::stateful::{StExpr, NeedsResult};
use super::body::builder::{StmtBuilder, HasDecls};
use super::stmt_wrapper::{self, StmtWrapper};
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::ir;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;

type IRExpr = ir::expr::Expr;

/// A `CompilerFrame` contains references to all of the pertinent
/// information about a particular frame (hence, scope) of a GDScript
/// body during compilation.
///
/// `CompilerFrame` never takes ownership of any of its fields; it
/// always mutably borrows every field. The type parameter `B` is the
/// type of the builder. This structure can be used in declaration
/// context
/// ([`CodeBuilder`](crate::compile::body::builder::CodeBuilder)) or in
/// statement context
/// ([`StmtBuilder`](crate::compile::body::builder::StmtBuilder)), and
/// the type of the builder determines which functionality is
/// available.
///
/// Note that several of the methods on this structure are
/// conditionally available, based on the type of `B`. There are three
/// categories of methods.
///
/// * Some methods are always available, for all compiler frames.
///
/// * Some methods are available whenever `B` implements [`HasDecls`].
///
/// * Some methods are only available when `B` is the concrete type
///   [`StmtBuilder`].
pub struct CompilerFrame<'a, 'b, 'c, 'd, B> {
  /// The compiler for the file. The compiler is mutable, but it is
  /// unlikely to be completely replaced in local scopes.
  pub compiler: &'a mut Compiler,
  /// The pipeline for the compilation. The pipeline is mutable, but
  /// it is unlikely to be completely replaced in local scopes.
  pub pipeline: &'b mut Pipeline,
  /// The builder for the current block of code, whose type is
  /// parameterized by the structure's `B` type parameter.
  ///
  /// Generally, this is one of [`StmtBuilder`] (if building
  /// statements), [`CodeBuilder`](super::body::builder::CodeBuilder)
  /// (if building declarations), or `()` (if not using the
  /// builder argument), though there is no specific requirement that
  /// the type `B` implement any particular trait in general.
  ///
  /// The builder is replaced using calls to
  /// [`CompilerFrame::with_builder`] and company.
  pub builder: &'c mut B,
  /// The symbol table for the current scope. `CompilerFrame`
  /// implements [`HasSymbolTable`], so methods such as
  /// [`HasSymbolTable::with_local_var`] will work on `CompilerFrame`.
  pub table: &'d mut SymbolTable,
}

impl<'a, 'b, 'c, 'd, B> CompilerFrame<'a, 'b, 'c, 'd, B> {

  /// Convenience function to construct a `CompilerFrame`.
  pub fn new(compiler: &'a mut Compiler,
             pipeline: &'b mut Pipeline,
             builder: &'c mut B,
             table: &'d mut SymbolTable)
             -> Self {
    CompilerFrame { compiler, pipeline, builder, table }
  }

  /// Gets the [`FreshNameGenerator`] from the frame's [`Compiler`].
  pub fn name_generator(&mut self) -> &mut FreshNameGenerator {
    self.compiler.name_generator()
  }

  /// Gets the [`PreloadResolver`] from the frame's [`Compiler`].
  pub fn preload_resolver(&self) -> &dyn PreloadResolver {
    self.compiler.preload_resolver()
  }

  /// Constructs a new [`CompilerFrame`] identical to `self` except
  /// with `new_builder` as its builder. The new frame is passed to
  /// `block` as its sole argument, and the result of the block is
  /// returned.
  pub fn with_builder<B1, R, F>(&mut self, new_builder: &mut B1, block: F) -> R
  where F : FnOnce(&mut CompilerFrame<B1>) -> R {
    let mut new_frame = CompilerFrame::new(self.compiler, self.pipeline, new_builder, self.table);
    block(&mut new_frame)
  }

  /// Compiles the expression `expr`, as though through
  /// [`CompilerFrame::compile_expr`].
  ///
  /// This method constructs a temporary [`StmtBuilder`] for use in
  /// the compilation. If the temporary builder ends up being used
  /// (i.e. if the compilation constructs any intermediate statements
  /// or declarations that require its use), then this method will
  /// return an [`ErrorF::NotConstantEnough`] error. If the temporary
  /// builder goes unused, then the resulting [`Expr`] is returned.
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

impl<'a, 'b, 'c, 'd, B: HasDecls> CompilerFrame<'a, 'b, 'c, 'd, B> {

  /// This method allows a block of code to run, given access to a
  /// compiler frame identical to `self` but with a new
  /// [`StmtBuilder`].
  ///
  /// Specifically, this method constructs a new, empty `StmtBuilder`,
  /// then runs the block using that builder via
  /// [`CompilerFrame::with_builder`]. At the end of the block, the
  /// new local builder will be built into (via
  /// [`StmtBuilder::build_into`]) the outer builder `self.builder`.
  /// Finally, the result of the block and all of the statements built
  /// using the local builder are returned.
  pub fn with_local_builder_value<R, F>(&mut self, block: F) -> (R, Vec<Stmt>)
  where F : FnOnce(&mut CompilerFrame<StmtBuilder>) -> R {
    let mut local_builder = StmtBuilder::new();
    let result = self.with_builder(&mut local_builder, block);
    let stmts = local_builder.build_into(self.builder);
    (result, stmts)
  }

  /// As
  /// [`with_local_builder_value`](CompilerFrame::with_local_builder_value),
  /// but the block is expected to return a [`Result<R, Error>`],
  /// whose error values are propagated outward to the return value of
  /// this method.
  pub fn with_local_builder_result<R, F>(&mut self, block: F) -> Result<(R, Vec<Stmt>), Error>
  where F : FnOnce(&mut CompilerFrame<StmtBuilder>) -> Result<R, Error> {
    let (error_value, vec) = self.with_local_builder_value(block);
    error_value.map(|r| (r, vec))
  }

  /// As
  /// [`with_local_builder_result`](CompilerFrame::with_local_builder_result),
  /// but with no return value `R`. Only the vector of statements from
  /// the builder is returned. Equivalent to
  /// `self.with_local_builder_result(block).map(|x| x.1)`.
  pub fn with_local_builder<F>(&mut self, block: F) -> Result<Vec<Stmt>, Error>
  where F : FnOnce(&mut CompilerFrame<StmtBuilder>) -> Result<(), Error> {
    let (error_value, vec) = self.with_local_builder_value(block);
    error_value.map(|_| vec)
  }

  /// As
  /// [`with_local_builder_value`](CompilerFrame::with_local_builder_value),
  /// but with no return value `R`. Only the vector of statements from
  /// the builder is returned. Equivalent to
  /// `self.with_local_builder_value(block).1`.
  pub fn with_local_builder_ok<F>(&mut self, block: F) -> Vec<Stmt>
  where F : FnOnce(&mut CompilerFrame<StmtBuilder>) -> () {
    let ((), vec) = self.with_local_builder_value(block);
    vec
  }

}

impl<'a, 'b, 'c, 'd> CompilerFrame<'a, 'b, 'c, 'd, StmtBuilder> {

  /// Compiles the sequence of statements into the current frame's
  /// builder.
  ///
  /// If `stmts` is empty, then the builder is unmodified and
  /// [`Expr::null`] is returned. Otherwise, all except the *final*
  /// statement in `stmts` are compiled using
  /// [`compile_stmt`](CompilerFrame::compile_stmt) (with destination
  /// of [`stmt_wrapper::Vacuous`]). The final statement is compiled
  /// with [`compile_expr`](CompilerFrame::compile_expr), using
  /// `needs_result` to determine if the result value will be used.
  ///
  /// The source offset `pos` is only used if `stmts` is empty, in
  /// which case it is reported as the source location of the compiled
  /// null value.
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

  /// Compiles a single statement into the current builder. The IR
  /// expression `stmt` is compiled (via
  /// [`compile_expr`](CompilerFrame::compile_expr)) into an [`Expr`].
  /// Then that expression object is converted into a [`Stmt`] using
  /// [`destination.wrap_to_builder`](StmtWrapper::wrap_to_builder).
  ///
  /// During the `compile_expr` call, `needs_result` is
  /// [`NeedsResult::Yes`] if and only if `destination.is_vacuous()`
  /// is false. That is, the expression result will be discarded if
  /// and only if the statement destination is vacuous.
  pub fn compile_stmt(&mut self,
                      destination: &dyn StmtWrapper,
                      stmt: &IRExpr)
                      -> Result<(), Error> {
    let needs_result = NeedsResult::from(!destination.is_vacuous());
    let expr = self.compile_expr(stmt, needs_result)?;
    destination.wrap_to_builder(self.builder, expr);
    Ok(())
  }

  // TODO Document me
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
