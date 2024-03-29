// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! A compiler frame contains a [`Compiler`], as well as information
//! about the current stack frame data.

use super::Compiler;
use super::factory;
use super::special_form;
use super::names;
use super::constant::validate_all_constant_scopes;
use super::special_form::lambda;
use super::special_form::flet;
use super::special_form::lambda_class;
use super::special_form::let_block;
use super::preload_resolver::{PreloadResolver, DefaultPreloadResolver};
use super::symbol_table::{SymbolTable, HasSymbolTable};
use super::symbol_table::local_var::{LocalVar, ValueHint, VarName};
use super::names::fresh::FreshNameGenerator;
use super::names::registered::RegisteredNameGenerator;
use super::error::{GDError, GDErrorF};
use super::stateful::{StExpr, NeedsResult, SideEffects};
use super::body::builder::{StmtBuilder, CodeBuilder, HasDecls};
use super::body::class_scope::ClassScope;
use super::stmt_wrapper::{self, StmtWrapper};
use crate::pipeline::Pipeline;
use crate::pipeline::error::PError;
use crate::pipeline::source::SourceOffset;
use crate::ir;
use crate::ir::expr::{FuncRefTarget, AssignTarget, BareName, CallTarget, FunctionBindingType};
use crate::ir::special_ref::SpecialRef;
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::stmt::Stmt;
use crate::gdscript::decl::{self, Decl, DeclF};
use crate::gdscript::inner_class;
use crate::gdscript::metadata;
use crate::sxp::reify::pretty::reify_pretty_expr;
use crate::runner::path::RPathBuf;

use std::cmp::max;
use std::convert::TryFrom;

type IRExpr = ir::expr::Expr;
type IRExprF = ir::expr::ExprF;
type IRDecl = ir::decl::Decl;
type IRDeclF = ir::decl::DeclF;
type IRArgList = ir::arglist::ordinary::ArgList;

/// Quoted S-expressions which are nested deeper than this constant
/// will be split into several local variables, for efficiency
/// reasons. See [Godot
/// #52113](https://github.com/godotengine/godot/issues/52113) for the
/// reason this is necessary. This constant is passed to
/// [`reify_pretty_expr`].
pub const MAX_QUOTE_REIFY_DEPTH: u32 = 4;

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
pub struct CompilerFrame<'a, 'b, 'c, 'd, 'e, B> {
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
  /// The scope for the class we're currently conceptually inside of,
  /// including information about whether we're in a closure. This is
  /// important, because when we're in a closure, we're still
  /// conceptually within the class as far as GDLisp is concerned, but
  /// we're compiling to something outside of the class, so special
  /// care needs to be taken when referencing information on the
  /// current class.
  pub class_scope: &'e mut dyn ClassScope,
}

impl<'a, 'b, 'c, 'd, 'e, B> CompilerFrame<'a, 'b, 'c, 'd, 'e, B> {

  /// Convenience function to construct a `CompilerFrame`.
  pub fn new(compiler: &'a mut Compiler,
             pipeline: &'b mut Pipeline,
             builder: &'c mut B,
             table: &'d mut SymbolTable,
             class_scope: &'e mut dyn ClassScope)
             -> Self {
    CompilerFrame { compiler, pipeline, builder, table, class_scope }
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
    let mut new_frame = CompilerFrame::new(self.compiler, self.pipeline, new_builder, self.table, self.class_scope);
    block(&mut new_frame)
  }

  /// Compiles the expression `expr`, as though through
  /// [`CompilerFrame::compile_expr`].
  ///
  /// This method constructs a temporary [`StmtBuilder`] for use in
  /// the compilation. If the temporary builder ends up being used
  /// (i.e. if the compilation constructs any intermediate statements
  /// or declarations that require its use), then this method will
  /// return an [`GDErrorF::NotConstantEnough`] error. If the
  /// temporary builder goes unused, then the resulting [`Expr`] is
  /// returned.
  pub fn compile_simple_expr(&mut self,
                             src_name: &str,
                             expr: &IRExpr,
                             needs_result: NeedsResult)
                             -> Result<Expr, GDError> {
    let mut tmp_builder = StmtBuilder::new();
    let value = self.with_builder(&mut tmp_builder, |frame| {
      frame.compile_expr(expr, needs_result).map(|x| x.expr)
    })?;
    let (stmts, decls) = tmp_builder.build();
    if stmts.is_empty() && decls.is_empty() {
      Ok(value)
    } else {
      Err(GDError::new(GDErrorF::NotConstantEnough(String::from(src_name)), expr.pos))
    }
  }

}

impl<'a, 'b, 'c, 'd, 'e, B: HasDecls> CompilerFrame<'a, 'b, 'c, 'd, 'e, B> {

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
  /// but the block is expected to return a [`Result<R, GDError>`],
  /// whose error values are propagated outward to the return value of
  /// this method.
  pub fn with_local_builder_result<R, F>(&mut self, block: F) -> Result<(R, Vec<Stmt>), GDError>
  where F : FnOnce(&mut CompilerFrame<StmtBuilder>) -> Result<R, GDError> {
    let (error_value, vec) = self.with_local_builder_value(block);
    error_value.map(|r| (r, vec))
  }

  /// As
  /// [`with_local_builder_result`](CompilerFrame::with_local_builder_result),
  /// but with no return value `R`. Only the vector of statements from
  /// the builder is returned. Equivalent to
  /// `self.with_local_builder_result(block).map(|x| x.1)`.
  pub fn with_local_builder<F>(&mut self, block: F) -> Result<Vec<Stmt>, GDError>
  where F : FnOnce(&mut CompilerFrame<StmtBuilder>) -> Result<(), GDError> {
    let (error_value, vec) = self.with_local_builder_value(block);
    error_value.map(|_| vec)
  }

  /// As
  /// [`with_local_builder_value`](CompilerFrame::with_local_builder_value),
  /// but with no return value `R`. Only the vector of statements from
  /// the builder is returned. Equivalent to
  /// `self.with_local_builder_value(block).1`.
  pub fn with_local_builder_ok<F>(&mut self, block: F) -> Vec<Stmt>
  where F : FnOnce(&mut CompilerFrame<StmtBuilder>) {
    let ((), vec) = self.with_local_builder_value(block);
    vec
  }

}

impl<'a, 'b, 'c, 'd, 'e> CompilerFrame<'a, 'b, 'c, 'd, 'e, CodeBuilder> {

  pub fn compile_toplevel(&mut self, toplevel: &ir::decl::TopLevel) -> Result<(), PError> {

    // Special check to make sure there is only one main class.
    let _ = toplevel.find_main_class()?;

    for imp in &toplevel.imports {
      self.compiler.resolve_import(self.pipeline, self.builder, self.table, imp)?;
    }
    self.compile_decls(&toplevel.decls)?;

    Ok(())
  }

  pub fn compile_decls(&mut self, decls: &[IRDecl]) -> Result<(), GDError> {

    // Bind all declarations into the symbol table.
    for decl in decls {
      Compiler::bind_decl(&self.compiler.magic_table, self.pipeline, self.table, decl)?;
    }

    // Validate the const-ness of any constants, enum values, or
    // `export` clause bodies.
    validate_all_constant_scopes(decls, self.table)?;

    // Now compile.
    for decl in decls {
      self.table.clear_synthetic_locals();
      self.compile_decl(decl)?;
    }

    Ok(())
  }

  pub fn compile_decl(&mut self, decl: &IRDecl) -> Result<(), GDError> {
    match &decl.value {
      IRDeclF::FnDecl(ir::decl::FnDecl { visibility: _, call_magic: _, name, args, body }) => {
        let gd_name = names::lisp_to_gd(name);
        let function = factory::declare_function(self, gd_name, args.clone(), body, &stmt_wrapper::Return)?;
        self.builder.add_decl(Decl::new(DeclF::FnDecl(decl::Static::IsStatic, function), decl.pos));
        Ok(())
      }
      IRDeclF::MacroDecl(ir::decl::MacroDecl { visibility: _, name, args, body }) => {
        // Note: Macros compile identically to functions, as far as
        // this stage of compilation is concerned. They'll be resolved
        // and then purged during the IR phase.
        let gd_name = names::lisp_to_gd(name);
        let function = factory::declare_function(self, gd_name, args.clone(), body, &stmt_wrapper::Return)?;
        self.builder.add_decl(Decl::new(DeclF::FnDecl(decl::Static::IsStatic, function), decl.pos));
        Ok(())
      }
      IRDeclF::SymbolMacroDecl(ir::decl::SymbolMacroDecl { visibility: _, name, body }) => {
        // Note: Macros compile identically to functions, as far as
        // this stage of compilation is concerned. They'll be resolved
        // and then purged during the IR phase.
        let gd_name = metadata::symbol_macro(&names::lisp_to_gd(name));
        let function = factory::declare_function(self, gd_name, IRArgList::empty(), body, &stmt_wrapper::Return)?;
        self.builder.add_decl(Decl::new(DeclF::FnDecl(decl::Static::IsStatic, function), decl.pos));
        Ok(())
      }
      IRDeclF::ConstDecl(ir::decl::ConstDecl { visibility: _, name, value }) => {
        let gd_name = names::lisp_to_gd(name);
        let value = self.compile_simple_expr(name, value, NeedsResult::Yes)?;
        self.builder.add_decl(Decl::new(DeclF::ConstDecl(gd_name, value), decl.pos));
        Ok(())
      }
      IRDeclF::ClassDecl(ir::decl::ClassDecl { visibility: _, name, extends, main_class, constructor, decls }) => {
        let gd_name = names::lisp_to_gd(name);
        let extends = Compiler::resolve_extends(self.table, extends, decl.pos)?;

        // Synthesize default constructor if needed
        let default_constructor: ir::decl::ConstructorDecl;
        let constructor = match constructor {
          None => {
            default_constructor = ir::decl::ConstructorDecl::empty(decl.pos);
            &default_constructor
          }
          Some(c) => {
            c
          }
        };

        let class = factory::declare_class(self, gd_name, extends, *main_class, constructor, decls, decl.pos)?;
        if *main_class {
          factory::flatten_class_into_main(self.compiler.import_path_table(), self.builder, class);
          Ok(())
        } else {
          self.builder.add_decl(Decl::new(DeclF::ClassDecl(class), decl.pos));
          Ok(())
        }
      }
      IRDeclF::EnumDecl(ir::decl::EnumDecl { visibility: _, name, clauses }) => {
        let gd_name = names::lisp_to_gd(name);
        let gd_clauses = clauses.iter().map(|(const_name, const_value)| {
          let gd_const_name = names::lisp_to_gd(const_name);
          let gd_const_value = const_value.as_ref().map(|x| self.compile_simple_expr(const_name, x, NeedsResult::Yes)).transpose()?;
          Ok((gd_const_name, gd_const_value))
        }).collect::<Result<_, GDError>>()?;
        self.builder.add_decl(Decl::new(DeclF::EnumDecl(decl::EnumDecl { name: Some(gd_name), clauses: gd_clauses }), decl.pos));
        Ok(())
      }
      IRDeclF::DeclareDecl(_) => {
        // (sys/declare ...) statements have no runtime presence and do
        // nothing here.
        Ok(())
      }
    }
  }

}

impl<'a, 'b, 'c, 'd, 'e> CompilerFrame<'a, 'b, 'c, 'd, 'e, StmtBuilder> {

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
                       -> Result<StExpr, GDError> {
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
                      destination: &(impl StmtWrapper + ?Sized),
                      stmt: &IRExpr)
                      -> Result<(), GDError> {
    let needs_result = NeedsResult::from(!destination.is_vacuous());
    let expr = self.compile_expr(stmt, needs_result)?;
    destination.wrap_to_builder(self.builder, expr);
    Ok(())
  }

  // TODO Document me
  pub fn compile_expr(&mut self,
                      expr: &IRExpr,
                      needs_result: NeedsResult)
                      -> Result<StExpr, GDError> {
    match &expr.value {
      IRExprF::BareName(name) => {
        self.compile_bare_name(name, expr.pos)
      }
      IRExprF::Literal(lit) => {
        let lit = factory::compile_literal(lit, expr.pos);
        Ok(StExpr { expr: lit, side_effects: SideEffects::None })
      }
      IRExprF::Progn(body) => {
        let body: Vec<_> = body.iter().collect();
        self.compile_stmts(&body[..], needs_result, expr.pos)
      }
      IRExprF::CondStmt(clauses) => {
        special_form::compile_cond_stmt(self, clauses, needs_result, expr.pos)
      }
      IRExprF::WhileStmt(cond, body) => {
        special_form::compile_while_stmt(self, cond, body, needs_result, expr.pos)
      }
      IRExprF::ForStmt(name, iter, body) => {
        special_form::compile_for_stmt(self, name, iter, body, needs_result, expr.pos)
      }
      IRExprF::Call(object, f, args) => {
        self.compile_function_call(object, f, args, expr.pos)
      }
      IRExprF::Let(clauses, body) => {
        let_block::compile_let(self, clauses, body, needs_result, expr.pos)
      }
      IRExprF::FunctionLet(FunctionBindingType::OuterScoped, clauses, body) => {
        flet::compile_flet(self, clauses, body, needs_result, self.compiler.is_minimalist(), expr.pos)
      }
      IRExprF::FunctionLet(FunctionBindingType::Recursive, clauses, body) => {
        flet::compile_labels(self, clauses, body, needs_result, expr.pos)
      }
      IRExprF::Lambda(args, body) => {
        lambda::compile_lambda_stmt(self, args, body, self.compiler.is_minimalist(), expr.pos)
      }
      IRExprF::FuncRef(name) => {
        match name {
          FuncRefTarget::SimpleName(name) => {
            let func = self.table.get_fn(name).ok_or_else(|| GDError::new(GDErrorF::NoSuchFn(name.clone()), expr.pos))?.0.clone();
            lambda::compile_function_ref(self.compiler, self.pipeline, self.builder, self.table, func, self.compiler.is_minimalist(), expr.pos)
          }
        }
      }
      IRExprF::Assign(target, expr) => {
        self.compile_assignment(target, expr, needs_result)
      }
      IRExprF::Quote(ast) => {
        let mut gen = RegisteredNameGenerator::new_local_var(self.table);
        let (stmts, result) = reify_pretty_expr(ast, MAX_QUOTE_REIFY_DEPTH, &mut gen);
        self.builder.append_all(&mut stmts.into_iter());
        Ok(StExpr { expr: result, side_effects: SideEffects::None })
      }
      IRExprF::FieldAccess(lhs, sym) => {

        // This is a special case to validate enum names, as an extra sanity check.
        if let Some(lhs) = lhs.as_plain_name() {
          if let Some(LocalVar { value_hint: Some(ValueHint::Enum(vs)), .. }) = self.table.get_var(lhs) {
            // It's an enum and we know its values; validate
            if !vs.contains(&names::lisp_to_gd(sym)) {
              return Err(GDError::new(GDErrorF::NoSuchEnumValue(lhs.to_owned(), sym.clone()), expr.pos));
            }
          }
        }

        let StExpr { expr: lhs, side_effects: state } = self.compile_expr(lhs, NeedsResult::Yes)?;
        let side_effects = max(SideEffects::ReadsState, state);
        Ok(StExpr { expr: Expr::new(ExprF::Attribute(Box::new(lhs), names::lisp_to_gd(sym)), expr.pos), side_effects })

      }
      IRExprF::LambdaClass(cls) => {
        lambda_class::compile_lambda_class(self, cls, expr.pos)
      }
      IRExprF::Yield(arg) => {
        match arg {
          None => Ok(StExpr { expr: Expr::yield_expr(None, expr.pos), side_effects: SideEffects::ModifiesState }),
          Some((x, y)) => {
            let x = self.compile_expr(x, NeedsResult::Yes)?.expr;
            let y = self.compile_expr(y, NeedsResult::Yes)?.expr;
            Ok(StExpr { expr: Expr::yield_expr(Some((x, y)), expr.pos), side_effects: SideEffects::ModifiesState })
          }
        }
      }
      IRExprF::Assert(cond, message) => {
        let cond = self.compile_expr(cond, NeedsResult::Yes)?.expr;
        let message = message.as_ref().map(|x| self.compile_expr(x, NeedsResult::Yes).map(|y| y.expr)).transpose()?;
        Ok(StExpr { expr: Expr::assert_expr(cond, message, expr.pos), side_effects: SideEffects::ModifiesState })
      }
      IRExprF::Return(expr) => {
        self.compile_stmt(&stmt_wrapper::Return, expr)?;
        Ok(Compiler::nil_expr(expr.pos))
      }
      IRExprF::Break => {
        self.builder.append(Stmt::break_stmt(expr.pos));
        Ok(Compiler::nil_expr(expr.pos))
      }
      IRExprF::Continue => {
        self.builder.append(Stmt::continue_stmt(expr.pos));
        Ok(Compiler::nil_expr(expr.pos))
      }
      IRExprF::SpecialRef(special_ref) => {
        Ok(self.compile_special_ref(*special_ref, expr.pos))
      }
      IRExprF::ContextualFilename(filename) => {
        let new_filename = self.preload_resolver().resolve_preload(filename)
          .ok_or_else(|| GDError::new(GDErrorF::ContextualFilenameUnresolved, expr.pos))?;
        Ok(StExpr { expr: Expr::from_value(new_filename, expr.pos), side_effects: SideEffects::None })
      }
      IRExprF::Split(name, expr) => {
        let pos = expr.pos;
        let expr = self.compile_expr(expr, NeedsResult::Yes)?.expr;
        let mut gen = RegisteredNameGenerator::new_local_var(self.table);
        let tmp_var = factory::declare_var(&mut gen, self.builder, name, Some(expr), pos); // TODO Contextual name generator?
        Ok(StExpr {
          expr: Expr::new(ExprF::Var(tmp_var), pos),
          side_effects: SideEffects::None,
        })
      }
      IRExprF::Preload(arg) => {
        let path = RPathBuf::try_from(arg.to_owned()).map_err(|_| {
          GDError::new(GDErrorF::BadPreloadArgument(arg.to_owned()), expr.pos)
        })?;
        let preload_call = self.compiler.make_preload_expr(&path, expr.pos)?;
        Ok(StExpr {
          expr: preload_call,
          side_effects: SideEffects::None,
        })
      }
      /* // This will eventually be an optimization.
      IRExprF::Funcall(f, args) => {
        let func_expr = self.compile_expr(builder, table, f, NeedsResult::Yes)?.0;
        let args_expr = args.iter().map(|arg| {
        self.compile_expr(builder, table, arg, NeedsResult::Yes).map(|x| x.0)
      }).collect::<Result<Vec<_>, _>>()?;
        let fn_name = String::from("call_func");
        let expr = Expr::Call(Some(Box::new(func_expr)), fn_name, args_expr);
        Ok(StExpr(expr, true))
      }
       */
    }
  }

  fn compile_bare_name(&mut self,
                       bare_name: &BareName,
                       pos: SourceOffset)
                       -> Result<StExpr, GDError> {
    match bare_name {
      BareName::Plain(s) => {
        self.table.get_var(s).ok_or_else(|| GDError::new(GDErrorF::NoSuchVar(s.clone()), pos)).map(|var| {
          StExpr { expr: var.expr(pos), side_effects: SideEffects::from(var.access_type) }
        })
      }
      BareName::Atomic(s) => {
        Ok(StExpr { expr: Expr::var(&names::lisp_to_gd_bare(s), pos), side_effects: SideEffects::ReadsState })
      }
    }
  }

  fn compile_function_call(&mut self,
                           object: &CallTarget,
                           name: &str,
                           args: &[IRExpr],
                           pos: SourceOffset)
                           -> Result<StExpr, GDError> {
    match object {
      CallTarget::Scoped => {
        self.compile_ordinary_function_call(name, args, pos)
      }
      CallTarget::Object(lhs) => {
        // Note: No call magic, no optional/rest arguments. When
        // calling a method, we assume all arguments are required, we
        // perform no optimization, we do not check arity, and we
        // simply blindly forward the call on the GDScript side.
        let lhs = self.compile_expr(lhs, NeedsResult::Yes)?.expr;
        let args = args.iter()
          .map(|arg| self.compile_expr(arg, NeedsResult::Yes).map(|x| x.expr))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(StExpr {
          expr: Expr::call(Some(lhs), &names::lisp_to_gd(name), args, pos),
          side_effects: SideEffects::ModifiesState
        })
      }
      CallTarget::Super => {
        let args = args.iter()
          .map(|arg| self.compile_expr(arg, NeedsResult::Yes).map(|x| x.expr))
          .collect::<Result<Vec<_>, _>>()?;
        let self_binding = self.table.get_var("self").ok_or_else(|| GDError::new(GDErrorF::BadSuperCall(String::from(name)), pos))?;
        let expr = self.class_scope.super_call(self.compiler.name_generator(), self_binding, name.to_owned(), args, pos)?;
        Ok(StExpr {
          expr: expr,
          side_effects: SideEffects::ModifiesState,
        })
      }
      CallTarget::Atomic => {
        let name = names::lisp_to_gd_bare(name);
        let args = args.iter()
          .map(|x| self.compile_expr(x, NeedsResult::Yes).map(|x| x.expr))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(StExpr {
          expr: Expr::call(None, &name, args, pos),
          side_effects: SideEffects::ModifiesState,
        })
      }
    }
  }

  /// Compiles a function call, given the name of the function and its
  /// argument list.
  pub fn compile_ordinary_function_call(&mut self,
                                        name: &str,
                                        args: &[IRExpr],
                                        pos: SourceOffset)
                                        -> Result<StExpr, GDError> {
    let (fcall, call_magic) = match self.table.get_fn(name) {
      None => return Err(GDError::new(GDErrorF::NoSuchFn(name.to_owned()), pos)),
      Some((p, m)) => (p.clone(), dyn_clone::clone_box(m))
    };
    // Macro calls should not occur at this stage in compilation.
    if fcall.is_macro {
      return Err(GDError::new(GDErrorF::MacroBeforeDefinitionError(name.to_owned()), pos));
    }
    // Call magic is used to implement some commonly used wrappers
    // for simple GDScript operations.
    let args = args.iter()
      .map(|x| self.compile_expr(x, NeedsResult::Yes))
      .collect::<Result<Vec<_>, _>>()?;
    Ok(StExpr {
      expr: fcall.into_expr_with_magic(&call_magic, self.compiler, self.builder, self.table, args, pos)?,
      side_effects: SideEffects::ModifiesState,
    })
  }

  pub fn compile_special_ref(&mut self, special_ref: SpecialRef, pos: SourceOffset) -> StExpr {
    match special_ref {
      SpecialRef::ThisFile => {
        let current_filename =
          inner_class::get_current_filename(self.pipeline, self.compiler.preload_resolver())
          .expect("Error identifying current file");
        let expr = VarName::load_expr(current_filename, pos);
        StExpr { expr, side_effects: SideEffects::None }
      }
      SpecialRef::ThisFileName => {
        let current_filename =
          inner_class::get_current_filename(self.pipeline, self.compiler.preload_resolver())
          .expect("Error identifying current file");
        let expr = Expr::from_value(current_filename, pos);
        StExpr { expr, side_effects: SideEffects::None }
      }
      SpecialRef::ThisTrueFileName => {
        let current_filename =
          inner_class::get_current_filename(self.pipeline, &DefaultPreloadResolver)
          .expect("Error identifying current file");
        let expr = Expr::from_value(current_filename, pos);
        StExpr { expr, side_effects: SideEffects::None }
      }
      SpecialRef::GodotVersion => {
        let godot_version = self.pipeline.config().godot_version.version;
        let expr = Expr::from_value(godot_version.into_i32(), pos);
        StExpr { expr, side_effects: SideEffects::None }
      }
    }
  }

  fn compile_assignment(&mut self,
                        target: &AssignTarget,
                        expr: &IRExpr,
                        needs_result: NeedsResult)
                        -> Result<StExpr, GDError> {
    match target {
      AssignTarget::Variable(pos, name) => {
        let var = self.table.get_var(name).ok_or_else(|| GDError::new(GDErrorF::NoSuchVar(name.clone()), *pos))?.to_owned();
        if !var.assignable {
          return Err(GDError::new(GDErrorF::CannotAssignTo(var.name.to_gd(*pos)), expr.pos));
        }
        self.compile_stmt(&stmt_wrapper::AssignToExpr(var.expr(*pos)), expr)?;
        Ok(StExpr { expr: var.expr(*pos), side_effects: SideEffects::from(var.access_type) })
      }
      AssignTarget::InstanceField(pos, lhs, name) => {
        let StExpr { expr: lhs, side_effects: _ } = self.compile_expr(lhs, NeedsResult::Yes)?;
        let lhs = Expr::new(ExprF::Attribute(Box::new(lhs), names::lisp_to_gd(name)), expr.pos);
        let StExpr { expr: mut rhs, side_effects } = self.compile_expr(expr, NeedsResult::Yes)?;
        // Assign to a temporary if the RHS is stateful and we need a result.
        if needs_result == NeedsResult::Yes && side_effects.modifies_state() {
          let mut gen = RegisteredNameGenerator::new_local_var(self.table);
          let var = factory::declare_var(&mut gen, self.builder, "_assign", Some(rhs), expr.pos);
          rhs = Expr::new(ExprF::Var(var), *pos);
        }
        self.builder.append(
          Stmt::simple_assign(lhs, rhs.clone(), *pos),
        );
        if needs_result == NeedsResult::Yes {
          Ok(StExpr { expr: rhs, side_effects: SideEffects::None })
        } else {
          Ok(Compiler::nil_expr(expr.pos))
        }
      }
    }
  }

}

impl<'a, 'b, 'c, 'd, 'e, B> HasSymbolTable for CompilerFrame<'a, 'b, 'c, 'd, 'e, B> {

  fn get_symbol_table(&self) -> &SymbolTable {
    self.table
  }

  fn get_symbol_table_mut(&mut self) -> &mut SymbolTable {
    self.table
  }

}
