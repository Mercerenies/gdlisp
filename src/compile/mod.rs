
pub mod names;
pub mod body;
pub mod error;
pub mod stmt_wrapper;
pub mod symbol_table;
pub mod special_form;
pub mod stateful;
pub mod preload_resolver;

use body::builder::{CodeBuilder, StmtBuilder, HasDecls};
use names::fresh::FreshNameGenerator;
use preload_resolver::PreloadResolver;
use crate::sxp::reify::Reify;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::op;
use crate::gdscript::library;
use error::Error;
use stmt_wrapper::StmtWrapper;
use symbol_table::{HasSymbolTable, SymbolTable, LocalVar, VarScope};
use symbol_table::function_call;
use symbol_table::call_magic::DefaultCall;
use crate::ir;
use crate::ir::expr::{FuncRefTarget, AssignTarget};
use crate::ir::import::{ImportName, ImportDecl, ImportDetails};
use crate::ir::identifier::Namespace;
use crate::ir::locals::AccessType;
use crate::runner::path::RPathBuf;
use crate::pipeline::error::{Error as PError};
use crate::pipeline::Pipeline;
use special_form::lambda;
use special_form::flet;
use stateful::{StExpr, NeedsResult, SideEffects};

use std::cmp::max;

type IRDecl = ir::decl::Decl;
type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;
type IRLiteral = ir::literal::Literal;

pub struct Compiler<'a> {
  gen: FreshNameGenerator<'a>,
  resolver: Box<dyn PreloadResolver>,
}

impl<'a> Compiler<'a> {

  pub fn new(gen: FreshNameGenerator<'a>, resolver: Box<dyn PreloadResolver>) -> Compiler<'a> {
    Compiler { gen, resolver }
  }

  pub fn compile_stmts(&mut self,
                       builder: &mut StmtBuilder,
                       table: &mut SymbolTable,
                       stmts: &[&IRExpr],
                       needs_result: NeedsResult)
                       -> Result<StExpr, Error> {
    if stmts.is_empty() {
      Ok(Compiler::nil_expr())
    } else {
      let prefix = &stmts[..stmts.len()-1];
      let end = &stmts[stmts.len()-1];
      for x in prefix {
        self.compile_stmt(builder, table, &stmt_wrapper::Vacuous, x)?;
      }
      self.compile_expr(builder, table, end, needs_result)
    }
  }

  pub fn compile_stmt(&mut self,
                      builder: &mut StmtBuilder,
                      table: &mut SymbolTable,
                      destination: &dyn StmtWrapper,
                      stmt: &IRExpr)
                      -> Result<(), Error> {
    let needs_result = NeedsResult::from(!destination.is_vacuous());
    let expr = self.compile_expr(builder, table, stmt, needs_result)?;
    destination.wrap_to_builder(builder, expr);
    Ok(())
  }

  pub fn compile_expr(&mut self,
                      builder: &mut StmtBuilder,
                      table: &mut SymbolTable,
                      expr: &IRExpr,
                      needs_result: NeedsResult)
                      -> Result<StExpr, Error> {
    // TODO I made a mess of this when converting to IR. Separate this
    // into many helper functions, probably over multiple files.
    match expr {
      IRExpr::LocalVar(s) => {
        table.get_var(s).ok_or_else(|| Error::NoSuchVar(s.clone())).map(|var| {
          StExpr(var.expr(), SideEffects::from(var.access_type))
        })
      }
      IRExpr::Literal(lit) => {
        match lit {
          IRLiteral::Nil => Ok(Compiler::nil_expr()),
          IRLiteral::Int(n) => Ok(StExpr(Expr::from(*n), SideEffects::None)),
          IRLiteral::Float(f) => Ok(StExpr(Expr::from(*f), SideEffects::None)),
          IRLiteral::Bool(b) => Ok(StExpr(Expr::from(*b), SideEffects::None)),
          IRLiteral::String(s) => Ok(StExpr(Expr::from(s.to_owned()), SideEffects::None)),
          IRLiteral::Symbol(s) =>
            Ok(StExpr(Expr::Call(Some(Box::new(library::symbol_class())), String::from("new"), vec!(s.reify())),
                      SideEffects::None)),
        }
      }
      IRExpr::Progn(body) => {
        let body: Vec<_> = body.iter().collect();
        self.compile_stmts(builder, table, &body[..], needs_result)
      }
      IRExpr::IfStmt(c, t, f) => {
        special_form::compile_if_stmt(self, builder, table, c, t, f, needs_result)
      }
      IRExpr::CondStmt(clauses) => {
        special_form::compile_cond_stmt(self, builder, table, clauses, needs_result)
      }
      IRExpr::WhileStmt(cond, body) => {
        special_form::compile_while_stmt(self, builder, table, cond, body, needs_result)
      }
      IRExpr::ForStmt(name, iter, body) => {
        special_form::compile_for_stmt(self, builder, table, &*name, iter, body, needs_result)
      }
      IRExpr::Call(f, args) => {
        let (fcall, call_magic) = match table.get_fn(f) {
          None => return Err(Error::NoSuchFn(f.clone())),
          Some((p, m)) => (p.clone(), dyn_clone::clone_box(m))
        };
        // Call magic is used to implement some commonly used wrappers
        // for simple GDScript operations.
        let args = args.iter()
                       .map(|x| self.compile_expr(builder, table, x, NeedsResult::Yes))
                       .collect::<Result<Vec<_>, _>>()?;
        Ok(StExpr(fcall.into_expr_with_magic(&*call_magic, self, builder, table, args)?, SideEffects::ModifiesState))
      }
      IRExpr::Let(clauses, body) => {
        let closure_vars = body.get_locals();
        let var_names = clauses.iter().map::<Result<(String, String), Error>, _>(|clause| {
          let (ast_name, expr) = clause;
          let ast_name = ast_name.to_owned();
          let result_value = self.compile_expr(builder, table, &expr, NeedsResult::Yes)?.0;
          let result_value =
            if closure_vars.get(&ast_name).requires_cell() {
              library::construct_cell(result_value)
            } else {
              result_value
            };
          let gd_name = self.declare_var(builder, &names::lisp_to_gd(&ast_name), Some(result_value));
          Ok((ast_name, gd_name))
        }).collect::<Result<Vec<_>, _>>()?;
        table.with_local_vars(&mut var_names.into_iter().map(|x| (x.0.clone(), LocalVar::local(x.1, closure_vars.get(&x.0)))), |table| {
          self.compile_expr(builder, table, body, needs_result)
        })
      }
      IRExpr::FLet(clauses, body) => {
        flet::compile_flet(self, builder, table, clauses, body, needs_result)
      }
      IRExpr::Labels(clauses, body) => {
        flet::compile_labels(self, builder, table, clauses, body, needs_result)
      }
      IRExpr::Lambda(args, body) => {
        lambda::compile_lambda_stmt(self, builder, table, args, body)
      }
      IRExpr::FuncRef(name) => {
        match name {
          FuncRefTarget::SimpleName(name) => {
            let func = table.get_fn(name).ok_or_else(|| Error::NoSuchFn(name.clone()))?.0.clone();
            lambda::compile_function_ref(self, builder, table, func)
          }
        }
      }
      IRExpr::Assign(AssignTarget::Variable(name), expr) => {
        let var = table.get_var(name).ok_or_else(|| Error::NoSuchVar(name.clone()))?.to_owned();
        if !var.assignable {
          return Err(Error::CannotAssignTo(var.name.to_gd()));
        }
        self.compile_stmt(builder, table, &stmt_wrapper::AssignToExpr(var.expr()), expr)?;
        Ok(StExpr(var.expr(), SideEffects::from(var.access_type)))
      }
      IRExpr::Assign(AssignTarget::InstanceField(lhs, name), expr) => {
        // TODO Weirdness with setget makes this stateful flag not
        // always right? I mean, foo:bar can have side effects if bar
        // is protected by a setget.
        let StExpr(lhs, stateful) = self.compile_expr(builder, table, lhs, NeedsResult::Yes)?;
        let lhs = Expr::Attribute(Box::new(lhs), name.to_owned());
        self.compile_stmt(builder, table, &stmt_wrapper::AssignToExpr(lhs.clone()), expr)?;
        Ok(StExpr(lhs, stateful))
      }
      IRExpr::Array(vec) => {
        let mut side_effects = SideEffects::None;
        let vec = vec.iter().map(|expr| {
          let StExpr(cexpr, state) = self.compile_expr(builder, table, expr, NeedsResult::Yes)?;
          side_effects = max(side_effects, state);
          Ok(cexpr)
        }).collect::<Result<Vec<_>, Error>>()?;
        Ok(StExpr(Expr::ArrayLit(vec), side_effects))
      }
      IRExpr::Quote(ast) => {
        Ok(StExpr(ast.reify(), SideEffects::None))
      }
      IRExpr::FieldAccess(lhs, sym) => {
        let StExpr(lhs, state) = self.compile_expr(builder, table, lhs, NeedsResult::Yes)?;
        let side_effects = max(SideEffects::ReadsState, state);
        Ok(StExpr(Expr::Attribute(Box::new(lhs), names::lisp_to_gd(sym)), side_effects))
      }
      IRExpr::MethodCall(lhs, sym, args) => {
        // Note: No call magic, no optional/rest arguments. When
        // calling a method, we assume all arguments are required, we
        // perform no optimization, we do not check arity, and we
        // simply blindly forward the call on the GDScript side.
        let StExpr(lhs, _) = self.compile_expr(builder, table, lhs, NeedsResult::Yes)?;
        let args = args.iter()
          .map(|arg| self.compile_expr(builder, table, arg, NeedsResult::Yes).map(|x| x.0))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(StExpr(Expr::Call(Some(Box::new(lhs)), names::lisp_to_gd(sym), args), SideEffects::ModifiesState))
      }
      IRExpr::Vector2(x, y) => {
        let StExpr(x, xs) = self.compile_expr(builder, table, x, NeedsResult::Yes)?;
        let StExpr(y, ys) = self.compile_expr(builder, table, y, NeedsResult::Yes)?;
        let side_effects = max(xs, ys);
        Ok(StExpr(Expr::Call(None, String::from("Vector2"), vec!(x, y)), side_effects))
      }
      IRExpr::Vector3(x, y, z) => {
        let StExpr(x, xs) = self.compile_expr(builder, table, x, NeedsResult::Yes)?;
        let StExpr(y, ys) = self.compile_expr(builder, table, y, NeedsResult::Yes)?;
        let StExpr(z, zs) = self.compile_expr(builder, table, z, NeedsResult::Yes)?;
        let side_effects = max(xs, max(ys, zs));
        Ok(StExpr(Expr::Call(None, String::from("Vector3"), vec!(x, y, z)), side_effects))
      }
      /* // This will eventually be an optimization.
      IRExpr::Funcall(f, args) => {
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

  pub fn nil_expr() -> StExpr {
    StExpr(library::nil(), SideEffects::None)
  }

  pub fn name_generator(&mut self) -> &mut FreshNameGenerator<'a> {
    &mut self.gen
  }

  pub fn declare_var(&mut self, builder: &mut StmtBuilder, prefix: &str, value: Option<Expr>)
                     -> String {
    let var_name = self.gen.generate_with(prefix);
    let value = value.unwrap_or(Compiler::nil_expr().0);
    builder.append(Stmt::VarDecl(var_name.clone(), value));
    var_name
  }

  pub fn compile_decl(&mut self,
                      builder: &mut CodeBuilder,
                      table: &mut SymbolTable,
                      decl: &IRDecl)
                      -> Result<(), Error> {
    match decl {
      IRDecl::FnDecl(ir::decl::FnDecl { name, args, body }) => {
        let gd_name = names::lisp_to_gd(&name);
        let function = self.declare_function(builder, table, gd_name, args.clone(), body)?;
        builder.add_decl(Decl::FnDecl(decl::Static::IsStatic, function));
        Ok(())
      }
      IRDecl::MacroDecl(ir::decl::MacroDecl { name, args, body }) => {
        // Note: Macros compile identically to functions, as far as
        // this stage of compilation is concerned. They'll be resolved
        // and then purged during the IR phase.
        let gd_name = names::lisp_to_gd(&name);
        let function = self.declare_function(builder, table, gd_name, args.clone(), body)?;
        builder.add_decl(Decl::FnDecl(decl::Static::IsStatic, function));
        Ok(())
      }
      IRDecl::ConstDecl(ir::decl::ConstDecl { name, value }) => {
        let gd_name = names::lisp_to_gd(&name);
        let mut tmp_builder = StmtBuilder::new();
        let value = self.compile_expr(&mut tmp_builder, table, value, NeedsResult::Yes)?.0;
        let (stmts, decls) = tmp_builder.build();
        if stmts.is_empty() && decls.is_empty() {
          builder.add_decl(Decl::ConstDecl(gd_name, value));
          Ok(())
        } else {
          Err(Error::NotConstantEnough(name.to_owned()))
        }
      }
      IRDecl::ClassDecl(ir::decl::ClassDecl { name, extends, main_class, constructor, decls }) => {
        let gd_name = names::lisp_to_gd(&name);
        let extends = table.get_var(&extends).ok_or_else(|| Error::NoSuchVar(extends.clone()))?.name.clone();
        let extends = Compiler::expr_to_extends(extends)?;
        let class = self.declare_class(builder, table, gd_name, extends, constructor, decls)?;
        if *main_class {
          self.flatten_class_into_main(builder, class);
          Ok(())
        } else {
          builder.add_decl(Decl::ClassDecl(class));
          Ok(())
        }
      }
    }
  }

  fn expr_to_extends(expr: Expr) -> Result<Vec<String>, Error> {
    match expr {
      Expr::Var(v) => {
        Ok(vec!(v))
      }
      Expr::Attribute(lhs, v) => {
        let mut vec = Compiler::expr_to_extends(*lhs)?;
        vec.push(v);
        Ok(vec)
      }
      e => {
        Err(Error::CannotExtend(e.to_gd()))
      }
    }
  }

  // TODO It's an error to have multiple main classes in one file, but we don't currently report this.
  fn flatten_class_into_main(&mut self,
                             builder: &mut CodeBuilder,
                             class: decl::ClassDecl)
                             -> () {
    let decl::ClassDecl { name: _, extends, body } = class;
    builder.extends(extends);
    for decl in body {
      builder.add_decl(decl);
    }
  }

  pub fn declare_function(&mut self,
                          builder: &mut impl HasDecls,
                          table: &mut SymbolTable,
                          gd_name: String,
                          args: IRArgList,
                          body: &IRExpr)
                          -> Result<decl::FnDecl, Error> {
    let local_vars = body.get_locals();
    let (arglist, gd_args) = args.into_gd_arglist(&mut self.gen);
    let mut stmt_builder = StmtBuilder::new();
    for arg in &gd_args {
      if local_vars.get(&arg.0).requires_cell() {
        // Special behavior to wrap the argument in a cell.
        stmt_builder.append(Stmt::Assign(Box::new(Expr::var(&arg.1)),
                                         op::AssignOp::Eq,
                                         Box::new(library::construct_cell(Expr::var(&arg.1)))));
      }
    }
    table.with_local_vars(&mut gd_args.into_iter().map(|x| (x.0.to_owned(), LocalVar::local(x.1, local_vars.get(&x.0)))), |table| {
      self.compile_stmt(&mut stmt_builder, table, &stmt_wrapper::Return, body)
    })?;
    Ok(decl::FnDecl {
      name: gd_name,
      args: arglist,
      body: stmt_builder.build_into(builder),
    })
  }

  pub fn declare_class(&mut self,
                       builder: &mut impl HasDecls,
                       table: &mut SymbolTable,
                       gd_name: String,
                       extends: Vec<String>,
                       constructor: &ir::decl::ConstructorDecl,
                       decls: &[ir::decl::ClassInnerDecl])
                       -> Result<decl::ClassDecl, Error> {

    let self_var = LocalVar {
      name: Expr::var("self"),
      access_type: AccessType::ClosedRead,
      scope: VarScope::LocalVar,
      assignable: false, // Cannot assign to self
    };

    let mut body = vec!();
    table.with_local_var::<Result<(), Error>, _>(String::from("self"), self_var, |table| {
      body.push(Decl::FnDecl(decl::Static::NonStatic, self.compile_constructor(builder, table, constructor)?));
      for d in decls {
        body.push(self.compile_class_inner_decl(builder, table, d)?);
      }
      Ok(())
    })?;

    let decl = decl::ClassDecl {
      name: gd_name,
      extends: decl::ClassExtends::Qualified(extends),
      body: body,
    };
    Ok(decl)
  }

  pub fn compile_constructor(&mut self,
                             builder: &mut impl HasDecls,
                             table: &mut SymbolTable,
                             constructor: &ir::decl::ConstructorDecl)
                             -> Result<decl::FnDecl, Error> {
    // TODO No implicit return on constructor
    self.declare_function(builder,
                          table,
                          String::from(library::CONSTRUCTOR_NAME),
                          IRArgList::from(constructor.args.clone()),
                          &constructor.body)
  }


  pub fn compile_class_inner_decl(&mut self,
                                  builder: &mut impl HasDecls,
                                  table: &mut SymbolTable,
                                  decl: &ir::decl::ClassInnerDecl)
                                  -> Result<Decl, Error> {
    match decl {
      ir::decl::ClassInnerDecl::ClassVarDecl(v) => {
        let name = names::lisp_to_gd(&v.name);
        Ok(Decl::VarDecl(name, None))
      }
      ir::decl::ClassInnerDecl::ClassFnDecl(f) => {
        let gd_name = names::lisp_to_gd(&f.name);
        let func = self.declare_function(builder,
                                         table,
                                         gd_name,
                                         IRArgList::from(f.args.clone()),
                                         &f.body)?;
        Ok(Decl::FnDecl(decl::Static::NonStatic, func))
      }
    }
  }

  fn bind_decl(pipeline: &mut Pipeline,
               table: &mut SymbolTable,
               decl: &IRDecl)
               -> Result<(), Error> {
    match decl {
      IRDecl::FnDecl(ir::decl::FnDecl { name, args, body: _ }) => {
        let func = function_call::FnCall::unqualified(
          function_call::FnSpecs::from(args.to_owned()),
          function_call::FnScope::Global,
          names::lisp_to_gd(name)
        );
        table.set_fn(name.clone(), func, Box::new(DefaultCall));
      }
      IRDecl::MacroDecl(ir::decl::MacroDecl { name, args, body: _ }) => {
        // As above, macros compile basically the same as functions in
        // terms of call semantics and should be resolved during the
        // IR stage.
        let func = function_call::FnCall::unqualified(
          function_call::FnSpecs::from(args.to_owned()),
          function_call::FnScope::Global,
          names::lisp_to_gd(name)
        );
        table.set_fn(name.clone(), func, Box::new(DefaultCall));
      }
      IRDecl::ConstDecl(ir::decl::ConstDecl { name, value: _ }) => {
        let mut var = LocalVar::global(names::lisp_to_gd(name));
        var.assignable = false; // Can't assign to constants
        table.set_var(name.clone(), var);
      }
      IRDecl::ClassDecl(ir::decl::ClassDecl { name, main_class, .. }) => {
        if *main_class {
          let mut filename = pipeline.currently_loading_file().expect("Loading file not recognized").to_owned(); // TODO Expect?
          filename.path_mut().set_extension("gd");
          let expr = Expr::Call(None, String::from("load"), vec!(Expr::from(filename.to_string())));
          let var = LocalVar { name: expr, access_type: AccessType::Read, scope: VarScope::GlobalVar, assignable: false };
          table.set_var(name.clone(), var);
        } else {
          let mut var = LocalVar::global(names::lisp_to_gd(name));
          var.assignable = false;
          table.set_var(name.clone(), var);
        }
      }
    };
    Ok(())
  }

  fn make_preload_line(&self, var: String, path: &RPathBuf) -> Result<Decl, Error> {
    let mut path = path.clone();
    path.path_mut().set_extension("gd");
    let path = self.resolver.resolve_preload(&path).ok_or_else(|| Error::NoSuchFile(path.clone()))?;
    Ok(Decl::ConstDecl(var, Expr::Call(None, String::from("preload"), vec!(Expr::from(path)))))
  }

  fn import_name(&mut self, import: &ImportDecl) -> String {
    let prefix = match &import.details {
      ImportDetails::Named(s) => names::lisp_to_gd(&s),
      ImportDetails::Restricted(_) | ImportDetails::Open => String::from("_Import"),
    };
    self.gen.generate_with(&prefix)
  }

  fn insert_object_into_call(import_name: String, object: Expr) -> Expr {
    // This is really just a hacky approximation. I'm not sure this
    // function even makes total sense in the general case, if given
    // inputs not of the form we expect.
    match object {
      Expr::Var(s) => {
        Expr::Attribute(Box::new(Expr::Var(import_name)), s)
      }
      Expr::Attribute(inner, s) => {
        let inner = Compiler::insert_object_into_call(import_name, *inner);
        Expr::Attribute(Box::new(inner), s)
      }
      Expr::Subscript(inner, rhs) => {
        // Can this case even happen? WHEN would this case even happen?
        let inner = Compiler::insert_object_into_call(import_name, *inner);
        Expr::Subscript(Box::new(inner), rhs)
      }
      _ => {
        // WTF? What do we do here?
        //
        // TODO Make a real error for this and return Result<...> (or
        // restrict what FnCall can contain to be not all Expr but
        // some other simpler type with an Into<Expr>)
        panic!("Invalid import")
      }
    }
  }

  fn translate_call(import_name: String, call: function_call::FnCall) -> function_call::FnCall {
    let object = match call.object {
      None => Expr::Var(import_name),
      Some(x) => Compiler::insert_object_into_call(import_name, *x),
    };
    function_call::FnCall {
      scope: call.scope,
      object: Some(Box::new(object)),
      function: call.function,
      specs: call.specs,
    }
  }

  pub fn resolve_import(&mut self,
                        pipeline: &mut Pipeline,
                        builder: &mut CodeBuilder,
                        table: &mut SymbolTable,
                        import: &ImportDecl)
                        -> Result<(), PError> {
    let preload_name = self.import_name(import);
    builder.add_decl(self.make_preload_line(preload_name.clone(), &import.filename)?);

    // Now add the pertinent symbols to the symbol table
    let unit = pipeline.load_file(&import.filename.path())?;
    let unit_table = &unit.table;
    let exports = &unit.exports;
    let names = import.names(&unit.exports);
    for imp in names {
      let ImportName { namespace: namespace, in_name: import_name, out_name: export_name } = imp;
      match namespace {
        Namespace::Function => {
          let (call, _) = unit_table.get_fn(&export_name).ok_or(Error::NoSuchFn(export_name))?;
          let call = Compiler::translate_call(preload_name.clone(), call.clone());
          table.set_fn(import_name.clone(), call, Box::new(DefaultCall));
        }
        Namespace::Value => {
          let mut var = unit_table.get_var(&export_name).ok_or(Error::NoSuchVar(export_name))?.clone();
          var.name = Compiler::insert_object_into_call(preload_name.clone(), var.name);
          table.set_var(import_name.clone(), var);
        }
      }
    }

    // If it was a restricted import list, validate the import names
    if let ImportDetails::Restricted(vec) = &import.details {
      for imp in vec {
        imp.refine(exports).map_err(Error::from)?;
      }
    }

    Ok(())
  }

  pub fn compile_decls(&mut self,
                       pipeline: &mut Pipeline,
                       builder: &mut CodeBuilder,
                       table: &mut SymbolTable,
                       decls: &[IRDecl])
                       -> Result<(), PError> {
    for decl in decls {
      Compiler::bind_decl(pipeline, table, decl)?;
    }
    for decl in decls {
      self.compile_decl(builder, table, decl)?;
    }
    Ok(())
  }

  pub fn compile_toplevel(&mut self,
                          pipeline: &mut Pipeline,
                          builder: &mut CodeBuilder,
                          table: &mut SymbolTable,
                          toplevel: &ir::decl::TopLevel)
                          -> Result<(), PError> {
    for imp in &toplevel.imports {
      self.resolve_import(pipeline, builder, table, imp)?;
    }
    self.compile_decls(pipeline, builder, table, &toplevel.decls)
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::decl::Decl;
  use crate::sxp::ast::{self, AST};
  use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
  use crate::pipeline::config::ProjectConfig;
  use crate::compile::preload_resolver::DefaultPreloadResolver;

  use std::path::PathBuf;

  // TODO A lot more of this

  fn bind_helper_symbols(table: &mut SymbolTable) {
    // Binds a few helper names to the symbol table for the sake of
    // debugging.
    table.set_fn(String::from("foo1"), FnCall::unqualified(FnSpecs::new(1, 0, None), FnScope::Global, String::from("foo1")), Box::new(DefaultCall));
    table.set_fn(String::from("foo"), FnCall::unqualified(FnSpecs::new(0, 0, None), FnScope::Global, String::from("foo")), Box::new(DefaultCall));
    table.set_fn(String::from("bar"), FnCall::unqualified(FnSpecs::new(0, 0, None), FnScope::Global, String::from("bar")), Box::new(DefaultCall));
    table.set_var(String::from("foobar"), LocalVar::read(String::from("foobar")));
  }

  fn compile_stmt(ast: &AST) -> Result<(Vec<Stmt>, Vec<Decl>), PError> {

    let mut pipeline = Pipeline::new(ProjectConfig { root_directory: PathBuf::from(".") });

    let used_names = ast.all_symbols();
    let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver));
    let mut table = SymbolTable::new();
    bind_helper_symbols(&mut table);
    library::bind_builtins(&mut table);
    let mut builder = StmtBuilder::new();
    let expr = ir::compile_expr(&mut pipeline, ast)?;
    let () = compiler.compile_stmt(&mut builder, &mut table, &mut stmt_wrapper::Return, &expr)?;
    Ok(builder.build())
  }

  #[test]
  fn compile_var() {
    let ast = AST::Symbol(String::from("foobar"));
    let expected = Stmt::ReturnStmt(Expr::Var(String::from("foobar")));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_call() {
    let ast = ast::list(vec!(AST::Symbol(String::from("foo1")), AST::Int(10)));
    let expected = Stmt::ReturnStmt(Expr::Call(None, String::from("foo1"), vec!(Expr::from(10))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_int() {
    let ast = AST::Int(99);
    let expected = Stmt::ReturnStmt(Expr::from(99));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_bool_t() {
    let ast = AST::Bool(true);
    let expected = Stmt::ReturnStmt(Expr::from(true));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_bool_f() {
    let ast = AST::Bool(false);
    let expected = Stmt::ReturnStmt(Expr::from(false));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_string() {
    let ast = AST::String(String::from("foobar"));
    let expected = Stmt::ReturnStmt(Expr::from("foobar".to_owned()));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_vacuous() {
    let ast = ast::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1), AST::Int(2)));
    let expected = vec!(Stmt::ReturnStmt(Expr::from(2)));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_stateful() {
    let ast = ast::list(vec!(AST::Symbol(String::from("progn")),
                             ast::list(vec!(AST::Symbol(String::from("foo")))),
                             ast::list(vec!(AST::Symbol(String::from("bar"))))));
    let expected = vec!(Stmt::Expr(Expr::Call(None, String::from("foo"), vec!())),
                        Stmt::ReturnStmt(Expr::Call(None, String::from("bar"), vec!())));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_nil() {
    let result1 = compile_stmt(&AST::Nil).unwrap();
    assert_eq!(result1, (vec!(Stmt::ReturnStmt(Compiler::nil_expr().0)), vec!()));

    let result2 = compile_stmt(&ast::list(vec!(AST::Symbol(String::from("progn"))))).unwrap();
    assert_eq!(result2, (vec!(Stmt::ReturnStmt(Compiler::nil_expr().0)), vec!()));
  }

}
