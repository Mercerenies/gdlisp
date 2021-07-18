
pub mod names;
pub mod body;
pub mod error;
pub mod stmt_wrapper;
pub mod symbol_table;
pub mod special_form;
pub mod stateful;
pub mod preload_resolver;
pub mod resource_type;
pub mod constant;

use body::builder::{CodeBuilder, StmtBuilder, HasDecls};
use names::fresh::FreshNameGenerator;
use preload_resolver::PreloadResolver;
use constant::MaybeConstant;
use crate::sxp::reify::Reify;
use crate::gdscript::literal::Literal;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::decl::{self, Decl, ClassExtends};
use crate::gdscript::op;
use crate::gdscript::library;
use crate::gdscript::arglist::ArgList;
use crate::gdscript::inner_class::{self, NeedsOuterClassRef};
use error::Error;
use stmt_wrapper::StmtWrapper;
use symbol_table::{HasSymbolTable, SymbolTable, ClassTablePair};
use symbol_table::local_var::{LocalVar, ValueHint};
use symbol_table::function_call;
use symbol_table::call_magic::{CallMagic, DefaultCall};
use symbol_table::call_magic::table::MagicTable;
use crate::ir;
use crate::ir::expr::{FuncRefTarget, AssignTarget};
use crate::ir::import::{ImportName, ImportDecl, ImportDetails};
use crate::ir::identifier::Namespace;
use crate::runner::path::RPathBuf;
use crate::pipeline::error::{Error as PError};
use crate::pipeline::Pipeline;
use crate::pipeline::can_load::CanLoad;
use special_form::lambda;
use special_form::flet;
use special_form::lambda_class;
use stateful::{StExpr, NeedsResult, SideEffects};
use resource_type::ResourceType;

use std::ffi::OsStr;
use std::cmp::max;
use std::convert::TryFrom;

type IRDecl = ir::decl::Decl;
type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;
type IRLiteral = ir::literal::Literal;

pub struct Compiler<'a> {
  gen: FreshNameGenerator<'a>,
  resolver: Box<dyn PreloadResolver>,
  magic_table: MagicTable,
}

impl<'a> Compiler<'a> {

  pub fn new(gen: FreshNameGenerator<'a>, resolver: Box<dyn PreloadResolver>) -> Compiler<'a> {
    let magic_table = library::magic::standard_magic_table();
    Compiler { gen, resolver, magic_table }
  }

  pub fn compile_stmts(&mut self,
                       pipeline: &mut Pipeline,
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
        self.compile_stmt(pipeline, builder, table, &stmt_wrapper::Vacuous, x)?;
      }
      self.compile_expr(pipeline, builder, table, end, needs_result)
    }
  }

  pub fn compile_stmt(&mut self,
                      pipeline: &mut Pipeline,
                      builder: &mut StmtBuilder,
                      table: &mut SymbolTable,
                      destination: &dyn StmtWrapper,
                      stmt: &IRExpr)
                      -> Result<(), Error> {
    let needs_result = NeedsResult::from(!destination.is_vacuous());
    let expr = self.compile_expr(pipeline, builder, table, stmt, needs_result)?;
    destination.wrap_to_builder(builder, expr);
    Ok(())
  }

  pub fn compile_expr(&mut self,
                      pipeline: &mut Pipeline,
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
            Ok(StExpr(Expr::Call(Some(Box::new(library::gdlisp_root())), String::from("intern"), vec!(s.reify())),
                      SideEffects::None)),
        }
      }
      IRExpr::Progn(body) => {
        let body: Vec<_> = body.iter().collect();
        self.compile_stmts(pipeline, builder, table, &body[..], needs_result)
      }
      IRExpr::CondStmt(clauses) => {
        special_form::compile_cond_stmt(self, pipeline, builder, table, clauses, needs_result)
      }
      IRExpr::WhileStmt(cond, body) => {
        special_form::compile_while_stmt(self, pipeline, builder, table, cond, body, needs_result)
      }
      IRExpr::ForStmt(name, iter, body) => {
        special_form::compile_for_stmt(self, pipeline, builder, table, &*name, iter, body, needs_result)
      }
      IRExpr::Call(f, args) => {
        let (fcall, call_magic) = match table.get_fn(f) {
          None => return Err(Error::NoSuchFn(f.clone())),
          Some((p, m)) => (p.clone(), dyn_clone::clone_box(m))
        };
        // Macro calls should not occur at this stage in compilation.
        if fcall.is_macro {
          panic!("Macro call failed to resolve in IR; reached compile stage (this is a bug in the GDLisp compiler)");
        }
        // Call magic is used to implement some commonly used wrappers
        // for simple GDScript operations.
        let args = args.iter()
                       .map(|x| self.compile_expr(pipeline, builder, table, x, NeedsResult::Yes))
                       .collect::<Result<Vec<_>, _>>()?;
        Ok(StExpr(fcall.into_expr_with_magic(&*call_magic, self, builder, table, args)?, SideEffects::ModifiesState))
      }
      IRExpr::Let(clauses, body) => {
        let closure_vars = body.get_locals();
        let var_names = clauses.iter().map::<Result<(String, String), Error>, _>(|clause| {
          let (ast_name, expr) = clause;
          let ast_name = ast_name.to_owned();
          let result_value = self.compile_expr(pipeline, builder, table, &expr, NeedsResult::Yes)?.0;
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
          self.compile_expr(pipeline, builder, table, body, needs_result)
        })
      }
      IRExpr::FLet(clauses, body) => {
        flet::compile_flet(self, pipeline, builder, table, clauses, body, needs_result)
      }
      IRExpr::Labels(clauses, body) => {
        flet::compile_labels(self, pipeline, builder, table, clauses, body, needs_result)
      }
      IRExpr::Lambda(args, body) => {
        lambda::compile_lambda_stmt(self, pipeline, builder, table, args, body)
      }
      IRExpr::FuncRef(name) => {
        match name {
          FuncRefTarget::SimpleName(name) => {
            let func = table.get_fn(name).ok_or_else(|| Error::NoSuchFn(name.clone()))?.0.clone();
            lambda::compile_function_ref(self, pipeline, builder, table, func)
          }
        }
      }
      IRExpr::Assign(AssignTarget::Variable(name), expr) => {
        let var = table.get_var(name).ok_or_else(|| Error::NoSuchVar(name.clone()))?.to_owned();
        if !var.assignable {
          return Err(Error::CannotAssignTo(var.name.to_gd()));
        }
        self.compile_stmt(pipeline, builder, table, &stmt_wrapper::AssignToExpr(var.expr()), expr)?;
        Ok(StExpr(var.expr(), SideEffects::from(var.access_type)))
      }
      IRExpr::Assign(AssignTarget::InstanceField(lhs, name), expr) => {
        // TODO Weirdness with setget makes this stateful flag not
        // always right? I mean, foo:bar can have side effects if bar
        // is protected by a setget.
        let StExpr(mut lhs, stateful) = self.compile_expr(pipeline, builder, table, lhs, NeedsResult::Yes)?;
        // Assign to a temp if it's stateful
        if needs_result == NeedsResult::Yes && stateful.modifies_state() {
          let var = self.declare_var(builder, "_assign", Some(lhs));
          lhs = Expr::Var(var);
        }
        let lhs = Expr::Attribute(Box::new(lhs), names::lisp_to_gd(name));
        self.compile_stmt(pipeline, builder, table, &stmt_wrapper::AssignToExpr(lhs.clone()), expr)?;
        if needs_result == NeedsResult::Yes {
          Ok(StExpr(lhs, SideEffects::None))
        } else {
          Ok(Compiler::nil_expr())
        }
      }
      IRExpr::Array(vec) => {
        let mut side_effects = SideEffects::None;
        let vec = vec.iter().map(|expr| {
          let StExpr(cexpr, state) = self.compile_expr(pipeline, builder, table, expr, NeedsResult::Yes)?;
          side_effects = max(side_effects, state);
          Ok(cexpr)
        }).collect::<Result<Vec<_>, Error>>()?;
        Ok(StExpr(Expr::ArrayLit(vec), side_effects))
      }
      IRExpr::Dictionary(vec) => {
        let mut side_effects = SideEffects::None;
        let vec = vec.iter().map(|(k, v)| {

          let StExpr(kexpr, kstate) = self.compile_expr(pipeline, builder, table, k, NeedsResult::Yes)?;
          side_effects = max(side_effects, kstate);

          let StExpr(vexpr, vstate) = self.compile_expr(pipeline, builder, table, v, NeedsResult::Yes)?;
          side_effects = max(side_effects, vstate);

          Ok((kexpr, vexpr))
        }).collect::<Result<Vec<_>, Error>>()?;
        Ok(StExpr(Expr::DictionaryLit(vec), side_effects))
      }
      IRExpr::Quote(ast) => {
        Ok(StExpr(ast.reify(), SideEffects::None))
      }
      IRExpr::FieldAccess(lhs, sym) => {

        // This is a special case to validate enum names, as an extra sanity check.
        if let IRExpr::LocalVar(lhs) = &**lhs {
          if let Some(LocalVar { value_hint, .. }) = table.get_var(lhs) {
            if let Some(ValueHint::Enum(vs)) = value_hint {
              // It's an enum and we know its values; validate
              if !vs.contains(&names::lisp_to_gd(sym)) {
                return Err(Error::NoSuchEnumValue(lhs.clone(), sym.clone()));
              }
            }
          }
        }

        let StExpr(lhs, state) = self.compile_expr(pipeline, builder, table, lhs, NeedsResult::Yes)?;
        let side_effects = max(SideEffects::ReadsState, state);
        Ok(StExpr(Expr::Attribute(Box::new(lhs), names::lisp_to_gd(sym)), side_effects))

      }
      IRExpr::MethodCall(lhs, sym, args) => {
        // Note: No call magic, no optional/rest arguments. When
        // calling a method, we assume all arguments are required, we
        // perform no optimization, we do not check arity, and we
        // simply blindly forward the call on the GDScript side.
        let StExpr(lhs, _) = self.compile_expr(pipeline, builder, table, lhs, NeedsResult::Yes)?;
        let args = args.iter()
          .map(|arg| self.compile_expr(pipeline, builder, table, arg, NeedsResult::Yes).map(|x| x.0))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(StExpr(Expr::Call(Some(Box::new(lhs)), names::lisp_to_gd(sym), args), SideEffects::ModifiesState))
      }
      IRExpr::LambdaClass(cls) => {
        lambda_class::compile_lambda_class(self, pipeline, builder, table, cls)
      }
      IRExpr::Yield(arg) => {
        match arg {
          None => Ok(StExpr(Expr::yield_expr(None), SideEffects::ModifiesState)),
          Some((x, y)) => {
            let StExpr(x, _) = self.compile_expr(pipeline, builder, table, x, NeedsResult::Yes)?;
            let StExpr(y, _) = self.compile_expr(pipeline, builder, table, y, NeedsResult::Yes)?;
            Ok(StExpr(Expr::yield_expr(Some((x, y))), SideEffects::ModifiesState))
          }
        }
      }
      IRExpr::Return(expr) => {
        self.compile_stmt(pipeline, builder, table, &stmt_wrapper::Return, expr)?;
        Ok(Compiler::nil_expr())
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

  // Compile an expression, but fail if a builder is required for
  // helper stmts or decls.
  pub fn compile_simple_expr(&mut self,
                             pipeline: &mut Pipeline,
                             table: &mut SymbolTable,
                             src_name: &str,
                             expr: &IRExpr,
                             needs_result: NeedsResult)
                             -> Result<Expr, Error> {
    let mut tmp_builder = StmtBuilder::new();
    let value = self.compile_expr(pipeline, &mut tmp_builder, table, expr, needs_result)?.0;
    let (stmts, decls) = tmp_builder.build();
    if stmts.is_empty() && decls.is_empty() {
      Ok(value)
    } else {
      Err(Error::NotConstantEnough(String::from(src_name)))
    }
  }

  pub fn nil_expr() -> StExpr {
    StExpr(library::nil(), SideEffects::None)
  }

  pub fn name_generator(&mut self) -> &mut FreshNameGenerator<'a> {
    &mut self.gen
  }

  pub fn preload_resolver(&self) -> &dyn PreloadResolver {
    &*self.resolver
  }

  pub fn declare_var(&mut self, builder: &mut StmtBuilder, prefix: &str, value: Option<Expr>)
                     -> String {
    let var_name = self.gen.generate_with(prefix);
    let value = value.unwrap_or(Compiler::nil_expr().0);
    builder.append(Stmt::VarDecl(var_name.clone(), value));
    var_name
  }

  pub fn compile_decl(&mut self,
                      pipeline: &mut Pipeline,
                      builder: &mut CodeBuilder,
                      table: &mut SymbolTable,
                      decl: &IRDecl)
                      -> Result<(), Error> {
    match decl {
      IRDecl::FnDecl(ir::decl::FnDecl { visibility: _, call_magic: _, name, args, body }) => {
        let gd_name = names::lisp_to_gd(&name);
        let function = self.declare_function(pipeline, builder, table, gd_name, args.clone(), body, &stmt_wrapper::Return)?;
        builder.add_decl(Decl::FnDecl(decl::Static::IsStatic, function));
        Ok(())
      }
      IRDecl::MacroDecl(ir::decl::MacroDecl { visibility: _, name, args, body }) => {
        // Note: Macros compile identically to functions, as far as
        // this stage of compilation is concerned. They'll be resolved
        // and then purged during the IR phase.
        let gd_name = names::lisp_to_gd(&name);
        let function = self.declare_function(pipeline, builder, table, gd_name, args.clone(), body, &stmt_wrapper::Return)?;
        builder.add_decl(Decl::FnDecl(decl::Static::IsStatic, function));
        Ok(())
      }
      IRDecl::ConstDecl(ir::decl::ConstDecl { visibility: _, name, value }) => {
        let gd_name = names::lisp_to_gd(&name);
        let value = self.compile_simple_expr(pipeline, table, name, value, NeedsResult::Yes)?;
        value.validate_const_expr(&name, table)?;
        builder.add_decl(Decl::ConstDecl(gd_name, value));
        Ok(())
      }
      IRDecl::ClassDecl(ir::decl::ClassDecl { visibility: _, name, extends, main_class, constructor, decls }) => {
        let gd_name = names::lisp_to_gd(&name);
        let extends = table.get_var(&extends).ok_or_else(|| Error::NoSuchVar(extends.clone()))?.name.clone();
        let extends = ClassExtends::try_from(extends)?;
        let class = self.declare_class(pipeline, builder, table, gd_name, extends, *main_class, constructor, decls)?;
        if *main_class {
          self.flatten_class_into_main(builder, class);
          Ok(())
        } else {
          builder.add_decl(Decl::ClassDecl(class));
          Ok(())
        }
      }
      IRDecl::EnumDecl(ir::decl::EnumDecl { visibility: _, name, clauses }) => {
        let gd_name = names::lisp_to_gd(&name);
        let gd_clauses = clauses.iter().map(|(const_name, const_value)| {
          let gd_const_name = names::lisp_to_gd(const_name);
          let gd_const_value = const_value.as_ref().map(|x| self.compile_simple_expr(pipeline, table, const_name, x, NeedsResult::Yes)).transpose()?;
          if let Some(gd_const_value) = &gd_const_value {
            gd_const_value.validate_const_expr(const_name, table)?;
          }
          Ok((gd_const_name, gd_const_value))
        }).collect::<Result<_, Error>>()?;
        builder.add_decl(Decl::EnumDecl(decl::EnumDecl { name: Some(gd_name), clauses: gd_clauses }));
        Ok(())
      }
      IRDecl::DeclareDecl(_) => {
        // (sys/declare ...) statements have no runtime presence and do
        // nothing here.
        Ok(())
      }
    }
  }

  // TODO It's an error to have multiple main classes in one file, but we don't currently report this.
  fn flatten_class_into_main(&mut self,
                             builder: &mut CodeBuilder,
                             class: decl::ClassDecl) {
    let decl::ClassDecl { name: _, extends, body } = class;
    builder.extends(extends);
    for decl in body {
      builder.add_decl(decl);
    }
  }

  pub fn declare_function(&mut self,
                          pipeline: &mut Pipeline,
                          builder: &mut impl HasDecls,
                          table: &mut SymbolTable,
                          gd_name: String,
                          args: IRArgList,
                          body: &IRExpr,
                          result_destination: &impl StmtWrapper)
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
      self.compile_stmt(pipeline, &mut stmt_builder, table, result_destination, body)
    })?;
    Ok(decl::FnDecl {
      name: gd_name,
      args: arglist,
      body: stmt_builder.build_into(builder),
    })
  }

  pub fn declare_class(&mut self,
                       pipeline: &mut Pipeline,
                       builder: &mut impl HasDecls,
                       table: &mut SymbolTable,
                       gd_name: String,
                       extends: ClassExtends,
                       main_class: bool,
                       constructor: &ir::decl::ConstructorDecl,
                       decls: &[ir::decl::ClassInnerDecl])
                       -> Result<decl::ClassDecl, Error> {

    let self_var = LocalVar::self_var();

    let mut body = vec!();
    let mut outer_ref_name = String::new();
    let needs_outer_ref =
      constructor.needs_outer_class_ref(table) || decls.iter().any(|x| x.needs_outer_class_ref(table));
    if needs_outer_ref && !main_class {
      outer_ref_name = self.gen.generate_with(inner_class::OUTER_REFERENCE_NAME);
    }

    let mut instance_table = table.clone();
    let mut static_table = table.clone();
    instance_table.with_local_var::<Result<(), Error>, _>(String::from("self"), self_var, |instance_table| {

      // Modify all of the names in the instance / static table. We
      // run this even if needs_outer_ref is false, because we might
      // still need the static_table updates, and the instance_table
      // updates will be harmlessly ignored in that case.
      if !main_class {
        for (_, call, _) in instance_table.fns_mut() {
          call.object.update_for_inner_scope(false, self.preload_resolver(), pipeline, &outer_ref_name);
        }
        for (_, call, _) in static_table.fns_mut() {
          call.object.update_for_inner_scope(true, self.preload_resolver(), pipeline, &outer_ref_name);
        }
      }

      body.push(Decl::FnDecl(decl::Static::NonStatic, self.compile_constructor(pipeline, builder, instance_table, constructor)?));

      for d in decls {
        let tables = ClassTablePair { instance_table, static_table: &mut static_table };
        body.push(self.compile_class_inner_decl(pipeline, builder, tables, d)?);
      }

      Ok(())
    })?;

    let mut decl = decl::ClassDecl {
      name: gd_name,
      extends: extends,
      body: body,
    };
    if needs_outer_ref && !main_class {
      inner_class::add_outer_class_ref_named(&mut decl, self.preload_resolver(), pipeline, outer_ref_name);
    }
    Ok(decl)
  }

  pub fn compile_constructor(&mut self,
                             pipeline: &mut Pipeline,
                             builder: &mut impl HasDecls,
                             table: &mut SymbolTable,
                             constructor: &ir::decl::ConstructorDecl)
                             -> Result<decl::FnDecl, Error> {
    self.declare_function(pipeline,
                          builder,
                          table,
                          String::from(library::CONSTRUCTOR_NAME),
                          IRArgList::from(constructor.args.clone()),
                          &constructor.body,
                          &stmt_wrapper::Vacuous)
  }

  fn compile_export(&mut self,
                    pipeline: &mut Pipeline,
                    table: &mut SymbolTable,
                    expr: &IRExpr) -> Result<Expr, Error> {
    // Any expression valid as a const is valid here, but then so are
    // Expr::LocalVar since we need to allow type names.
    //
    // TODO Validate that the local vars appearing here make sense.
    match expr {
      IRExpr::LocalVar(s) => Ok(Expr::Var(s.to_owned())),
      _ => {
        let expr = self.compile_simple_expr(pipeline, table, "export", expr, NeedsResult::Yes)?;
        expr.validate_const_expr("export", table)?;
        Ok(expr)
      }
    }
  }

  pub fn compile_class_inner_decl(&mut self,
                                  pipeline: &mut Pipeline,
                                  builder: &mut impl HasDecls,
                                  tables: ClassTablePair<'_, '_>,
                                  decl: &ir::decl::ClassInnerDecl)
                                  -> Result<Decl, Error> {
    let table = tables.into_table(decl.is_static());
    match decl {
      ir::decl::ClassInnerDecl::ClassSignalDecl(s) => {
        let name = names::lisp_to_gd(&s.name);
        let args = s.args.args.iter().map(|x| names::lisp_to_gd(&x)).collect();
        Ok(Decl::SignalDecl(name, ArgList::required(args)))
      }
      ir::decl::ClassInnerDecl::ClassConstDecl(c) => {
        // TODO Merge this with IRDecl::ConstDecl above
        let gd_name = names::lisp_to_gd(&c.name);
        let value = self.compile_simple_expr(pipeline, table, &c.name, &c.value, NeedsResult::Yes)?;
        value.validate_const_expr(&c.name, table)?;
        Ok(Decl::ConstDecl(gd_name, value))
      }
      ir::decl::ClassInnerDecl::ClassVarDecl(v) => {
        let exports = v.export.as_ref().map(|export| {
          export.args.iter().map(|expr| self.compile_export(pipeline, table, expr)).collect::<Result<Vec<_>, _>>()
        }).transpose()?;
        let exports = exports.map(|args| decl::Export { args });
        let name = names::lisp_to_gd(&v.name);
        let value = v.value.as_ref().map::<Result<_, Error>, _>(|expr| {
          let value = self.compile_simple_expr(pipeline, table, &name, expr, NeedsResult::Yes)?;
          value.validate_const_expr(&v.name, table)?;
          Ok(value)
        }).transpose()?;
        Ok(Decl::VarDecl(exports, name, value))
      }
      ir::decl::ClassInnerDecl::ClassFnDecl(f) => {
        let gd_name = names::lisp_to_gd(&f.name);
        let func = self.declare_function(pipeline,
                                         builder,
                                         table,
                                         gd_name,
                                         IRArgList::from(f.args.clone()),
                                         &f.body,
                                         &stmt_wrapper::Return)?;
        Ok(Decl::FnDecl(f.is_static, func))
      }
    }
  }

  fn bind_decl(magic_table: &MagicTable,
               pipeline: &mut Pipeline,
               table: &mut SymbolTable,
               decl: &IRDecl)
               -> Result<(), Error> {
    match decl {
      IRDecl::FnDecl(ir::decl::FnDecl { visibility: _, call_magic, name, args, body: _ }) => {
        let func = function_call::FnCall::file_constant(
          function_call::FnSpecs::from(args.to_owned()),
          function_call::FnScope::Global,
          names::lisp_to_gd(name)
        );
        let call_magic: Box<dyn CallMagic> = match call_magic {
          None => Box::new(DefaultCall),
          Some(m) => {
            // If a call magic declaration was specified, it MUST
            // exist or it's a compile error.
            match magic_table.get(m) {
              None => return Err(Error::NoSuchMagic(m.to_owned())),
              Some(magic) => dyn_clone::clone_box(magic),
            }
          }
        };
        table.set_fn(name.clone(), func, call_magic);
      }
      IRDecl::MacroDecl(ir::decl::MacroDecl { visibility: _, name, args, body: _ }) => {
        // As above, macros compile basically the same as functions in
        // terms of call semantics and should be resolved during the
        // IR stage.
        let func = function_call::FnCall::file_macro(
          function_call::FnSpecs::from(args.to_owned()),
          function_call::FnScope::Global,
          names::lisp_to_gd(name)
        );
        table.set_fn(name.clone(), func, Box::new(DefaultCall));
      }
      IRDecl::ConstDecl(ir::decl::ConstDecl { visibility: _, name, value }) => {
        let mut var = LocalVar::file_constant(names::lisp_to_gd(name)); // Can't assign to constants
        if let IRExpr::Literal(value) = value {
          if let Ok(value) = Literal::try_from(value.clone()) {
            var = var.with_hint(ValueHint::Literal(value));
          }
        }
        table.set_var(name.clone(), var);
      }
      IRDecl::ClassDecl(ir::decl::ClassDecl { visibility: _, name, main_class, .. }) => {
        if *main_class {
          let var = LocalVar::current_file(pipeline.current_filename().expect("Could not identify current filename").to_string()).with_hint(ValueHint::ClassName); // TODO Expect?
          table.set_var(name.clone(), var);
        } else {
          let var = LocalVar::file_constant(names::lisp_to_gd(name))
            .no_assign() // Can't assign to class names
            .with_hint(ValueHint::ClassName);
          table.set_var(name.clone(), var);
        }
      }
      IRDecl::EnumDecl(edecl) => {
        let name = edecl.name.clone();
        let var = LocalVar::file_constant(names::lisp_to_gd(&name))
          .no_assign() // Can't assign to constants
          .with_hint(ValueHint::enumeration(edecl.value_names()));
        table.set_var(name, var);
      }
      IRDecl::DeclareDecl(ddecl) => {
        let ir::decl::DeclareDecl { visibility: _, declare_type, name } = ddecl;
        match declare_type {
          ir::decl::DeclareType::Value => {
            let var = LocalVar::file_constant(names::lisp_to_gd(name));
            table.set_var(name.clone(), var);
          }
          ir::decl::DeclareType::Superglobal => {
            let var = LocalVar::superglobal(names::lisp_to_gd(name))
              .with_hint(ValueHint::Superglobal);
            table.set_var(name.clone(), var);
          }
          ir::decl::DeclareType::Function(args) => {
            let func = function_call::FnCall::file_constant(
              function_call::FnSpecs::from(args.to_owned()),
              function_call::FnScope::Global,
              names::lisp_to_gd(name)
            );
            table.set_fn(name.clone(), func, Box::new(DefaultCall));
          }
          ir::decl::DeclareType::SuperglobalFn(args) => {
            let func = function_call::FnCall::file_constant(
              function_call::FnSpecs::from(args.to_owned()),
              function_call::FnScope::Superglobal,
              names::lisp_to_gd(name)
            );
            table.set_fn(name.clone(), func, Box::new(DefaultCall));
          }
        }
      }
    };
    Ok(())
  }

  fn make_preload_line(&self, var: String, path: &RPathBuf) -> Result<Decl, Error> {
    if self.resolver.include_resource(ResourceType::from(path.path())) {
      let mut path = path.clone();
      if path.path().extension() == Some(OsStr::new("lisp")) {
        path.path_mut().set_extension("gd");
      }
      let path = self.resolver.resolve_preload(&path).ok_or_else(|| Error::NoSuchFile(path.clone()))?;
      Ok(Decl::ConstDecl(var, Expr::Call(None, String::from("preload"), vec!(Expr::from(path)))))
    } else {
      // We null out any resources we don't understand. This means
      // that GDScript source files (those NOT written in GDLisp) and
      // other resources like PackedScene instances cannot be used in
      // macros, as they'll just be seen as "null" during macro
      // resolution. I do not verify that you follow this rule; you
      // are expected to be responsible with your macro resource
      // usage.
      Ok(Decl::ConstDecl(var, Expr::Literal(Literal::Null)))
    }
  }

  fn import_name(&mut self, import: &ImportDecl) -> String {
    let prefix = match &import.details {
      ImportDetails::Named(s) => names::lisp_to_gd(&s),
      ImportDetails::Restricted(_) | ImportDetails::Open => String::from("_Import"),
    };
    self.gen.generate_with(&prefix)
  }

  pub fn translate_call(import_name: String, mut call: function_call::FnCall) -> function_call::FnCall {
    if call.scope != function_call::FnScope::Superglobal {
      call.object = call.object.into_imported(import_name);
    }
    call
  }

  pub fn resolve_import(&mut self,
                        pipeline: &mut Pipeline,
                        builder: &mut CodeBuilder,
                        table: &mut SymbolTable,
                        import: &ImportDecl)
                        -> Result<(), PError> {
    let preload_name = self.import_name(import);
    builder.add_decl(self.make_preload_line(preload_name.clone(), &import.filename)?);
    let res_type = ResourceType::from(import);

    ResourceType::check_import(pipeline, import)?;

    if res_type == ResourceType::GDLispSource {
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
            var.name = var.name.into_imported(preload_name.clone());
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
    } else {
      // Simple resource import
      let name = match &import.details {
        ImportDetails::Named(s) => s.to_owned(),
        _ => return Err(PError::from(Error::InvalidImportOnResource(import.filename.to_string()))),
      };
      let var = LocalVar::file_constant(preload_name);
      // TODO Value hint? It would have to be based on the file extension / what resource type it is.
      // (e.g. *.gd would be ClassHint, but *.tres or *.png would not be)
      table.set_var(name, var);
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
      Compiler::bind_decl(&self.magic_table, pipeline, table, decl)?;
    }
    for decl in decls {
      self.compile_decl(pipeline, builder, table, decl)?;
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
  use crate::sxp::ast::AST;
  use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
  use crate::pipeline::config::ProjectConfig;
  use crate::compile::preload_resolver::DefaultPreloadResolver;

  use std::path::PathBuf;

  // TODO A lot more of this

  fn bind_helper_symbols(table: &mut SymbolTable) {
    // Binds a few helper names to the symbol table for the sake of
    // debugging.
    table.set_fn(String::from("foo1"), FnCall::file_constant(FnSpecs::new(1, 0, None), FnScope::Global, String::from("foo1")), Box::new(DefaultCall));
    table.set_fn(String::from("foo"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("foo")), Box::new(DefaultCall));
    table.set_fn(String::from("bar"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("bar")), Box::new(DefaultCall));
    table.set_var(String::from("foobar"), LocalVar::read(String::from("foobar")));
  }

  fn compile_stmt(ast: &AST) -> Result<(Vec<Stmt>, Vec<Decl>), PError> {

    let mut pipeline = Pipeline::new(ProjectConfig { root_directory: PathBuf::from("."), optimizations: false });

    let used_names = ast.all_symbols();
    let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver));
    let mut table = SymbolTable::new();
    bind_helper_symbols(&mut table);
    library::bind_builtins(&mut table, true);
    let mut builder = StmtBuilder::new();
    let expr = ir::compile_expr(&mut pipeline, ast)?;
    let () = compiler.compile_stmt(&mut pipeline, &mut builder, &mut table, &mut stmt_wrapper::Return, &expr)?;
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
    let ast = AST::list(vec!(AST::Symbol(String::from("foo1")), AST::Int(10)));
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
    let ast = AST::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1), AST::Int(2)));
    let expected = vec!(Stmt::ReturnStmt(Expr::from(2)));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_stateful() {
    let ast = AST::list(vec!(AST::Symbol(String::from("progn")),
                             AST::list(vec!(AST::Symbol(String::from("foo")))),
                             AST::list(vec!(AST::Symbol(String::from("bar"))))));
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

    let result2 = compile_stmt(&AST::list(vec!(AST::Symbol(String::from("progn"))))).unwrap();
    assert_eq!(result2, (vec!(Stmt::ReturnStmt(Compiler::nil_expr().0)), vec!()));
  }

}
