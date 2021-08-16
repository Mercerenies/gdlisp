
use crate::ir;
use crate::ir::expr::LambdaClass;
use crate::compile::error::{Error, ErrorF};
use crate::compile::factory;
use crate::compile::Compiler;
use crate::compile::stateful::{SideEffects, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::{SymbolTable, ClassTablePair};
use crate::compile::symbol_table::local_var::{VarScope, LocalVar};
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::decl::{self, Decl, DeclF};
use crate::gdscript::inner_class::{self, NeedsOuterClassRef};
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use super::lambda;

use std::convert::TryFrom;

pub fn compile_lambda_class(compiler: &mut Compiler,
                            pipeline: &mut Pipeline,
                            builder: &mut StmtBuilder,
                            table: &mut SymbolTable,
                            class: &LambdaClass,
                            pos: SourceOffset)
                            -> Result<StExpr, Error> {
  let LambdaClass { extends, args: constructor_args, constructor, decls } = class.clone();

  // Validate the extends declaration (must be a global variable)
  let extends_var = table.get_var(&extends).ok_or_else(|| Error::new(ErrorF::NoSuchVar(extends.clone()), pos))?;
  if extends_var.scope != VarScope::GlobalVar {
    return Err(Error::new(ErrorF::CannotExtend(extends.clone()), pos));
  }
  let extends = extends_var.name.clone();
  let extends = decl::ClassExtends::try_from(extends).map_err(|x| Error::from_value(x, pos))?;

  // New GD name
  let gd_class_name = compiler.name_generator().generate_with("_AnonymousClass");

  let (mut closure_vars, mut closure_fns) = constructor.get_names();
  for d in &decls {
    let (decl_vars, decl_fns) = d.get_names();
    closure_vars.merge_with(decl_vars);
    closure_fns.merge_with(decl_fns);
  }
  closure_vars.remove("self"); // Don't close around self; we get a new self.

  let mut lambda_table = SymbolTable::new();

  let mut lambda_static_table = lambda_table.clone();
  lambda_table.set_var(String::from("self"), LocalVar::self_var());

  let mut outer_ref_name = String::new();
  let needs_outer_ref = closure_fns.needs_outer_class_ref(table);
  if needs_outer_ref {
    outer_ref_name = compiler.name_generator().generate_with(inner_class::OUTER_REFERENCE_NAME);
  }

  lambda::purge_globals(&mut closure_vars, table);

  lambda::locally_bind_vars(compiler, table, &mut lambda_table, closure_vars.names(), pos)?;
  lambda::locally_bind_fns(compiler, pipeline, table, &mut lambda_table, closure_fns.names(), pos, false, &outer_ref_name)?;
  lambda::copy_global_vars(table, &mut lambda_table);

  lambda::locally_bind_vars(compiler, table, &mut lambda_static_table, closure_vars.names(), pos)?;
  lambda::locally_bind_fns(compiler, pipeline, table, &mut lambda_static_table, closure_fns.names(), pos, true, &outer_ref_name)?;
  lambda::copy_global_vars(table, &mut lambda_static_table);

  let mut gd_src_closure_vars = Vec::new();
  let mut gd_closure_vars = Vec::new();
  for ast_name in closure_vars.names() {
    let var = lambda_table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda class variable {}", ast_name)
    }).to_owned();
    if let Some(name) = var.simple_name() {
      gd_closure_vars.push(name.to_owned());
    }
    let src_var = table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda class variable {}", ast_name)
    }).to_owned();
    if let Some(name) = src_var.simple_name() {
      gd_src_closure_vars.push(name.to_owned());
    }
  }
  for func in closure_fns.names() {
    match table.get_fn(func) {
      None => { return Err(Error::new(ErrorF::NoSuchFn(func.to_owned()), pos)) }
      Some((call, _)) => {
        if let Some(var) = lambda::closure_fn_to_gd_var(call) {
          gd_closure_vars.push(var.to_owned());
          gd_src_closure_vars.push(var);
        }
      }
    }
  }

  let mut constructor = factory::declare_constructor(&mut compiler.frame(pipeline, builder, &mut lambda_table), &constructor)?;
  let original_args = constructor.args.args;
  constructor.args.args = gd_closure_vars.to_vec();
  constructor.args.args.extend(original_args);
  for name in gd_closure_vars.iter().rev() {
    constructor.body.insert(0, super::assign_to_compiler(name.to_string(), name.to_string(), pos));
  }
  let mut class_body = vec!();
  class_body.push(Decl::new(DeclF::FnDecl(decl::Static::NonStatic, constructor), pos));
  for name in gd_closure_vars.iter() {
    class_body.push(Decl::new(DeclF::VarDecl(None, name.clone(), None), pos));
  }
  for d in &decls {

    if let ir::decl::ClassInnerDeclF::ClassFnDecl(fndecl) = &d.value {
      if fndecl.is_static.into() {
        // Static methods are not allowed on lambda classes
        return Err(Error::new(ErrorF::StaticMethodOnLambdaClass(fndecl.name.clone()), d.pos));
      }
    }

    let tables = ClassTablePair { instance_table: &mut lambda_table, static_table: &mut lambda_static_table };
    class_body.push(compiler.compile_class_inner_decl(pipeline, builder, tables, d)?);

  }
  let mut class = decl::ClassDecl {
    name: gd_class_name.clone(),
    extends: extends,
    body: class_body,
  };

  if needs_outer_ref {
    inner_class::add_outer_class_ref_named(&mut class, compiler.preload_resolver(), pipeline, outer_ref_name, pos);
  }

  builder.add_helper(Decl::new(DeclF::ClassDecl(class), pos));

  let constructor_args = constructor_args.iter().map(|expr| compiler.compile_expr(pipeline, builder, table, expr, NeedsResult::Yes).map(|x| x.expr)).collect::<Result<Vec<_>, _>>()?;
  let constructor_args: Vec<_> = gd_src_closure_vars.into_iter().map(|x| Expr::new(ExprF::Var(x), pos)).chain(constructor_args.into_iter()).collect();
  let expr = Expr::call(Some(Expr::new(ExprF::Var(gd_class_name), pos)), "new", constructor_args, pos);
  Ok(StExpr { expr, side_effects: SideEffects::None })
}
