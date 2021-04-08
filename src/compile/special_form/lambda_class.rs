
use crate::ir::expr::LambdaClass;
use crate::compile::error::Error;
use crate::compile::Compiler;
use crate::compile::stateful::{SideEffects, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::{VarScope, LocalVar};
use crate::gdscript::expr::Expr;
use crate::gdscript::decl::{self, Decl};
use crate::pipeline::Pipeline;
use super::lambda;

use std::convert::TryFrom;

pub fn compile_lambda_class<'a>(compiler: &mut Compiler<'a>,
                                pipeline: &mut Pipeline,
                                builder: &mut StmtBuilder,
                                table: &mut SymbolTable,
                                class: &LambdaClass)
                                -> Result<StExpr, Error> {
  let LambdaClass { extends, args: constructor_args, constructor, decls } = class.clone();

  // Validate the extends declaration (must be a global variable)
  let extends_var = table.get_var(&extends).ok_or_else(|| Error::NoSuchVar(extends.clone()))?;
  if extends_var.scope != VarScope::GlobalVar {
    return Err(Error::CannotExtend(extends.clone()));
  }
  let extends = extends_var.name.clone();
  let extends = decl::ClassExtends::try_from(extends)?;

  // New GD name
  let gd_class_name = compiler.name_generator().generate_with("_AnonymousClass");

  let (mut closure_vars, mut closure_fns) = constructor.get_names();
  for d in &decls {
    let (decl_vars, decl_fns) = d.get_names();
    closure_vars.merge_with(decl_vars);
    closure_fns.merge_with(decl_fns);
  }

  let mut lambda_table = SymbolTable::new();
  lambda_table.set_var(String::from("self"), LocalVar::self_var());

  lambda::purge_globals(&mut closure_vars, table);
  lambda::locally_bind_vars(compiler, table, &mut lambda_table, closure_vars.names())?;
  lambda::locally_bind_fns(compiler, table, &mut lambda_table, closure_fns.names())?;
  lambda::copy_global_vars(table, &mut lambda_table);

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
      None => { return Err(Error::NoSuchFn(func.to_owned())) }
      Some((call, _)) => {
        if let Some(var) = lambda::closure_fn_to_gd_var(call) {
          gd_closure_vars.push(var.to_owned());
          gd_src_closure_vars.push(var);
        }
      }
    }
  }

  let mut constructor = compiler.compile_constructor(pipeline, builder, &mut lambda_table, &constructor)?;
  let original_args = constructor.args.args;
  constructor.args.args = gd_closure_vars.to_vec();
  constructor.args.args.extend(original_args);
  for name in gd_closure_vars.iter().rev() {
    constructor.body.insert(0, lambda::assign_to_compiler(name.to_string(), name.to_string()));
  }
  let mut class_body = vec!();
  class_body.push(Decl::FnDecl(decl::Static::NonStatic, constructor));
  for name in gd_closure_vars.iter() {
    class_body.push(Decl::VarDecl(None, name.clone(), None));
  }
  for d in &decls {
    class_body.push(compiler.compile_class_inner_decl(pipeline, builder, &mut lambda_table, d)?);
  }
  let class = decl::ClassDecl {
    name: gd_class_name.clone(),
    extends: extends,
    body: class_body,
  };
  builder.add_helper(Decl::ClassDecl(class));

  let constructor_args = constructor_args.iter().map(|expr| compiler.compile_expr(pipeline, builder, table, expr, NeedsResult::Yes).map(|x| x.0)).collect::<Result<Vec<_>, _>>()?;
  let constructor_args: Vec<_> = gd_src_closure_vars.into_iter().map(Expr::Var).chain(constructor_args.into_iter()).collect();
  let expr = Expr::Call(Some(Box::new(Expr::Var(gd_class_name))), String::from("new"), constructor_args);
  Ok(StExpr(expr, SideEffects::None))
}
