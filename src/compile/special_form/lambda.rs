
use crate::ir;
use crate::ir::expr::{Locals, Functions};
use crate::ir::access_type::AccessType;
use crate::compile::{Compiler, StExpr};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::{LocalVar, VarScope, VarName};
use crate::compile::symbol_table::function_call::{FnCall, FnSpecs, FnScope, FnName};
use crate::compile::symbol_table::call_magic::DefaultCall;
use crate::compile::stmt_wrapper;
use crate::compile::error::Error;
use crate::compile::stateful::SideEffects;
use crate::compile::names;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::op;
use crate::gdscript::arglist::ArgList;
use crate::gdscript::library;
use crate::gdscript::inner_class::{self, NeedsOuterClassRef};
use crate::pipeline::Pipeline;
use crate::pipeline::can_load::CanLoad;
use super::lambda_vararg::{generate_lambda_class, assign_to_compiler};

use std::borrow::Borrow;

type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;

pub fn purge_globals(vars: &mut Locals, table: &SymbolTable) {
  vars.retain(|var, _| {
    table.get_var(var).map_or(true, |v| v.scope != VarScope::GlobalVar)
  });
}

pub fn compile_labels_scc<'a>(compiler: &mut Compiler<'a>,
                              pipeline: &mut Pipeline,
                              builder: &mut StmtBuilder,
                              table: &mut SymbolTable,
                              clauses: &[&(String, IRArgList, IRExpr)])
                              -> Result<Vec<(String, FnCall)>, Error> {
  let class_name = compiler.name_generator().generate_with("_Labels");

  let mut closure_vars = Locals::new();
  let mut closure_fns = Functions::new();
  let mut all_vars = Locals::new();

  for (_, args, body) in clauses {
    let (mut inner_vars, inner_fns) = body.get_names();
    all_vars.merge_with(inner_vars.clone());
    for arg in args.iter_vars() {
      inner_vars.remove(arg);
    }
    closure_vars.merge_with(inner_vars);
    closure_fns.merge_with(inner_fns);
  }

  // Function names are in scope for the duration of their own bodies
  for (name, _, _) in clauses {
    closure_fns.remove(&name);
  }

  let mut outer_ref_name = String::new();
  let needs_outer_ref = closure_fns.needs_outer_class_ref(table);
  if needs_outer_ref {
    outer_ref_name = compiler.name_generator().generate_with(inner_class::OUTER_REFERENCE_NAME);
  }

  // No need to close around global variables, as they're available everywhere
  purge_globals(&mut closure_vars, table);

  let mut lambda_table = SymbolTable::new();
  locally_bind_vars(compiler, table, &mut lambda_table, closure_vars.names())?;
  locally_bind_fns(compiler, pipeline, table, &mut lambda_table, closure_fns.names(), false, &outer_ref_name)?;
  copy_global_vars(table, &mut lambda_table);

  let mut gd_src_closure_vars = Vec::new();
  let mut gd_closure_vars = Vec::new();
  for ast_name in closure_vars.names() {
    let var = lambda_table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda variable {}", ast_name)
    }).to_owned();
    if let Some(name) = var.simple_name() {
      gd_closure_vars.push(name.to_owned());
    }
    let src_var = table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda variable {}", ast_name)
    }).to_owned();
    if let Some(name) = src_var.simple_name() {
      gd_src_closure_vars.push(name.to_owned());
    }
  }
  for func in closure_fns.names() {
    match table.get_fn(func) {
      None => { return Err(Error::NoSuchFn(func.to_owned())) }
      Some((call, _)) => {
        if let Some(var) = closure_fn_to_gd_var(call) {
          gd_closure_vars.push(var.to_owned());
          gd_src_closure_vars.push(var);
        }
      }
    }
  }

  let local_var_name = compiler.name_generator().generate_with("_locals");

  // Bind the functions themselves
  let function_names: Vec<String> = clauses.iter().map(|(name, args, _)| {
    let name_prefix = format!("_fn_{}", names::lisp_to_gd(name));
    let func_name = compiler.name_generator().generate_with(&name_prefix);
    lambda_table.set_fn(name.to_owned(), FnCall {
      scope: FnScope::SpecialLocal(local_var_name.clone()),
      object: FnName::OnLocalScope,
      function: func_name.clone(),
      specs: FnSpecs::from(args.to_owned()),
      is_macro: false,
    }, Box::new(DefaultCall));
    func_name
  }).collect();

  let mut functions: Vec<decl::FnDecl> = Vec::new();

  let bound_calls: Vec<(String, FnCall)> = clauses.iter().enumerate().map(|(idx, (name, args, body))| {
    let mut lambda_table = lambda_table.clone(); // New table for this particular function
    let mut lambda_builder = StmtBuilder::new();
    let (arglist, gd_args) = args.clone().into_gd_arglist(&mut compiler.name_generator());
    for (arg, gd_arg) in &gd_args {
      let access_type = *all_vars.get(&arg).unwrap_or(&AccessType::None);
      lambda_table.set_var(arg.to_owned(), LocalVar::local(gd_arg.to_owned(), access_type));
      wrap_in_cell_if_needed(arg, gd_arg, &all_vars, &mut lambda_builder);
    }
    compiler.compile_stmt(pipeline, &mut lambda_builder, &mut lambda_table, &stmt_wrapper::Return, body)?;
    let lambda_body = lambda_builder.build_into(builder);
    let func_name = function_names[idx].to_owned();
    let func = decl::FnDecl {
      name: func_name.clone(),
      args: arglist,
      body: lambda_body,
    };
    let call = FnCall {
      scope: FnScope::SpecialLocal(local_var_name.clone()),
      object: FnName::on_local_var(VarName::local(&local_var_name)),
      function: func_name,
      specs: FnSpecs::from(args.to_owned()),
      is_macro: false,
    };
    functions.push(func);
    Ok((name.to_owned(), call))
  }).collect::<Result<_, Error>>()?;

  let mut constructor_body = Vec::new();
  for var in &gd_closure_vars {
    constructor_body.push(assign_to_compiler(var.to_string(), var.to_string()));
  }
  let constructor = decl::FnDecl {
    name: String::from(library::CONSTRUCTOR_NAME),
    args: ArgList::required(gd_closure_vars.iter().map(|x| x.to_owned()).collect()),
    body: constructor_body,
  };
  let mut class_body = vec!();
  for var in &gd_closure_vars {
    class_body.push(Decl::VarDecl(None, var.clone(), None));
  }
  class_body.push(Decl::FnDecl(decl::Static::NonStatic, constructor));
  for func in functions {
    class_body.push(Decl::FnDecl(decl::Static::NonStatic, func));
  }
  let mut class = decl::ClassDecl {
    name: class_name.clone(),
    extends: decl::ClassExtends::named(String::from("Reference")),
    body: class_body,
  };

  if needs_outer_ref {
    inner_class::add_outer_class_ref_named(&mut class, compiler.preload_resolver(), pipeline, outer_ref_name);
  }

  builder.add_helper(Decl::ClassDecl(class));
  let constructor_args: Vec<_> = gd_src_closure_vars.into_iter().map(Expr::Var).collect();
  let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), constructor_args);
  builder.append(Stmt::VarDecl(local_var_name, expr));

  Ok(bound_calls)
}

pub fn locally_bind_vars<'a, 'b, I, U>(compiler: &mut Compiler<'b>,
                                       table: &SymbolTable,
                                       lambda_table: &mut SymbolTable,
                                       closure_vars: I)
                                       -> Result<(), Error>
where I : Iterator<Item=&'a U>,
      U : Borrow<str>,
      U : ?Sized,
      U : 'a {
  for var in closure_vars {
    // Ensure the variable actually exists
    match table.get_var(var.borrow()) {
      None => return Err(Error::NoSuchVar(var.borrow().to_owned())),
      Some(gdvar) => {
        let mut new_var = gdvar.to_owned();
        // Ad-hoc rule for closing around self (TODO Generalize?)
        if new_var == LocalVar::self_var() { // TODO Special case over in VarName?
          new_var = LocalVar::local(compiler.name_generator().generate_with("_self"), AccessType::ClosedRead);
        }
        lambda_table.set_var(var.borrow().to_owned(), new_var);
      }
    };
  }
  Ok(())
}

pub fn locally_bind_fns<'a, 'b, I, U, L>(compiler: &mut Compiler<'b>,
                                         pipeline: &L,
                                         table: &SymbolTable,
                                         lambda_table: &mut SymbolTable,
                                         closure_fns: I,
                                         static_binding: bool,
                                         outer_reference_name: &str)
                                         -> Result<(), Error>
where I : Iterator<Item=&'a U>,
      U : Borrow<str>,
      U : ?Sized,
      U : 'a,
      L : CanLoad {
  for func in closure_fns {
    // Ensure the function actually exists
    match table.get_fn(func.borrow()) {
      None => { return Err(Error::NoSuchFn(func.borrow().to_owned())) }
      Some((call, magic)) => {
        let mut call = call.clone();
        call.object.update_for_inner_scope(static_binding, compiler.preload_resolver(), pipeline, &outer_reference_name);
        lambda_table.set_fn(func.borrow().to_owned(), call, dyn_clone::clone_box(magic));
      }
    };
  }
  Ok(())
}

pub fn copy_global_vars(src_table: &SymbolTable, dest_table: &mut SymbolTable) {
  for (name, var) in src_table.vars() {
    if var.scope == VarScope::GlobalVar {
      dest_table.set_var(name.to_owned(), var.clone());
    }
  }
}

pub fn closure_fn_to_gd_var(call: &FnCall) -> Option<String> {
  call.scope.local_name().map(str::to_owned)
}

fn wrap_in_cell_if_needed(name: &str, gd_name: &str, all_vars: &Locals, lambda_builder: &mut StmtBuilder) {
  if all_vars.get(name).unwrap_or(&AccessType::None).requires_cell() {
    lambda_builder.append(Stmt::Assign(Box::new(Expr::var(gd_name)),
                                       op::AssignOp::Eq,
                                       Box::new(library::construct_cell(Expr::var(gd_name)))));
  }
}

pub fn compile_lambda_stmt<'a>(compiler: &mut Compiler<'a>,
                               pipeline: &mut Pipeline,
                               builder: &mut StmtBuilder,
                               table: &mut SymbolTable,
                               args: &IRArgList,
                               body: &IRExpr)
                               -> Result<StExpr, Error> {
  let (arglist, gd_args) = args.clone().into_gd_arglist(&mut compiler.name_generator());

  let mut lambda_builder = StmtBuilder::new();
  let (all_vars, closure_fns) = body.get_names();
  let mut closure_vars = all_vars.clone();
  for arg in &gd_args {
    closure_vars.remove(&arg.0);
  }

  let mut lambda_table = SymbolTable::new();
  for arg in &gd_args {
    let access_type = *all_vars.get(&arg.0).unwrap_or(&AccessType::None);
    lambda_table.set_var(arg.0.to_owned(), LocalVar::local(arg.1.to_owned(), access_type));
  }

  let mut outer_ref_name = String::new();
  let needs_outer_ref = closure_fns.needs_outer_class_ref(table);
  if needs_outer_ref {
    outer_ref_name = compiler.name_generator().generate_with(inner_class::OUTER_REFERENCE_NAME);
  }

  // No need to close around global variables, as they're available everywhere
  purge_globals(&mut closure_vars, table);

  locally_bind_vars(compiler, table, &mut lambda_table, closure_vars.names())?;
  locally_bind_fns(compiler, pipeline, table, &mut lambda_table, closure_fns.names(), false, &outer_ref_name)?;
  copy_global_vars(table, &mut lambda_table);

  let mut gd_src_closure_vars = Vec::new();
  let mut gd_closure_vars = Vec::new();
  for ast_name in closure_vars.names() {
    let var = lambda_table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda variable {}", ast_name)
    }).to_owned();
    if let Some(name) = var.simple_name() {
      gd_closure_vars.push(name.to_owned());
    }
    let src_var = table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda variable {}", ast_name)
    }).to_owned();
    if let Some(name) = src_var.simple_name() {
      gd_src_closure_vars.push(name.to_owned());
    }
  }
  for func in closure_fns.names() {
    match table.get_fn(func) {
      None => { return Err(Error::NoSuchFn(func.to_owned())) }
      Some((call, _)) => {
        if let Some(var) = closure_fn_to_gd_var(call) {
          gd_closure_vars.push(var.to_owned());
          gd_src_closure_vars.push(var);
        }
      }
    }
  }

  for (arg, gd_arg) in &gd_args {
    wrap_in_cell_if_needed(arg, gd_arg, &all_vars, &mut lambda_builder);
  }
  compiler.compile_stmt(pipeline, &mut lambda_builder, &mut lambda_table, &stmt_wrapper::Return, body)?;
  let lambda_body = lambda_builder.build_into(builder);

  let class_name = compiler.name_generator().generate_with("_LambdaBlock");
  let mut class = generate_lambda_class(class_name.clone(), args.clone().into(), arglist, &gd_closure_vars, lambda_body);

  if needs_outer_ref {
    inner_class::add_outer_class_ref_named(&mut class, compiler.preload_resolver(), pipeline, outer_ref_name);
  }

  builder.add_helper(Decl::ClassDecl(class));
  let constructor_args = gd_src_closure_vars.into_iter().map(Expr::Var).collect();
  let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), constructor_args);
  Ok(StExpr { expr, side_effects: SideEffects::None })
}

pub fn compile_function_ref<'a>(compiler: &mut Compiler<'a>,
                                pipeline: &mut Pipeline,
                                builder: &mut StmtBuilder,
                                _table: &mut SymbolTable,
                                func: FnCall)
                                -> Result<StExpr, Error> {
  if let FnScope::Local(name) = func.scope {
    Ok(StExpr { expr: Expr::Var(name), side_effects: SideEffects::None })
  } else {
    let specs = func.specs;
    let arg_count = func.specs.runtime_arity();
    let arg_names: Vec<_> = (0..arg_count).map(|i| format!("arg{}", i)).collect();
    let arglist = ArgList::required(arg_names.clone());

    let mut closure_vars = Vec::new();
    if let FnScope::SpecialLocal(name) = func.scope {
      closure_vars.push(name);
    }
    let closure_var_ctor_args: Vec<_> = closure_vars.iter().map(|name| {
      Expr::var(name)
    }).collect();

    // TODO This into().map(Box::new) pattern needs to be written into FnName itself
    let object = if func.object == FnName::FileConstant { FnName::inner_static_load(compiler.preload_resolver(), pipeline) } else { func.object };
    let object: Option<Expr> = object.into();
    let body = Stmt::ReturnStmt(
      Expr::Call(object.map(Box::new), func.function, arg_names.into_iter().map(Expr::Var).collect())
    );

    let class_name = compiler.name_generator().generate_with("_FunctionRefBlock");
    let class = generate_lambda_class(class_name.clone(), specs, arglist, &closure_vars[..], vec!(body));
    builder.add_helper(Decl::ClassDecl(class));
    let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), closure_var_ctor_args);
    Ok(StExpr { expr, side_effects: SideEffects::None })
  }
}
