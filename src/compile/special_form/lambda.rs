
use crate::ir;
use crate::ir::locals::{Locals, AccessType};
use crate::ir::functions::Functions;
use crate::compile::{Compiler, StExpr};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::{SymbolTable, LocalVar};
use crate::compile::symbol_table::function_call::{FnCall, FnSpecs, FnScope};
use crate::compile::stmt_wrapper;
use crate::compile::error::Error;
use crate::compile::stateful::SideEffects;
use crate::compile::names;
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::expr::Expr;
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::op;
use crate::gdscript::arglist::ArgList;
use crate::gdscript::library;

use std::convert::TryInto;
use std::borrow::Borrow;

type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;

fn generate_lambda_vararg(specs: FnSpecs) -> decl::FnDecl {
  let mut stmts = Vec::new();

  let args = String::from("args");
  let required: Vec<_> = (0..specs.required).map(|i| format!("required_{}", i)).collect();
  let optional: Vec<_> = (0..specs.optional).map(|i| format!("optional_{}", i)).collect();

  for req in &required {
    stmts.push(Stmt::VarDecl(req.to_owned(), Expr::null()));
    stmts.push(stmt::if_else(
      Expr::Binary(Box::new(Expr::Var(String::from("args"))), op::BinaryOp::Is, Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("GDLisp"))), String::from("NilClass")))),
      vec!(
        Stmt::Expr(Expr::Call(None, String::from("push_error"), vec!(Expr::str_lit("Not enough arguments"))))
      ),
      vec!(
        Stmt::Assign(Box::new(Expr::Var(req.to_owned())), op::AssignOp::Eq, Box::new(Expr::Attribute(Box::new(Expr::Var(args.clone())), String::from("car")))),
        Stmt::Assign(Box::new(Expr::Var(args.clone())), op::AssignOp::Eq, Box::new(Expr::Attribute(Box::new(Expr::Var(args.clone())), String::from("cdr"))))
      ),
    ));
  }

  for opt in &optional {
    stmts.push(Stmt::VarDecl(opt.to_owned(), Expr::null()));
    stmts.push(stmt::if_else(
      Expr::Binary(Box::new(Expr::Var(String::from("args"))), op::BinaryOp::Is, Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("GDLisp"))), String::from("NilClass")))),
      vec!(
        Stmt::Assign(Box::new(Expr::Var(opt.to_owned())), op::AssignOp::Eq, Box::new(library::nil()))
      ),
      vec!(
        Stmt::Assign(Box::new(Expr::Var(opt.to_owned())), op::AssignOp::Eq, Box::new(Expr::Attribute(Box::new(Expr::Var(args.clone())), String::from("car")))),
        Stmt::Assign(Box::new(Expr::Var(args.clone())), op::AssignOp::Eq, Box::new(Expr::Attribute(Box::new(Expr::Var(args.clone())), String::from("cdr"))))
      ),
    ));
  }

  let mut all_args: Vec<_> =
    required.into_iter()
    .chain(optional.into_iter())
    .map(Expr::Var)
    .collect();
  if specs.rest {
    all_args.push(Expr::Var(args));
    stmts.push(Stmt::ReturnStmt(Expr::Call(None, String::from("call_func"), all_args)));
  } else {
    stmts.push(
      stmt::if_else(
        Expr::Binary(Box::new(Expr::Var(String::from("args"))), op::BinaryOp::Is, Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("GDLisp"))), String::from("NilClass")))),
        vec!(
          Stmt::ReturnStmt(Expr::Call(None, String::from("call_func"), all_args)),
        ),
        vec!(
          Stmt::Expr(Expr::Call(None, String::from("push_error"), vec!(Expr::str_lit("Too many arguments")))),
        ),
      )
    );
  }

  decl::FnDecl {
    name: String::from("call_funcv"),
    args: ArgList::required(vec!(String::from("args"))),
    body: stmts,
  }
}

fn generate_lambda_class<'a, 'b>(compiler: &mut Compiler<'a>,
                                 specs: FnSpecs,
                                 args: ArgList,
                                 closed_vars: &[LocalVar],
                                 lambda_body: Vec<Stmt>,
                                 block_prefix: &'b str)
                                 -> decl::ClassDecl {
  let class_name = compiler.name_generator().generate_with(block_prefix);
  let func_name = String::from("call_func");
  let func = decl::FnDecl {
    name: func_name,
    args: args,
    body: lambda_body,
  };
  let funcv = generate_lambda_vararg(specs);
  let mut constructor_body = Vec::new();
  for var in closed_vars.iter() {
    constructor_body.push(assign_to_compiler(var.name.to_string(), var.name.to_string()));
  }
  let r: i32  = specs.required.try_into().unwrap();
  let o: i32  = specs.optional.try_into().unwrap();
  let x: bool = specs.rest;
  constructor_body.push(assign_expr_to_compiler(String::from("__gdlisp_required"), Expr::from(r)));
  constructor_body.push(assign_expr_to_compiler(String::from("__gdlisp_optional"), Expr::from(o)));
  constructor_body.push(assign_expr_to_compiler(String::from("__gdlisp_rest"), Expr::from(x)));
  let constructor =
    decl::FnDecl {
      name: String::from("_init"),
      args: ArgList::required(closed_vars.iter().map(|x| x.name.to_owned()).collect()),
      body: constructor_body,
    };
  let mut class_body = vec!();
  for var in closed_vars {
    class_body.push(Decl::VarDecl(var.name.clone(), None));
  }
  class_body.append(&mut vec!(
    Decl::FnDecl(decl::Static::NonStatic, constructor),
    Decl::FnDecl(decl::Static::NonStatic, func),
    Decl::FnDecl(decl::Static::NonStatic, funcv),
  ));
  decl::ClassDecl {
    name: class_name,
    extends: decl::ClassExtends::Qualified(String::from("GDLisp"), String::from("Function")),
    body: class_body,
  }
}

///// This is hilariously untested. Good luck :)
pub fn compile_labels_scc<'a>(compiler: &mut Compiler<'a>,
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

  let mut lambda_table = SymbolTable::new();
  locally_bind_vars(table, &mut lambda_table, closure_vars.names())?;
  locally_bind_fns(table, &mut lambda_table, closure_fns.names())?;

  let mut gd_closure_vars = Vec::new();
  for ast_name in closure_vars.names() {
    let var = lambda_table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda variable {}", ast_name)
    }).to_owned();
    gd_closure_vars.push(var);
  }
  for func in closure_fns.names() {
    match table.get_fn(func) {
      None => { return Err(Error::NoSuchFn(func.to_owned())) }
      Some((call, _)) => {
        if let Some(var) = closure_fn_to_gd_var(call) {
          gd_closure_vars.push(var);
        }
      }
    }
  }

  let local_var_name = compiler.name_generator().generate_with("_locals");

  // Bind the functions themselves
  let function_names: Vec<String> = clauses.iter().map(|(name, args, _)| {
    let name_prefix = format!("_fn_{}", names::lisp_to_gd(name));
    let func_name = compiler.name_generator().generate_with(&name_prefix);
    lambda_table.set_fn_base(name.to_owned(), FnCall {
      scope: FnScope::SpecialLocal(local_var_name.clone()),
      object: None,
      function: func_name.clone(),
      specs: FnSpecs::from(args.to_owned()),
    });
    func_name
  }).collect();

  let mut functions: Vec<decl::FnDecl> = Vec::new();

  let bound_calls: Vec<(String, FnCall)> = clauses.iter().enumerate().map(|(idx, (name, args, body))| {
    let mut lambda_table = lambda_table.clone(); // New table for this particular function
    let mut lambda_builder = StmtBuilder::new();
    let (arglist, gd_args) = args.clone().into_gd_arglist(&mut compiler.name_generator());
    for (arg, gd_arg) in &gd_args {
      lambda_table.set_var(arg.to_owned(), LocalVar::new(gd_arg.to_owned(), all_vars.get(&arg)));
      wrap_in_cell_if_needed(arg, gd_arg, &all_vars, &mut lambda_builder);
    }
    compiler.compile_stmt(&mut lambda_builder, &mut lambda_table, &stmt_wrapper::Return, body)?;
    let lambda_body = lambda_builder.build_into(builder);
    let func_name = function_names[idx].to_owned();
    let func = decl::FnDecl {
      name: func_name.clone(),
      args: arglist,
      body: lambda_body,
    };
    let call = FnCall {
      scope: FnScope::SpecialLocal(local_var_name.clone()),
      object: Some(Box::new(Expr::Var(local_var_name.clone()))),
      function: func_name,
      specs: FnSpecs::from(args.to_owned()),
    };
    functions.push(func);
    Ok((name.to_owned(), call))
  }).collect::<Result<_, Error>>()?;

  let mut constructor_body = Vec::new();
  for var in &gd_closure_vars {
    constructor_body.push(assign_to_compiler(var.name.to_string(), var.name.to_string()));
  }
  let constructor = decl::FnDecl {
    name: String::from("_init"),
    args: ArgList::required(gd_closure_vars.iter().map(|x| x.name.to_owned()).collect()),
    body: constructor_body,
  };
  let mut class_body = vec!();
  for var in &gd_closure_vars {
    class_body.push(Decl::VarDecl(var.name.clone(), None));
  }
  class_body.push(Decl::FnDecl(decl::Static::NonStatic, constructor));
  for func in functions {
    class_body.push(Decl::FnDecl(decl::Static::NonStatic, func));
  }
  let class = decl::ClassDecl {
    name: class_name.clone(),
    extends: decl::ClassExtends::Named(String::from("Reference")),
    body: class_body,
  };
  builder.add_helper(Decl::ClassDecl(class));
  let constructor_args: Vec<_> = gd_closure_vars.into_iter().map(|s| Expr::Var(s.name)).collect();
  let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), constructor_args);
  builder.append(Stmt::VarDecl(local_var_name, expr));

  Ok(bound_calls)
}

fn assign_to_compiler(inst_var: String, local_var: String) -> Stmt {
  assign_expr_to_compiler(inst_var, Expr::Var(local_var))
}

fn assign_expr_to_compiler(inst_var: String, expr: Expr) -> Stmt {
  let self_target = Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("self"))), inst_var));
  let value = Box::new(expr);
  Stmt::Assign(self_target, op::AssignOp::Eq, value)
}

fn locally_bind_vars<'a, I, U>(table: &SymbolTable,
                               lambda_table: &mut SymbolTable,
                               closure_vars: I)
                               -> Result<(), Error>
where I : Iterator<Item=&'a U>,
      U : Borrow<str>,
      U : 'a {
  for var in closure_vars {
    // Ensure the variable actually exists
    match table.get_var(var.borrow()) {
      None => return Err(Error::NoSuchVar(var.borrow().to_owned())),
      Some(gdvar) => lambda_table.set_var(var.borrow().to_owned(), gdvar.to_owned()), // TODO Generate new names here
    };
  }
  Ok(())
}

fn locally_bind_fns<'a, I, U>(table: &SymbolTable,
                              lambda_table: &mut SymbolTable,
                              closure_fns: I)
                              -> Result<(), Error>
where I : Iterator<Item=&'a U>,
      U : Borrow<str>,
      U : 'a {
  for func in closure_fns {
    // Ensure the function actually exists
    match table.get_fn(func.borrow()) {
      None => { return Err(Error::NoSuchFn(func.borrow().to_owned())) }
      Some((call, magic)) => {
        match call.scope {
          FnScope::SpecialLocal(_) | FnScope::Local(_) | FnScope::SemiGlobal | FnScope::Global => {
            lambda_table.set_fn_base(func.borrow().to_owned(), call.clone());
            lambda_table.set_magic_fn(func.borrow().to_owned(), dyn_clone::clone_box(magic));
          }
        }
      }
    };
  }
  Ok(())
}

fn closure_fn_to_gd_var(call: &FnCall) -> Option<LocalVar> {
  match &call.scope {
    FnScope::Local(name) | FnScope::SpecialLocal(name) => {
      // ClosedRead *might* be more conservative than necessary
      // here (it's possible we can get away with Read in some
      // situations), but I don't think it changes anything, so we
      // may as well play it safe.
      Some(LocalVar {
        name: name.to_owned(),
        access_type: AccessType::ClosedRead,
      })
    }
    FnScope::SemiGlobal | FnScope::Global => {
      None
    }
  }
}

fn wrap_in_cell_if_needed(name: &str, gd_name: &str, all_vars: &Locals, lambda_builder: &mut StmtBuilder) {
  if all_vars.get(name).requires_cell() {
    lambda_builder.append(Stmt::Assign(Box::new(Expr::var(gd_name)),
                                       op::AssignOp::Eq,
                                       Box::new(library::construct_cell(Expr::var(gd_name)))));
  }
}

pub fn compile_lambda_stmt<'a>(compiler: &mut Compiler<'a>,
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
    lambda_table.set_var(arg.0.to_owned(), LocalVar::new(arg.1.to_owned(), all_vars.get(&arg.0)));
  }

  locally_bind_vars(table, &mut lambda_table, closure_vars.names())?;
  locally_bind_fns(table, &mut lambda_table, closure_fns.names())?;

  let mut gd_closure_vars = Vec::new();
  for ast_name in closure_vars.names() {
    let var = lambda_table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda variable {}", ast_name)
    }).to_owned();
    gd_closure_vars.push(var);
  }
  for func in closure_fns.names() {
    match table.get_fn(func) {
      None => { return Err(Error::NoSuchFn(func.to_owned())) }
      Some((call, _)) => {
        if let Some(var) = closure_fn_to_gd_var(call) {
          gd_closure_vars.push(var);
        }
      }
    }
  }

  for (arg, gd_arg) in &gd_args {
    wrap_in_cell_if_needed(arg, gd_arg, &all_vars, &mut lambda_builder);
  }
  compiler.compile_stmt(&mut lambda_builder, &mut lambda_table, &stmt_wrapper::Return, body)?;
  let lambda_body = lambda_builder.build_into(builder);
  let class = generate_lambda_class(compiler, args.clone().into(), arglist, &gd_closure_vars, lambda_body, "_LambdaBlock");
  let class_name = class.name.clone();
  builder.add_helper(Decl::ClassDecl(class));
  let constructor_args = gd_closure_vars.into_iter().map(|s| Expr::Var(s.name)).collect();
  let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), constructor_args);
  Ok(StExpr(expr, SideEffects::None))
}

pub fn compile_function_ref<'a>(compiler: &mut Compiler<'a>,
                                builder: &mut StmtBuilder,
                                _table: &mut SymbolTable,
                                func: FnCall)
                                -> Result<StExpr, Error> {
  if let FnScope::Local(name) = func.scope {
    Ok(StExpr(Expr::Var(name), SideEffects::None))
  } else {
    let specs = func.specs;
    let arg_count = func.specs.runtime_arity();
    let arg_names: Vec<_> = (0..arg_count).map(|i| format!("arg{}", i)).collect();
    let arglist = ArgList::required(arg_names.clone());

    let mut closure_vars = Vec::new();
    if let FnScope::SpecialLocal(name) = func.scope {
      closure_vars.push(LocalVar {
        name: name,
        access_type: AccessType::ClosedRead, // May be overly conservative but definitely safe.
      });
    }
    let closure_var_ctor_args: Vec<_> = closure_vars.iter().map(|x| {
      Expr::var(&x.name)
    }).collect();

    let body = Stmt::ReturnStmt(
      Expr::Call(func.object, func.function, arg_names.into_iter().map(Expr::Var).collect())
    );
    let class = generate_lambda_class(compiler, specs, arglist, &closure_vars[..], vec!(body), "_FunctionRefBlock");
    let class_name = class.name.clone();
    builder.add_helper(Decl::ClassDecl(class));
    let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), closure_var_ctor_args);
    Ok(StExpr(expr, SideEffects::None))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn compile_vararg(specs: FnSpecs) -> String {
    let result = generate_lambda_vararg(specs);
    Decl::FnDecl(decl::Static::NonStatic, result).to_gd(0)
  }

  #[test]
  fn test_lambda_vararg() {
    assert_eq!(compile_vararg(FnSpecs::new(0, 0, false)), "func call_funcv(args):\n    if args is GDLisp.NilClass:\n        return call_func()\n    else:\n        push_error(\"Too many arguments\")\n");
    assert_eq!(compile_vararg(FnSpecs::new(0, 0, true)), "func call_funcv(args):\n    return call_func(args)\n");
    assert_eq!(compile_vararg(FnSpecs::new(1, 0, true)), "func call_funcv(args):\n    var required_0 = null\n    if args is GDLisp.NilClass:\n        push_error(\"Not enough arguments\")\n    else:\n        required_0 = args.car\n        args = args.cdr\n    return call_func(required_0, args)\n");
    assert_eq!(compile_vararg(FnSpecs::new(0, 1, true)), "func call_funcv(args):\n    var optional_0 = null\n    if args is GDLisp.NilClass:\n        optional_0 = GDLisp.Nil\n    else:\n        optional_0 = args.car\n        args = args.cdr\n    return call_func(optional_0, args)\n");
  }

}
