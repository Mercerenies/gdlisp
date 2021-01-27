
use crate::ir;
use crate::compile::{Compiler, StExpr};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::{SymbolTable, LocalVar};
use crate::compile::symbol_table::function_call::{FnCall, FnSpecs, FnScope};
use crate::compile::stmt_wrapper;
use crate::compile::error::Error;
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::expr::Expr;
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::op;
use crate::gdscript::arglist::ArgList;
use crate::gdscript::library;

use std::convert::TryInto;

type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;

fn generate_lambda_vararg<'a>(specs: FnSpecs) -> decl::FnDecl {
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
    .map(|x| Expr::Var(x))
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

// ///// (function ...) and #'... syntax
fn generate_lambda_class<'a, 'b>(compiler: &mut Compiler<'a>,
                                 specs: FnSpecs,
                                 args: ArgList,
                                 closed_vars: &Vec<LocalVar>,
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

fn assign_to_compiler(inst_var: String, local_var: String) -> Stmt {
  assign_expr_to_compiler(inst_var, Expr::Var(local_var))
}

fn assign_expr_to_compiler(inst_var: String, expr: Expr) -> Stmt {
  let self_target = Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("self"))), inst_var));
  let value = Box::new(expr);
  Stmt::Assign(self_target, op::AssignOp::Eq, value)
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
  // I want them in a consistent order for the constructor
  // function. I don't care which order, but I need an order, so
  // let's make a Vec now.
  let closure_vars_vec: Vec<_> = closure_vars.names().collect();

  let mut lambda_table = SymbolTable::new();
  for arg in &gd_args {
    lambda_table.set_var(arg.0.to_owned(), LocalVar::new(arg.1.to_owned(), all_vars.get(&arg.0)));
  }
  for var in &closure_vars_vec {
    // Ensure the variable actually exists
    match table.get_var(var) {
      None => return Err(Error::NoSuchVar((*var).to_owned())),
      Some(gdvar) => lambda_table.set_var((*var).to_owned(), gdvar.to_owned()), // TODO Generate new names here
    };
  }
  for func in closure_fns.names() {
    // Ensure the function actually exists
    match table.get_fn(func) {
      None => { return Err(Error::NoSuchFn(func.to_owned())) }
      Some(       FnCall { scope: FnScope::Local , .. }) => { panic!("Not yet supported!"); } ////
      Some(call @ FnCall { scope: FnScope::Global, .. }) => { lambda_table.set_fn(func.to_owned(), call.clone()); }
    }
  }

  let gd_closure_vars = closure_vars_vec.iter().map(|ast_name| {
    lambda_table.get_var(&ast_name).unwrap_or_else(|| {
      panic!("Internal error compiling lambda variable {}", ast_name)
    }).to_owned()
  }).collect();

  for arg in &gd_args {
    if all_vars.get(&arg.0).requires_cell() {
      // Special behavior to wrap the argument in a cell.
      lambda_builder.append(Stmt::Assign(Box::new(Expr::var(&arg.1)),
                                         op::AssignOp::Eq,
                                         Box::new(library::construct_cell(Expr::var(&arg.1)))));
    }
  }
  compiler.compile_stmt(&mut lambda_builder, &mut lambda_table, &stmt_wrapper::Return, body)?;
  let lambda_body = lambda_builder.build_into(builder);
  let class = generate_lambda_class(compiler, args.clone().into(), arglist, &gd_closure_vars, lambda_body, "_LambdaBlock");
  let class_name = class.name.clone();
  builder.add_helper(Decl::ClassDecl(class));
  let constructor_args = gd_closure_vars.into_iter().map(|s| Expr::Var(s.name.to_owned())).collect();
  let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), constructor_args);
  Ok(StExpr(expr, false))
}

pub fn compile_function_ref<'a>(compiler: &mut Compiler<'a>,
                                builder: &mut StmtBuilder,
                                _table: &mut SymbolTable,
                                func: FnCall)
                                -> Result<StExpr, Error> {
  if func.scope != FnScope::Global {
    panic!("Local function refs not implemented yet!"); // TODO This
  }
  let specs = func.specs;
  let arg_count = func.specs.runtime_arity();
  let arg_names: Vec<_> = (0..arg_count).map(|i| format!("arg{}", i)).collect();
  let arglist = ArgList::required(arg_names.clone());

  let body = Stmt::ReturnStmt(
    Expr::Call(func.object, func.function, arg_names.into_iter().map(Expr::Var).collect())
  );
  let class = generate_lambda_class(compiler, specs, arglist.clone(), &vec!(), vec!(body), "_FunctionRefBlock");
  let class_name = class.name.clone();
  builder.add_helper(Decl::ClassDecl(class));
  let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), vec!());
  Ok(StExpr(expr, false))
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
