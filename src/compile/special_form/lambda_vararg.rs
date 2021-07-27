
use crate::ir::arglist::VarArg;
use crate::compile::symbol_table::function_call::FnSpecs;
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::op;
use crate::gdscript::library;
use crate::gdscript::arglist::ArgList;

use std::convert::TryInto;

pub fn generate_lambda_vararg(specs: FnSpecs) -> decl::FnDecl {
  let mut stmts = Vec::new();

  let args = "args";
  let required: Vec<_> = (0..specs.required).map(|i| format!("required_{}", i)).collect();
  let optional: Vec<_> = (0..specs.optional).map(|i| format!("optional_{}", i)).collect();

  for req in &required {
    stmts.push(Stmt::VarDecl(req.to_owned(), Expr::null()));
    stmts.push(stmt::if_else(
      Expr::binary(Expr::var(args), op::BinaryOp::Eq, Expr::null()),
      vec!(
        Stmt::Expr(Expr::simple_call("push_error", vec!(Expr::str_lit("Not enough arguments"))))
      ),
      vec!(
        Stmt::simple_assign(Expr::var(req), Expr::var(args).attribute("car")),
        Stmt::simple_assign(Expr::var(args), Expr::var(args).attribute("cdr")),
      ),
    ));
  }

  for opt in &optional {
    stmts.push(Stmt::VarDecl(opt.to_owned(), Expr::null()));
    stmts.push(stmt::if_else(
      Expr::binary(Expr::var(args), op::BinaryOp::Eq, Expr::null()),
      vec!(
        Stmt::simple_assign(Expr::var(opt), Expr::null()),
      ),
      vec!(
        Stmt::simple_assign(Expr::var(opt), Expr::var(args).attribute("car")),
        Stmt::simple_assign(Expr::var(args), Expr::var(args).attribute("cdr")),
      ),
    ));
  }

  let mut all_args: Vec<_> =
    required.into_iter()
    .chain(optional.into_iter())
    .map(Expr::Var)
    .collect();
  match specs.rest {
    Some(VarArg::RestArg) => {
      all_args.push(Expr::var(args));
      stmts.push(Stmt::ReturnStmt(Expr::simple_call("call_func", all_args)));
    }
    Some(VarArg::ArrArg) => {
      let array = Expr::call(Some(Expr::var("GDLisp")), "list_to_array", vec!(Expr::var(args)));
      all_args.push(array);
      stmts.push(Stmt::ReturnStmt(Expr::simple_call("call_func", all_args)));
    }
    None => {
      stmts.push(
        stmt::if_else(
          Expr::binary(Expr::var(args), op::BinaryOp::Eq, Expr::null()),
          vec!(
            Stmt::ReturnStmt(Expr::simple_call("call_func", all_args)),
          ),
          vec!(
            Stmt::Expr(Expr::simple_call("push_error", vec!(Expr::str_lit("Too many arguments")))),
          ),
        )
      );
    }
  }

  decl::FnDecl {
    name: String::from("call_funcv"),
    args: ArgList::required(vec!(String::from("args"))),
    body: stmts,
  }
}

pub fn generate_lambda_class<'a, 'b>(class_name: String,
                                     specs: FnSpecs,
                                     args: ArgList,
                                     closed_vars: &[String],
                                     lambda_body: Vec<Stmt>)
                                     -> decl::ClassDecl {
  let func_name = String::from("call_func");
  let func = decl::FnDecl {
    name: func_name,
    args: args,
    body: lambda_body,
  };
  let funcv = generate_lambda_vararg(specs);
  let mut constructor_body = Vec::new();
  for name in closed_vars.iter() {
    constructor_body.push(super::assign_to_compiler(name.to_string(), name.to_string()));
  }
  let r: i32  = specs.required.try_into().unwrap();
  let o: i32  = specs.optional.try_into().unwrap();
  let x: i32 = match specs.rest { // TODO Document these constants on the GDScript side
    None => 0,
    Some(VarArg::RestArg) => 1,
    Some(VarArg::ArrArg) => 2,
  };
  constructor_body.push(super::assign_expr_to_compiler(String::from("__gdlisp_required"), Expr::from(r)));
  constructor_body.push(super::assign_expr_to_compiler(String::from("__gdlisp_optional"), Expr::from(o)));
  constructor_body.push(super::assign_expr_to_compiler(String::from("__gdlisp_rest"), Expr::from(x)));
  let constructor =
    decl::FnDecl {
      name: String::from(library::CONSTRUCTOR_NAME),
      args: ArgList::required(closed_vars.iter().map(|x| x.to_owned()).collect()),
      body: constructor_body,
    };
  let mut class_body = vec!();
  for var in closed_vars {
    class_body.push(Decl::VarDecl(None, var.to_owned(), None));
  }
  class_body.append(&mut vec!(
    Decl::FnDecl(decl::Static::NonStatic, constructor),
    Decl::FnDecl(decl::Static::NonStatic, func),
    Decl::FnDecl(decl::Static::NonStatic, funcv),
  ));
  decl::ClassDecl {
    name: class_name,
    extends: decl::ClassExtends::Qualified(vec!(String::from("GDLisp"), String::from("Function"))),
    body: class_body,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ir::arglist::VarArg;

  fn compile_vararg(specs: FnSpecs) -> String {
    let result = generate_lambda_vararg(specs);
    Decl::FnDecl(decl::Static::NonStatic, result).to_gd(0)
  }

  #[test]
  fn test_lambda_vararg() {
    assert_eq!(compile_vararg(FnSpecs::new(0, 0, None)), "func call_funcv(args):\n    if args == null:\n        return call_func()\n    else:\n        push_error(\"Too many arguments\")\n");
    assert_eq!(compile_vararg(FnSpecs::new(0, 0, Some(VarArg::RestArg))), "func call_funcv(args):\n    return call_func(args)\n");
    assert_eq!(compile_vararg(FnSpecs::new(0, 0, Some(VarArg::ArrArg))), "func call_funcv(args):\n    return call_func(GDLisp.list_to_array(args))\n");
    assert_eq!(compile_vararg(FnSpecs::new(1, 0, Some(VarArg::RestArg))), "func call_funcv(args):\n    var required_0 = null\n    if args == null:\n        push_error(\"Not enough arguments\")\n    else:\n        required_0 = args.car\n        args = args.cdr\n    return call_func(required_0, args)\n");
    assert_eq!(compile_vararg(FnSpecs::new(0, 1, Some(VarArg::RestArg))), "func call_funcv(args):\n    var optional_0 = null\n    if args == null:\n        optional_0 = null\n    else:\n        optional_0 = args.car\n        args = args.cdr\n    return call_func(optional_0, args)\n");
  }

}
