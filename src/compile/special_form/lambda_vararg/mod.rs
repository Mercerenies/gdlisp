
pub mod builder;

use builder::LambdaVarargBuilder;
use crate::ir::arglist::vararg::VarArg;
use crate::compile::symbol_table::function_call::FnSpecs;
use crate::gdscript::decl::{self, Decl, DeclF, VarDecl};
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::library;
use crate::gdscript::class_extends::ClassExtends;
use crate::gdscript::arglist::ArgList;
use crate::pipeline::source::SourceOffset;

use std::convert::TryInto;

const ARGS_VAR: &str = "args";

pub fn generate_lambda_vararg(specs: FnSpecs, pos: SourceOffset) -> decl::FnDecl {
  let mut builder = LambdaVarargBuilder::new(ARGS_VAR.to_owned(), pos);

  let required: Vec<_> = (0..specs.required).map(|i| format!("required_{}", i)).collect();
  let optional: Vec<_> = (0..specs.optional).map(|i| format!("optional_{}", i)).collect();

  for req in &required {
    builder.declare_argument_var(req.to_owned());
    builder.if_args_is_empty(|builder| {
      builder.push_error("Not enough arguments");
    }, |builder| {
      builder.pop_argument(req);
    });
  }

  for opt in &optional {
    builder.declare_argument_var(opt.to_owned());
    builder.if_args_is_empty(|builder| {
      builder.assign_to_var(opt, Expr::null(pos));
    }, |builder| {
      builder.pop_argument(opt);
    });
  }

  match specs.rest {
    Some(VarArg::RestArg) => {
      builder.pop_rest_of_arguments();
      builder.call_function_with_arguments("call_func");
    }
    Some(VarArg::ArrArg) => {
      builder.pop_rest_of_arguments_with(|args| {
        Expr::call(Some(Expr::var(library::GDLISP_NAME, pos)), "list_to_array", vec!(args), pos)
      });
      builder.call_function_with_arguments("call_func");
    }
    None => {
      builder.if_args_is_empty(|builder| {
        builder.call_function_with_arguments("call_func");
      }, |builder| {
        builder.push_error("Too many arguments");
      });
    }
  }

  decl::FnDecl {
    name: String::from("call_funcv"),
    args: ArgList::required(vec!(String::from(ARGS_VAR))),
    body: builder.build(),
  }
}

pub fn generate_lambda_class(class_name: String,
                             specs: FnSpecs,
                             args: ArgList,
                             closed_vars: &[String],
                             lambda_body: Vec<Stmt>,
                             pos: SourceOffset)
                             -> decl::ClassDecl {
  let func_name = String::from("call_func");
  let func = decl::FnDecl {
    name: func_name,
    args: args,
    body: lambda_body,
  };
  let funcv = generate_lambda_vararg(specs, pos);
  let mut constructor_body = Vec::new();
  for name in closed_vars.iter() {
    constructor_body.push(super::assign_to_compiler(name.to_string(), name.to_string(), pos));
  }
  let r: i32  = specs.required.try_into().unwrap();
  let o: i32  = specs.optional.try_into().unwrap();
  let x: i32  = VarArg::arg_to_const(specs.rest);
  constructor_body.push(super::assign_expr_to_compiler(String::from("__gdlisp_required"), Expr::from_value(r, pos)));
  constructor_body.push(super::assign_expr_to_compiler(String::from("__gdlisp_optional"), Expr::from_value(o, pos)));
  constructor_body.push(super::assign_expr_to_compiler(String::from("__gdlisp_rest"), Expr::from_value(x, pos)));
  let constructor =
    decl::FnDecl {
      name: String::from(library::CONSTRUCTOR_NAME),
      args: ArgList::required(closed_vars.iter().map(|x| x.to_owned()).collect()),
      body: constructor_body,
    };
  let mut class_body = vec!();
  for var in closed_vars {
    class_body.push(Decl::new(DeclF::VarDecl(VarDecl::simple(var.to_owned())), pos));
  }
  class_body.append(&mut vec!(
    Decl::new(DeclF::FnDecl(decl::Static::NonStatic, constructor), pos),
    Decl::new(DeclF::FnDecl(decl::Static::NonStatic, func), pos),
    Decl::new(DeclF::FnDecl(decl::Static::NonStatic, funcv), pos),
  ));
  decl::ClassDecl {
    name: class_name,
    extends: ClassExtends::Qualified(vec!(String::from("GDLisp"), String::from("Function"))),
    body: class_body,
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ir::arglist::vararg::VarArg;

  fn compile_vararg(specs: FnSpecs) -> String {
    let result = generate_lambda_vararg(specs, SourceOffset::default());
    Decl::new(DeclF::FnDecl(decl::Static::NonStatic, result), SourceOffset::default()).to_gd(0)
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
