
/*
pub struct Lambda;

use super::SpecialForm;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::stmt_wrapper::StmtWrapper;
use crate::compile::error::Error;
use crate::compile::stmt_wrapper;
use crate::compile::symbol_table::monitored::MonitoredTable;
use crate::compile::symbol_table::concrete::ConcreteTable;
use crate::compile::names::fresh::FreshNameGenerator;
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::arglist::ArgList;
use crate::gdscript::op;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;

use std::convert::TryInto;
use std::borrow::Borrow;

impl SpecialForm for Lambda {

  fn compile<'a>(&mut self,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 table: &mut impl SymbolTable,
                 tail: &[&AST],
                 _needs_result: NeedsResult)
                 -> Result<StExpr, Error> {
    if tail.len() <= 0 {
      return Err(Error::TooFewArgs(String::from("lambda"), 1));
    }
    // TODO Currently, we don't (can't) support varargs directly since
    // GDScript doesn't expose that functionality. Consider some sort
    // of workaround?
    let args: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
    let body = &tail[1..];
    let arg_names = args.iter().map(|curr| {
      match curr {
        AST::Symbol(s) => {
          let ast_name = s.clone();
          let gd_name = compiler.name_generator().generate_with(&ast_name);
          Ok((ast_name, gd_name))
        },
        _ => Err(Error::InvalidArg(String::from("lambda"), (*curr).clone(), String::from("variable name"))),
      }
    }).collect::<Result<Vec<_>, _>>()?;
    let mut lambda_builder = StmtBuilder::new();

    let impl_table = ConcreteTable::new();
    let mut lambda_table = MonitoredTable::new(impl_table);
    for arg in arg_names.clone().into_iter() {
      lambda_table.set_var(arg.0, arg.1);
    }
    for local in table.vars() {
      // We're reusing the local_gd name from the lexical scope here.
      // It should always be safe to do so, since the lambda class
      // will always be instantiated strictly outside of the current
      // lexical scope. (TODO It does produce warnings in Godot if the
      // settings are right, so we might consider generating new names
      // anyway)
      let (local_ast, local_gd) = local;
      lambda_table.set_var(String::from(local_ast), String::from(local_gd));
      lambda_table.monitor_var(String::from(local_gd));
    }

    let result = compiler.compile_stmts(&mut lambda_builder, &mut lambda_table, body, NeedsResult::Yes)?;
    stmt_wrapper::Return.wrap_to_builder(&mut lambda_builder, result);

    // I don't care what order these things end up in, but I do need
    // to get them in some kind of internally consistent order so I
    // can call the constructor with the right arguments.
    let closed_vars = lambda_table.get_used_vars().map(|x| x.borrow()).collect::<Vec<&str>>();

    let arglist = ArgList::required(arg_names.into_iter().map(|x| x.1.clone()).collect());
    let class = generate_lambda_class(&mut compiler.name_generator(),
                                      arglist,
                                      &closed_vars,
                                      builder,
                                      lambda_builder);
    let class_name = class.name.clone();
    builder.add_helper(Decl::ClassDecl(class));
    let constructor_args = closed_vars.into_iter().map(|x| Expr::Var(x.to_owned())).collect();
    let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), constructor_args);
    Ok(StExpr(expr, false))
  }

}

fn generate_lambda_class(gen: &mut FreshNameGenerator,
                         args: ArgList,
                         closed_vars: &Vec<&str>,
                         parent_builder: &mut StmtBuilder,
                         lambda_builder: StmtBuilder)
                         -> decl::ClassDecl {
  let class_name = gen.generate_with("_LambdaBlock");
  let func_name = String::from("call_func");
  let func_body = lambda_builder.build_into(parent_builder);
  let func = decl::FnDecl {
    name: func_name,
    args: args,
    body: func_body,
  };
  let constructor =
    decl::FnDecl {
      name: String::from("_init"),
      args: ArgList::required(closed_vars.iter().map(|x| (*x).to_owned()).collect()),
      body: closed_vars.iter().map(|name| assign_to_self(name.to_string(), name.to_string())).collect(),
    };
  let mut class_body = vec!();
  for var in closed_vars {
    class_body.push(Decl::VarDecl(String::from(*var), None));
  }
  class_body.append(&mut vec!(
    Decl::FnDecl(decl::Static::NonStatic, constructor),
    Decl::FnDecl(decl::Static::NonStatic, func),
  ));
  decl::ClassDecl {
    name: class_name,
    extends: decl::ClassExtends::Named(String::from("Reference")),
    body: class_body,
  }
}

fn assign_to_self(inst_var: String, local_var: String) -> Stmt {
  let self_target = Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("self"))), inst_var));
  let value = Box::new(Expr::Var(local_var));
  Stmt::Assign(self_target, op::AssignOp::Eq, value)
}
*/
