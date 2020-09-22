
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
use crate::gdscript::expr::Expr;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;

use std::convert::TryInto;

// TODO Currently, lambdas don't close over local variables (they just
// get their own new scope). We'll fix this soon.

impl SpecialForm for Lambda {

  fn compile<'a, 'b>(&mut self,
                     compiler: &mut Compiler<'a>,
                     builder: &mut StmtBuilder,
                     table: &mut impl SymbolTable<'b>,
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

    let result = compiler.compile_stmts(&mut lambda_builder, &mut lambda_table, body, NeedsResult::Yes)?;
    stmt_wrapper::Return.wrap_to_builder(&mut lambda_builder, result);

    let arglist = ArgList::required(arg_names.into_iter().map(|x| x.1.clone()).collect());
    let class = generate_lambda_class(&mut compiler.name_generator(), arglist, builder, lambda_builder);
    let class_name = class.name.clone();
    builder.add_helper(Decl::ClassDecl(class));
    let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), vec!());
    Ok(StExpr(expr, false))
  }

}

fn generate_lambda_class(gen: &mut FreshNameGenerator,
                         args: ArgList,
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
  let class_body = vec!(Decl::FnDecl(decl::Static::NonStatic, func));
  decl::ClassDecl {
    name: class_name,
    extends: decl::ClassExtends::Named(String::from("Reference")),
    body: class_body,
  }
}
