
pub struct Funcall;

use super::SpecialForm;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::compile::symbol_table::SymbolTable;
use crate::gdscript::expr::Expr;
use crate::sxp::ast::AST;

// TODO funcall is a special form right now. Ideally, it would be a
// function (with some special compilation rules, for efficiency), but
// there are some annoying complexities with varargs and whatnot that
// make that tricky.

impl SpecialForm for Funcall {

  fn compile<'a>(&mut self,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 table: &mut impl SymbolTable,
                 tail: &[&AST],
                 _needs_result: NeedsResult)
                 -> Result<StExpr, Error> {
    if tail.len() <= 0 {
      return Err(Error::TooFewArgs(String::from("funcall"), 1));
    }
    let func = tail[0];
    let args = &tail[1..];
    let func_expr = compiler.compile_expr(builder, table, func, NeedsResult::Yes)?.0;
    let args_expr = args.iter().map(|arg| {
      compiler.compile_expr(builder, table, arg, NeedsResult::Yes).map(|x| x.0)
    }).collect::<Result<Vec<_>, _>>()?;
    let fn_name = String::from("call_func");
    let expr = Expr::Call(Some(Box::new(func_expr)), fn_name, args_expr);
    Ok(StExpr(expr, true))
  }

}
