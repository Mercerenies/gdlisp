
pub struct If;

use super::SpecialForm;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::compile::stmt_wrapper::{self, StmtWrapper};
use crate::sxp::ast::AST;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt;

impl SpecialForm for If {

  fn compile<'a>(&mut self,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 tail: &[&AST],
                 needs_result: NeedsResult)
                 -> Result<StExpr, Error> {
    let (cond, t, f) = match tail {
      [] | [_] => Err(Error::TooFewArgs(String::from("if"), tail.len())),
      [cond, t] => Ok((*cond, *t, &AST::Nil)),
      [cond, t, f] => Ok((*cond, *t, *f)),
      _ => Err(Error::TooManyArgs(String::from("if"), tail.len())),
    }?;
    let (destination, result) = if needs_result.into() {
      let var_name = compiler.declare_var(builder, "_if", None);
      let destination = Box::new(stmt_wrapper::AssignToVar(var_name.clone())) as Box<dyn StmtWrapper>;
      (destination, StExpr(Expr::Var(var_name), false))
    } else {
      let destination = Box::new(stmt_wrapper::Vacuous) as Box<dyn StmtWrapper>;
      (destination, Compiler::nil_expr())
    };
    let cond_expr = compiler.compile_expr(builder, cond, NeedsResult::Yes)?.0;
    let mut true_builder = StmtBuilder::new();
    let mut false_builder = StmtBuilder::new();
    compiler.compile_stmt(&mut true_builder, destination.as_ref(), t)?;
    compiler.compile_stmt(&mut false_builder, destination.as_ref(), f)?;
    let true_body = true_builder.build_into(builder);
    let false_body = false_builder.build_into(builder);
    builder.append(stmt::if_else(cond_expr, true_body, false_body));
    Ok(result)
  }

}

