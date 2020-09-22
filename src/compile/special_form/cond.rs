
pub struct Cond;

use super::SpecialForm;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::error::Error;
use crate::compile::stmt_wrapper::{self, StmtWrapper};
use crate::compile::symbol_table::SymbolTable;
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::{self, Stmt};

use std::convert::TryInto;

impl SpecialForm for Cond {

  fn compile<'a, 'b>(&mut self,
                     compiler: &mut Compiler<'a>,
                     builder: &mut StmtBuilder,
                     table: &mut impl SymbolTable<'b>,
                     tail: &[&AST],
                     needs_result: NeedsResult)
             -> Result<StExpr, Error> {
    let (destination, result) = if needs_result.into() {
      let var_name = compiler.declare_var(builder, "_cond", None);
      let destination = Box::new(stmt_wrapper::AssignToVar(var_name.clone())) as Box<dyn StmtWrapper>;
      (destination, StExpr(Expr::Var(var_name), false))
    } else {
      let destination = Box::new(stmt_wrapper::Vacuous) as Box<dyn StmtWrapper>;
      (destination, Compiler::nil_expr())
    };
    let init: Vec<Stmt> = destination.wrap_to_stmts(Compiler::nil_expr());
    let body = tail.iter().rev().fold(Ok(init), |acc: Result<_, Error>, curr| {
      let acc = acc?;
      let vec: Vec<&AST> = DottedExpr::new(curr).try_into()?;
      match vec.len() {
        0 =>
          Err(Error::InvalidArg(String::from("cond"), (*curr).clone(), String::from("nonempty list"))),
        1 => {
          let mut outer_builder = StmtBuilder::new();
          let mut inner_builder = StmtBuilder::new();
          let cond = compiler.compile_expr(&mut outer_builder, table, vec[0], NeedsResult::Yes)?.0;
          let var_name = compiler.declare_var(&mut outer_builder, "_cond", Some(cond));
          destination.wrap_to_builder(&mut inner_builder, StExpr(Expr::Var(var_name.clone()), false));
          let if_branch = inner_builder.build_into(builder);
          outer_builder.append(stmt::if_else(Expr::Var(var_name.clone()), if_branch, acc));
          Ok(outer_builder.build_into(builder))
        }
        _ => {
          let mut outer_builder = StmtBuilder::new();
          let mut inner_builder = StmtBuilder::new();
          let cond = compiler.compile_expr(&mut outer_builder, table, vec[0], NeedsResult::Yes)?.0;
          let result = compiler.compile_stmts(&mut inner_builder, table, &vec[1..], needs_result)?;
          destination.wrap_to_builder(&mut inner_builder, result);
          let if_branch = inner_builder.build_into(builder);
          outer_builder.append(stmt::if_else(cond, if_branch, acc));
          Ok(outer_builder.build_into(builder))
        }
      }
    })?;
    builder.append_all(&mut body.into_iter());
    Ok(result)
  }

}

