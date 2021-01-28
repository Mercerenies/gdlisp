
pub mod lambda;

use crate::ir;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::stmt_wrapper;
use crate::compile::error::Error;
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::expr::Expr;
use crate::gdscript::literal::Literal;

type IRExpr = ir::expr::Expr;

pub fn compile_if_stmt<'a>(compiler: &mut Compiler<'a>,
                           builder: &mut StmtBuilder,
                           table: &mut SymbolTable,
                           c: &IRExpr,
                           t: &IRExpr,
                           f: &IRExpr,
                           needs_result: NeedsResult)
                           -> Result<StExpr, Error> {
  let (destination, result) = needs_result.into_destination(compiler, builder, "_if");
  let cond_expr = compiler.compile_expr(builder, table, c, NeedsResult::Yes)?.0;
  let mut true_builder = StmtBuilder::new();
  let mut false_builder = StmtBuilder::new();
  compiler.compile_stmt(&mut true_builder , table, destination.as_ref(), t)?;
  compiler.compile_stmt(&mut false_builder, table, destination.as_ref(), f)?;
  let true_body  =  true_builder.build_into(builder);
  let false_body = false_builder.build_into(builder);
  builder.append(stmt::if_else(cond_expr, true_body, false_body));
  Ok(StExpr(result, false))
}

pub fn compile_cond_stmt<'a>(compiler: &mut Compiler<'a>,
                             builder: &mut StmtBuilder,
                             table: &mut SymbolTable,
                             clauses: &Vec<(IRExpr, Option<IRExpr>)>,
                             needs_result: NeedsResult)
                             -> Result<StExpr, Error> {
  let (destination, result) = needs_result.into_destination(compiler, builder, "_cond");
  let init: Vec<Stmt> = destination.wrap_to_stmts(Compiler::nil_expr());
  let body = clauses.iter().rev().fold(Ok(init), |acc: Result<_, Error>, curr| {
    let acc = acc?;
    let (cond, body) = curr;
    match body {
      None => {
        let mut outer_builder = StmtBuilder::new();
        let mut inner_builder = StmtBuilder::new();
        let cond = compiler.compile_expr(&mut outer_builder, table, cond, NeedsResult::Yes)?.0;
        let var_name = compiler.declare_var(&mut outer_builder, "_cond", Some(cond));
        destination.wrap_to_builder(&mut inner_builder, StExpr(Expr::Var(var_name.clone()), false));
        let if_branch = inner_builder.build_into(builder);
        outer_builder.append(stmt::if_else(Expr::Var(var_name.clone()), if_branch, acc));
        Ok(outer_builder.build_into(builder))
      }
      Some(body) => {
        let mut outer_builder = StmtBuilder::new();
        let mut inner_builder = StmtBuilder::new();
        let cond = compiler.compile_expr(&mut outer_builder, table, cond, NeedsResult::Yes)?.0;
        compiler.compile_stmt(&mut inner_builder, table, destination.as_ref(), body)?;
        let if_branch = inner_builder.build_into(builder);
        outer_builder.append(stmt::if_else(cond, if_branch, acc));
        Ok(outer_builder.build_into(builder))
      }
    }
  })?;
  builder.append_all(&mut body.into_iter());
  Ok(StExpr(result, false))
}

pub fn compile_while_stmt<'a>(compiler: &mut Compiler<'a>,
                              builder: &mut StmtBuilder,
                              table: &mut SymbolTable,
                              cond: &IRExpr,
                              body: &IRExpr,
                              _needs_result: NeedsResult)
                              -> Result<StExpr, Error> {
  // If the condition fits in a single GDScript expression, then we'll
  // just compile straight to a GDScript while loop. If not, then we
  // need to compile to "while True:" and have a break statement when
  // we check our conditional. So, to figure out whether the condition
  // fits in a single expression, we'll compile it with a temporary
  // builder and then ask that builder whether or not it received any
  // statements.
  let mut cond_builder = StmtBuilder::new();
  let mut body_builder = StmtBuilder::new();
  let mut cond_expr = compiler.compile_expr(&mut cond_builder, table, cond, NeedsResult::Yes)?.0;
  let cond_body = cond_builder.build_into(builder);
  if !cond_body.is_empty() {
    // Compound while form
    body_builder.append_all(&mut cond_body.into_iter());
    body_builder.append(stmt::if_then(cond_expr, vec!(Stmt::BreakStmt)));
    cond_expr = Expr::Literal(Literal::Bool(true));
  }
  compiler.compile_stmt(&mut body_builder, table, &stmt_wrapper::Vacuous, body)?;
  let body = body_builder.build_into(builder);
  builder.append(Stmt::WhileLoop(stmt::WhileLoop { condition: cond_expr, body: body }));
  Ok(Compiler::nil_expr())
}
