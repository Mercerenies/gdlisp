
use crate::ir;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::error::Error;
use crate::gdscript::stmt;

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
