
pub mod lambda;
pub mod flet;
pub mod lambda_class;
pub mod lambda_vararg;

use crate::ir;
use crate::ir::access_type::AccessType;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::{SymbolTable, HasSymbolTable};
use crate::compile::symbol_table::local_var::LocalVar;
use crate::compile::stmt_wrapper;
use crate::compile::error::Error;
use crate::compile::stateful::SideEffects;
use crate::compile::names;
use crate::compile::factory;
use crate::gdscript::stmt::{self, Stmt, StmtF};
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::literal::Literal;
use crate::gdscript::op;
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::util;

type IRExpr = ir::expr::Expr;

pub fn compile_cond_stmt(compiler: &mut Compiler,
                         pipeline: &mut Pipeline,
                         builder: &mut StmtBuilder,
                         table: &mut SymbolTable,
                         clauses: &[(IRExpr, Option<IRExpr>)],
                         needs_result: NeedsResult,
                         pos: SourceOffset)
                         -> Result<StExpr, Error> {
  let (destination, result) = needs_result.into_destination(compiler, builder, "_cond", pos);
  let init: Vec<Stmt> = util::option_to_vec(destination.wrap_to_stmt(Compiler::nil_expr(pos)));
  let body = clauses.iter().rev().fold(Ok(init), |acc: Result<_, Error>, curr| {
    let acc = acc?;
    let (cond, body) = curr;
    match body {
      None => {
        let mut outer_builder = StmtBuilder::new();
        let mut inner_builder = StmtBuilder::new();
        let cond = compiler.compile_expr(pipeline, &mut outer_builder, table, cond, NeedsResult::Yes)?.expr;
        let var_name = factory::declare_var(compiler.name_generator(), &mut outer_builder, "_cond", Some(cond), pos);
        let var_expr = StExpr { expr: Expr::new(ExprF::Var(var_name.clone()), pos), side_effects: SideEffects::None };
        destination.wrap_to_builder(&mut inner_builder, var_expr);
        let if_branch = inner_builder.build_into(builder);
        outer_builder.append(stmt::if_else(Expr::new(ExprF::Var(var_name), pos), if_branch, acc, pos));
        Ok(outer_builder.build_into(builder))
      }
      Some(body) => {
        let mut outer_builder = StmtBuilder::new();
        let mut inner_builder = StmtBuilder::new();
        let cond = compiler.compile_expr(pipeline, &mut outer_builder, table, cond, NeedsResult::Yes)?.expr;
        compiler.frame(pipeline, &mut inner_builder, table).compile_stmt(destination.as_ref(), body)?;
        let if_branch = inner_builder.build_into(builder);
        outer_builder.append(stmt::if_else(cond, if_branch, acc, pos));
        Ok(outer_builder.build_into(builder))
      }
    }
  })?;
  builder.append_all(&mut body.into_iter());
  Ok(StExpr { expr: result, side_effects: SideEffects::None })
}

pub fn compile_while_stmt(compiler: &mut Compiler,
                          pipeline: &mut Pipeline,
                          builder: &mut StmtBuilder,
                          table: &mut SymbolTable,
                          cond: &IRExpr,
                          body: &IRExpr,
                          _needs_result: NeedsResult,
                          pos: SourceOffset)
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
  let mut cond_expr = compiler.compile_expr(pipeline, &mut cond_builder, table, cond, NeedsResult::Yes)?.expr;
  let cond_expr_pos = cond_expr.pos;
  let cond_body = cond_builder.build_into(builder);
  if !cond_body.is_empty() {
    // Compound while form
    body_builder.append_all(&mut cond_body.into_iter());
    let inner_cond_expr = Expr::new(ExprF::Unary(op::UnaryOp::Not, Box::new(cond_expr)), pos);
    body_builder.append(stmt::if_then(inner_cond_expr, vec!(Stmt::new(StmtF::BreakStmt, cond_expr_pos)), cond_expr_pos));
    cond_expr = Expr::new(ExprF::Literal(Literal::Bool(true)), pos);
  }
  compiler.frame(pipeline, &mut body_builder, table).compile_stmt(&stmt_wrapper::Vacuous, body)?;
  let body = body_builder.build_into(builder);
  builder.append(Stmt::new(StmtF::WhileLoop(stmt::WhileLoop { condition: cond_expr, body: body }), pos));
  Ok(Compiler::nil_expr(pos))
}

pub fn compile_for_stmt(compiler: &mut Compiler,
                        pipeline: &mut Pipeline,
                        builder: &mut StmtBuilder,
                        table: &mut SymbolTable,
                        name: &str,
                        iter: &IRExpr,
                        body: &IRExpr,
                        _needs_result: NeedsResult,
                        pos: SourceOffset)
                        -> Result<StExpr, Error> {
  let closure_vars = body.get_locals();
  let citer = compiler.compile_expr(pipeline, builder, table, iter, NeedsResult::Yes)?.expr;
  let var_name = compiler.name_generator().generate_with(&names::lisp_to_gd(name));
  let mut inner_builder = StmtBuilder::new();
  let local_var = LocalVar::local(var_name.to_owned(), *closure_vars.get(&name).unwrap_or(&AccessType::None));
  table.with_local_var(name.to_owned(), local_var, |table| {
    compiler.frame(pipeline, &mut inner_builder, table).compile_stmt(&stmt_wrapper::Vacuous, body)
  })?;
  let body = inner_builder.build_into(builder);
  builder.append(Stmt::new(StmtF::ForLoop(stmt::ForLoop { iter_var: var_name, collection: citer, body: body }), pos));
  Ok(Compiler::nil_expr(pos))
}

/// A [`Stmt`] which assigns the value of the local variable
/// `local_var` to the variable `inst_var` on the Godot `self` object.
pub fn assign_to_compiler(inst_var: String, local_var: String, pos: SourceOffset) -> Stmt {
  assign_expr_to_compiler(inst_var, Expr::new(ExprF::Var(local_var), pos))
}

/// A [`Stmt`] which assigns `expr` to the variable `inst_var` on the
/// Godot `self` object.
pub fn assign_expr_to_compiler(inst_var: String, expr: Expr) -> Stmt {
  let pos = expr.pos;
  let self_target = Expr::self_var(pos).attribute(inst_var, pos);
  Stmt::simple_assign(self_target, expr, pos)
}
