
pub mod lambda;
pub mod flet;
pub mod lambda_class;
pub mod lambda_vararg;
pub mod closure;
pub mod let_block;

use crate::ir;
use crate::ir::access_type::AccessType;
use crate::compile::{Compiler, StExpr, NeedsResult};
use crate::compile::body::builder::StmtBuilder;
use crate::compile::symbol_table::HasSymbolTable;
use crate::compile::symbol_table::local_var::LocalVar;
use crate::compile::stmt_wrapper;
use crate::compile::error::Error;
use crate::compile::stateful::SideEffects;
use crate::compile::names;
use crate::compile::factory;
use crate::compile::frame::CompilerFrame;
use crate::gdscript::stmt::{self, Stmt, StmtF};
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::op;
use crate::pipeline::source::SourceOffset;
use crate::util;

type IRExpr = ir::expr::Expr;

pub fn compile_cond_stmt(frame: &mut CompilerFrame<StmtBuilder>,
                         clauses: &[(IRExpr, Option<IRExpr>)],
                         needs_result: NeedsResult,
                         pos: SourceOffset)
                         -> Result<StExpr, Error> {
  let (destination, result) = needs_result.into_destination(frame.compiler.name_generator(), frame.builder, "_cond", pos);
  let init: Vec<Stmt> = util::option_to_vec(destination.wrap_to_stmt(Compiler::nil_expr(pos)));
  let body = clauses.iter().rev().fold(Ok(init), |acc: Result<_, Error>, curr| {
    let acc = acc?;
    let (cond, body) = curr;
    match body {
      None => {
        // Outer local builder (for constructing any values needed in
        // the conditional).
        frame.with_local_builder(|frame| {
          let cond = frame.compile_expr(cond, NeedsResult::Yes)?.expr;
          let var_name = factory::declare_var(frame.compiler.name_generator(), frame.builder, "_cond", Some(cond), pos);
          let var_expr = StExpr { expr: Expr::new(ExprF::Var(var_name.clone()), pos), side_effects: SideEffects::None };
          // Inner local builder (for the contents of the "true"
          // block).
          let if_branch = frame.with_local_builder_ok(|frame| {
            destination.wrap_to_builder(frame.builder, var_expr);
          });
          frame.builder.append(stmt::if_else(Expr::new(ExprF::Var(var_name), pos), if_branch, acc, pos));
          Ok(())
        })
      }
      Some(body) => {
        // Outer local builder (for constructing any values needed in
        // the conditional).
        frame.with_local_builder(|frame| {
          let cond = frame.compile_expr(cond, NeedsResult::Yes)?.expr;
          // Inner local builder (for the contents of the "true"
          // block).
          let if_branch = frame.with_local_builder(|frame| {
            frame.compile_stmt(destination.as_ref(), body)
          })?;
          frame.builder.append(stmt::if_else(cond, if_branch, acc, pos));
          Ok(())
        })
      }
    }
  })?;
  frame.builder.append_all(&mut body.into_iter());
  Ok(StExpr { expr: result, side_effects: SideEffects::None })
}

pub fn compile_while_stmt(frame: &mut CompilerFrame<StmtBuilder>,
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
  let (mut cond_expr, cond_body) = frame.with_local_builder_result(|frame| {
    frame.compile_expr(cond, NeedsResult::Yes).map(|x| x.expr)
  })?;
  let body = frame.with_local_builder(|frame| {
    if !cond_body.is_empty() {
      // Compound while form
      frame.builder.append_all(&mut cond_body.into_iter());
      let inner_cond_expr = cond_expr.clone().unary(op::UnaryOp::Not, pos);
      frame.builder.append(stmt::if_then(inner_cond_expr, vec!(Stmt::new(StmtF::BreakStmt, cond.pos)), cond.pos));
      cond_expr = Expr::from_value(true, pos);
    }
    frame.compile_stmt(&stmt_wrapper::Vacuous, body)?;
    Ok(())
  })?;
  frame.builder.append(Stmt::new(StmtF::WhileLoop(stmt::WhileLoop { condition: cond_expr, body: body }), pos));
  Ok(Compiler::nil_expr(pos))
}

pub fn compile_for_stmt(frame: &mut CompilerFrame<StmtBuilder>,
                        name: &str,
                        iter: &IRExpr,
                        body: &IRExpr,
                        _needs_result: NeedsResult,
                        pos: SourceOffset)
                        -> Result<StExpr, Error> {
  let closure_vars = body.get_locals();
  let citer = frame.compile_expr(iter, NeedsResult::Yes)?.expr;
  let var_name = frame.name_generator().generate_with(&names::lisp_to_gd(name));
  let body = frame.with_local_builder(|frame| {
    let local_var = LocalVar::local(var_name.to_owned(), *closure_vars.get(&name).unwrap_or(&AccessType::None));
    frame.with_local_var(name.to_owned(), local_var, |frame| {
      frame.compile_stmt(&stmt_wrapper::Vacuous, body).map(|_| ())
    })
  })?;
  frame.builder.append(Stmt::new(StmtF::ForLoop(stmt::ForLoop { iter_var: var_name, collection: citer, body: body }), pos));
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
