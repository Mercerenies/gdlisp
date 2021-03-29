
use super::stmt_walker;
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::expr::{self, Expr};
use crate::compile::error::Error;

pub fn walk_expr<'a>(stmt: &Stmt, mut walker: impl FnMut(&Expr) -> Result<Expr, Error> + 'a)
                     -> Result<Vec<Stmt>, Error> {
  stmt_walker::walk_stmt(stmt, stmt_walker::on_each_stmt(|s| walk_impl(&mut walker, s)))
}

pub fn walk_exprs<'a>(stmts: &[Stmt], mut walker: impl FnMut(&Expr) -> Result<Expr, Error> + 'a)
                      -> Result<Vec<Stmt>, Error> {
  stmt_walker::walk_stmts(stmts, stmt_walker::on_each_stmt(|s| walk_impl(&mut walker, s)))
}

fn walk_impl_expr<'a>(walker: &mut (impl FnMut(&Expr) -> Result<Expr, Error> + 'a), expr: &Expr)
                      -> Result<Expr, Error> {
  let mut expr = walker(expr)?;
  match &mut expr {
    Expr::Subscript(a, b) => {
      **a = walk_impl_expr(walker, &**a)?;
      **b = walk_impl_expr(walker, &**b)?;
    }
    Expr::Attribute(a, _) => {
      **a = walk_impl_expr(walker, &**a)?;
    }
    Expr::Call(lhs, _, args) => {
      if let Some(lhs) = lhs.as_mut() {
        **lhs = walk_impl_expr(walker, lhs)?;
      }
      for arg in args.iter_mut() {
        *arg = walk_impl_expr(walker, arg)?;
      }
    }
    Expr::SuperCall(_, args) => {
      for arg in args.iter_mut() {
        *arg = walk_impl_expr(walker, arg)?;
      }
    }
    Expr::Unary(_, a) => {
      **a = walk_impl_expr(walker, &**a)?;
    }
    Expr::Binary(a, _, b) => {
      **a = walk_impl_expr(walker, &**a)?;
      **b = walk_impl_expr(walker, &**b)?;
    }
    Expr::TernaryIf(expr::TernaryIf { true_case: a, cond: b, false_case: c }) => {
      **a = walk_impl_expr(walker, &**a)?;
      **b = walk_impl_expr(walker, &**b)?;
      **c = walk_impl_expr(walker, &**c)?;
    }
    Expr::ArrayLit(args) => {
      for arg in args.iter_mut() {
        *arg = walk_impl_expr(walker, arg)?;
      }
    }
    Expr::Var(_) | Expr::Literal(_) => {}
  }
  Ok(expr)
}

fn walk_impl<'a>(walker: &mut (impl FnMut(&Expr) -> Result<Expr, Error> + 'a), stmt: &Stmt)
                 -> Result<Vec<Stmt>, Error> {
  let new_stmt = match stmt {
    Stmt::Expr(e) => {
      Stmt::Expr(walk_impl_expr(walker, &e)?)
    }
    Stmt::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause }) => {
      let new_if_stmt = stmt::IfStmt {
        if_clause: (walk_impl_expr(walker, &if_clause.0)?, if_clause.1.clone()),
        elif_clauses: (elif_clauses.iter().map(|(e, s)| Ok((walk_impl_expr(walker, e)?, s.clone()))).collect::<Result<_, Error>>()?),
        else_clause: else_clause.clone(),
      };
      Stmt::IfStmt(new_if_stmt)
    }
    Stmt::ForLoop(stmt::ForLoop { iter_var, collection, body }) => {
      let new_for_loop = stmt::ForLoop {
        iter_var: iter_var.clone(),
        collection: walk_impl_expr(walker, collection)?,
        body: body.clone(),
      };
      Stmt::ForLoop(new_for_loop)
    }
    Stmt::WhileLoop(stmt::WhileLoop { condition, body }) => {
      let new_while_loop = stmt::WhileLoop {
        condition: walk_impl_expr(walker, condition)?,
        body: body.clone(),
      };
      Stmt::WhileLoop(new_while_loop)
    }
    Stmt::MatchStmt(expr, clauses) => {
      Stmt::MatchStmt(walk_impl_expr(walker, expr)?, clauses.clone())
    }
    Stmt::VarDecl(name, expr) => {
      Stmt::VarDecl(name.clone(), walk_impl_expr(walker, expr)?)
    }
    Stmt::ReturnStmt(expr) => {
      Stmt::ReturnStmt(walk_impl_expr(walker, expr)?)
    }
    Stmt::Assign(lhs, op, rhs) => {
      let lhs = walk_impl_expr(walker, &*lhs)?;
      let rhs = walk_impl_expr(walker, &*rhs)?;
      Stmt::Assign(Box::new(lhs), *op, Box::new(rhs))
    }
    Stmt::PassStmt | Stmt::BreakStmt | Stmt::ContinueStmt => {
      stmt.clone()
    }
  };
  Ok(vec!(new_stmt))
}
