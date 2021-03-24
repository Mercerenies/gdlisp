
use super::stmt_walker;
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::expr::Expr;
use crate::compile::error::Error;

pub fn walk_expr<'a>(stmt: &Stmt, mut walker: impl FnMut(&Expr) -> Result<Expr, Error> + 'a)
                     -> Result<Vec<Stmt>, Error> {
  stmt_walker::walk_stmt(stmt, stmt_walker::on_each_stmt(|s| walk_impl(&mut walker, s)))
}

pub fn walk_exprs<'a>(stmts: &[Stmt], mut walker: impl FnMut(&Expr) -> Result<Expr, Error> + 'a)
                      -> Result<Vec<Stmt>, Error> {
  stmt_walker::walk_stmts(stmts, stmt_walker::on_each_stmt(|s| walk_impl(&mut walker, s)))
}

fn walk_impl<'a>(walker: &mut (impl FnMut(&Expr) -> Result<Expr, Error> + 'a), stmt: &Stmt)
                 -> Result<Vec<Stmt>, Error> {
  let new_stmt = match stmt {
    Stmt::Expr(e) => {
      Stmt::Expr(walker(&e)?)
    }
    Stmt::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause }) => {
      let new_if_stmt = stmt::IfStmt {
        if_clause: (walker(&if_clause.0)?, if_clause.1.clone()),
        elif_clauses: (elif_clauses.iter().map(|(e, s)| Ok((walker(e)?, s.clone()))).collect::<Result<_, Error>>()?),
        else_clause: else_clause.clone(),
      };
      Stmt::IfStmt(new_if_stmt)
    }
    Stmt::ForLoop(stmt::ForLoop { iter_var, collection, body }) => {
      let new_for_loop = stmt::ForLoop {
        iter_var: iter_var.clone(),
        collection: walker(collection)?,
        body: body.clone(),
      };
      Stmt::ForLoop(new_for_loop)
    }
    Stmt::WhileLoop(stmt::WhileLoop { condition, body }) => {
      let new_while_loop = stmt::WhileLoop {
        condition: walker(condition)?,
        body: body.clone(),
      };
      Stmt::WhileLoop(new_while_loop)
    }
    Stmt::MatchStmt(expr, clauses) => {
      Stmt::MatchStmt(walker(expr)?, clauses.clone())
    }
    Stmt::VarDecl(name, expr) => {
      Stmt::VarDecl(name.clone(), walker(expr)?)
    }
    Stmt::ReturnStmt(expr) => {
      Stmt::ReturnStmt(walker(expr)?)
    }
    Stmt::Assign(lhs, op, rhs) => {
      let lhs = walker(&*lhs)?;
      let rhs = walker(&*rhs)?;
      Stmt::Assign(Box::new(lhs), *op, Box::new(rhs))
    }
    Stmt::PassStmt | Stmt::BreakStmt | Stmt::ContinueStmt => {
      stmt.clone()
    }
  };
  Ok(vec!(new_stmt))
}
