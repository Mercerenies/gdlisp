
use super::stmt_walker;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::expr::Expr;
use crate::compile::error::Error;

fn walk_impl<'a>(walker: &mut (impl FnMut(&Expr) -> Result<Expr, Error> + 'a), stmt: &Stmt)
                 -> Result<Vec<Stmt>, Error> {
  todo!()
}

pub fn walk_expr<'a>(stmt: &Stmt, mut walker: impl FnMut(&Expr) -> Result<Expr, Error> + 'a)
                     -> Result<Vec<Stmt>, Error> {
  stmt_walker::walk_stmt(stmt, stmt_walker::on_each_stmt(|s| walk_impl(&mut walker, s)))
}

pub fn walk_exprs<'a>(stmts: &[Stmt], mut walker: impl FnMut(&Expr) -> Result<Expr, Error> + 'a)
                      -> Result<Vec<Stmt>, Error> {
  stmt_walker::walk_stmts(stmts, stmt_walker::on_each_stmt(|s| walk_impl(&mut walker, s)))
}
