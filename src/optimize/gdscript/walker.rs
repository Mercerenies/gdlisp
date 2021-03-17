
use crate::gdscript::stmt::{self, Stmt};
use crate::compile::error::Error;

pub struct StmtWalker<'a> {
  pub imp: Box<dyn Fn(&Stmt) -> Result<Vec<Stmt>, Error> + 'a>,
}

impl<'a> StmtWalker<'a> {

  pub fn new(function: impl Fn(&Stmt) -> Result<Vec<Stmt>, Error> + 'a) -> StmtWalker<'a> {
    StmtWalker { imp: Box::new(function) }
  }

  pub fn walk_stmts(&self, stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
    let mut result = Vec::new();
    for stmt in stmts {
      result.extend(self.walk_stmt(stmt)?);
    }
    Ok(result)
  }

  pub fn walk_stmt(&self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    // Postorder traversal; first recurse on the constituents
    let stmt = match stmt {
      Stmt::Expr(_) | Stmt::PassStmt | Stmt::BreakStmt | Stmt::ContinueStmt
        | Stmt::VarDecl(_, _) | Stmt::ReturnStmt(_) | Stmt::Assign(_, _, _) => stmt.clone(),
      Stmt::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause }) => {
        let if_clause = (if_clause.0.clone(), self.walk_stmts(&if_clause.1)?);
        let elif_clauses = elif_clauses.iter().map(|(e, s)| self.walk_stmts(s).map(|s| (e.clone(), s))).collect::<Result<_, _>>()?;
        let else_clause = else_clause.as_ref().map(|s| self.walk_stmts(s)).transpose()?;
        Stmt::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause })
      }
      Stmt::ForLoop(stmt::ForLoop { iter_var, collection, body }) => {
        let body = self.walk_stmts(&body)?;
        Stmt::ForLoop(stmt::ForLoop {
          iter_var: iter_var.clone(),
          collection: collection.clone(),
          body: body,
        })
      }
      Stmt::WhileLoop(stmt::WhileLoop { condition, body }) => {
        let body = self.walk_stmts(&body)?;
        Stmt::WhileLoop(stmt::WhileLoop {
          condition: condition.clone(),
          body: body,
        })
      }
      Stmt::MatchStmt(expr, clauses) => {
        let clauses = clauses.iter().map(|(p, s)| self.walk_stmts(s).map(|s| (p.clone(), s))).collect::<Result<_, _>>()?;
        Stmt::MatchStmt(expr.clone(), clauses)
      }
    };
    // Now call the function
    (self.imp)(&stmt)
  }

}

pub fn walk_stmt<'a>(stmt: &Stmt, walker: impl Fn(&Stmt) -> Result<Vec<Stmt>, Error> + 'a)
                     -> Result<Vec<Stmt>, Error> {
  let walker = StmtWalker::new(walker);
  walker.walk_stmt(&stmt)
}

pub fn walk_stmts<'a>(stmts: &[Stmt], walker: impl Fn(&Stmt) -> Result<Vec<Stmt>, Error> + 'a)
                      -> Result<Vec<Stmt>, Error> {
  let walker = StmtWalker::new(walker);
  walker.walk_stmts(&stmts)
}

// TODO Test me :)