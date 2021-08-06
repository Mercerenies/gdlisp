
use crate::gdscript::stmt::{self, Stmt, StmtF};
use crate::compile::error::Error;

pub struct StmtWalker<'a> {
  pub imp: Box<WalkFn<'a>>,
}

pub type WalkFn<'a> = dyn FnMut(&[Stmt]) -> Result<Vec<Stmt>, Error> + 'a;

impl<'a> StmtWalker<'a> {

  pub fn new(function: impl FnMut(&[Stmt]) -> Result<Vec<Stmt>, Error> + 'a) -> StmtWalker<'a> {
    StmtWalker { imp: Box::new(function) }
  }

  pub fn walk_stmts(&mut self, stmts: &[Stmt]) -> Result<Vec<Stmt>, Error> {
    let mut result = Vec::new();
    for stmt in stmts {
      result.extend(self.walk_stmt(stmt)?);
    }
    (self.imp)(&result)
  }

  pub fn walk_stmt(&mut self, stmt: &Stmt) -> Result<Vec<Stmt>, Error> {
    let new_stmt = match &stmt.value {
      StmtF::Expr(_) | StmtF::PassStmt | StmtF::BreakStmt | StmtF::ContinueStmt
        | StmtF::VarDecl(_, _) | StmtF::ReturnStmt(_) | StmtF::Assign(_, _, _) => stmt.value.clone(),
      StmtF::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause }) => {
        let if_clause = (if_clause.0.clone(), self.walk_stmts(&if_clause.1)?);
        let elif_clauses = elif_clauses.iter().map(|(e, s)| self.walk_stmts(s).map(|s| (e.clone(), s))).collect::<Result<_, _>>()?;
        let else_clause = else_clause.as_ref().map(|s| self.walk_stmts(s)).transpose()?;
        StmtF::IfStmt(stmt::IfStmt { if_clause, elif_clauses, else_clause })
      }
      StmtF::ForLoop(stmt::ForLoop { iter_var, collection, body }) => {
        let body = self.walk_stmts(&body)?;
        StmtF::ForLoop(stmt::ForLoop {
          iter_var: iter_var.clone(),
          collection: collection.clone(),
          body: body,
        })
      }
      StmtF::WhileLoop(stmt::WhileLoop { condition, body }) => {
        let body = self.walk_stmts(&body)?;
        StmtF::WhileLoop(stmt::WhileLoop {
          condition: condition.clone(),
          body: body,
        })
      }
      StmtF::MatchStmt(expr, clauses) => {
        let clauses = clauses.iter().map(|(p, s)| self.walk_stmts(s).map(|s| (p.clone(), s))).collect::<Result<_, _>>()?;
        StmtF::MatchStmt(expr.clone(), clauses)
      }
    };
    Ok(vec!(Stmt::new(new_stmt, stmt.pos)))
  }

}

pub fn on_each_stmt<'a>(mut walker: impl FnMut(&Stmt) -> Result<Vec<Stmt>, Error> + 'a)
                        -> impl FnMut(&[Stmt]) -> Result<Vec<Stmt>, Error> + 'a {
  move |stmts| {
    let mut result = Vec::new();
    for stmt in stmts {
      result.extend(walker(stmt)?);
    }
    Ok(result)
  }
}

pub fn walk_stmt<'a>(stmt: &Stmt, walker: impl FnMut(&[Stmt]) -> Result<Vec<Stmt>, Error> + 'a)
                     -> Result<Vec<Stmt>, Error> {
  let stmts = vec!(stmt.clone());
  walk_stmts(&stmts[..], walker)
}

pub fn walk_stmts<'a>(stmts: &[Stmt], walker: impl FnMut(&[Stmt]) -> Result<Vec<Stmt>, Error> + 'a)
                      -> Result<Vec<Stmt>, Error> {
  let mut walker = StmtWalker::new(walker);
  walker.walk_stmts(&stmts)
}

// TODO Test me :)
