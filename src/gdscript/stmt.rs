
use crate::gdscript::expr::Expr;

use std::fmt;
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub enum Stmt {
  Expr(Expr),
  IfStmt(IfStmt),
  ForLoop(ForLoop),
  WhileLoop(WhileLoop),
  PassStmt,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
  if_clause: (Expr, Vec<Stmt>),
  elif_clauses: Vec<(Expr, Vec<Stmt>)>,
  else_clause: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct ForLoop {
  iter_var: String,
  collection: Expr,
  body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
  condition: Expr,
  body: Vec<Stmt>,
}

fn indent<W : fmt::Write>(w : &mut W, ind: u32) -> Result<(), fmt::Error> {
  let spaces = String::from(" ").repeat(ind.try_into().unwrap());
  write!(w, "{}", spaces)
}

impl Stmt {

  pub fn write_gd<W : fmt::Write>(&self, w : &mut W, ind: u32) -> Result<(), fmt::Error> {
    indent(w, ind)?;
    match self {
      Stmt::Expr(expr) => {
        write!(w, "{}\n", expr.to_gd())
      }
      Stmt::IfStmt(IfStmt { if_clause, elif_clauses, else_clause }) => {
        write!(w, "if {}:\n", if_clause.0.to_gd())?;
        Stmt::write_gd_stmts(&if_clause.1, w, ind + 4)?;
        for clause in elif_clauses {
          indent(w, ind)?;
          write!(w, "elif {}:\n", clause.0.to_gd())?;
          Stmt::write_gd_stmts(&clause.1, w, ind + 4)?;
        }
        if let Some(else_clause) = else_clause {
          indent(w, ind)?;
          write!(w, "else:\n")?;
          Stmt::write_gd_stmts(else_clause, w, ind + 4)?;
        }
        Ok(())
      }
      Stmt::PassStmt => write!(w, "pass\n"),
      Stmt::ForLoop(ForLoop { iter_var, collection, body }) => {
        write!(w, "for {} in {}:\n", iter_var, collection.to_gd())?;
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
      Stmt::WhileLoop(WhileLoop { condition, body }) => {
        write!(w, "while {}:\n", condition.to_gd())?;
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
    }
  }

  pub fn write_gd_stmts<'a, W, I>(iter: I, w: &mut W, ind: u32) -> Result<(), fmt::Error>
  where W : fmt::Write,
        I : IntoIterator<Item = &'a Stmt> {
    let mut empty = true;
    for stmt in iter {
      stmt.write_gd(w, ind)?;
      empty = false;
    }
    if empty {
      Stmt::PassStmt.write_gd(w, ind)?;
    }
    Ok(())
  }

  pub fn to_gd(&self, ind: u32) -> String {
    let mut string = String::new();
    self.write_gd(&mut string, ind).expect("Could not write to string in Stmt::to_gd");
    string
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::expr::Expr;
  use crate::gdscript::literal::Literal;

  #[test]
  fn expr_stmt() {
    assert_eq!(Stmt::Expr(Expr::Var(String::from("foobar"))).to_gd(0), "foobar\n");
    assert_eq!(Stmt::Expr(Expr::Literal(Literal::Int(9))).to_gd(0), "9\n");
  }

  #[test]
  fn basic_indent() {
    assert_eq!(Stmt::Expr(Expr::Var(String::from("foobar"))).to_gd(0), "foobar\n");
    assert_eq!(Stmt::Expr(Expr::Var(String::from("foobar"))).to_gd(4), "    foobar\n");
    assert_eq!(Stmt::Expr(Expr::Var(String::from("foobar"))).to_gd(8), "        foobar\n");
  }

  #[test]
  fn if_stmt() {
    let cond1 = Expr::Var(String::from("condition1"));
    let cond2 = Expr::Var(String::from("condition2"));
    let cond3 = Expr::Var(String::from("condition3"));

    let stmt1 = Stmt::Expr(Expr::Literal(Literal::Int(1)));
    let stmt2 = Stmt::Expr(Expr::Literal(Literal::Int(2)));
    let stmt3 = Stmt::Expr(Expr::Literal(Literal::Int(3)));
    let stmt4 = Stmt::Expr(Expr::Literal(Literal::Int(4)));
    let stmt5 = Stmt::Expr(Expr::Literal(Literal::Int(5)));

    let if1 = Stmt::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!()),
      elif_clauses: vec!(),
      else_clause: None
    });
    assert_eq!(if1.to_gd(0), "if condition1:\n    pass\n");

    let if2 = Stmt::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone())),
      elif_clauses: vec!(),
      else_clause: None
    });
    assert_eq!(if2.to_gd(0), "if condition1:\n    1\n");

    let if3 = Stmt::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!(),
      else_clause: None
    });
    assert_eq!(if3.to_gd(0), "if condition1:\n    1\n    2\n");

    let if4 = Stmt::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!(),
      else_clause: Some(vec!())
    });
    assert_eq!(if4.to_gd(0), "if condition1:\n    1\n    2\nelse:\n    pass\n");

    let if5 = Stmt::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!(),
      else_clause: Some(vec!(stmt3.clone()))
    });
    assert_eq!(if5.to_gd(0), "if condition1:\n    1\n    2\nelse:\n    3\n");

    let if6 = Stmt::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!((cond2.clone(), vec!(stmt3.clone()))),
      else_clause: Some(vec!(stmt4.clone()))
    });
    assert_eq!(if6.to_gd(0), "if condition1:\n    1\n    2\nelif condition2:\n    3\nelse:\n    4\n");

    let if7 = Stmt::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!((cond2.clone(), vec!(stmt3.clone())), (cond3.clone(), vec!(stmt4.clone()))),
      else_clause: Some(vec!(stmt5.clone()))
    });
    assert_eq!(if7.to_gd(0), "if condition1:\n    1\n    2\nelif condition2:\n    3\nelif condition3:\n    4\nelse:\n    5\n");

  }

  #[test]
  fn nested_if() {
    let cond1 = Expr::Var(String::from("condition1"));
    let cond2 = Expr::Var(String::from("condition2"));

    let stmt1 = Stmt::Expr(Expr::Literal(Literal::Int(1)));
    let stmt2 = Stmt::Expr(Expr::Literal(Literal::Int(2)));
    let stmt3 = Stmt::Expr(Expr::Literal(Literal::Int(3)));
    let stmt4 = Stmt::Expr(Expr::Literal(Literal::Int(4)));

    let inner = Stmt::IfStmt(IfStmt {
      if_clause: (cond2.clone(), vec!(stmt2.clone(), stmt3.clone())),
      elif_clauses: vec!(),
      else_clause: None,
    });
    let outer = Stmt::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), inner, stmt4.clone())),
      elif_clauses: vec!(),
      else_clause: None,
    });
    assert_eq!(outer.to_gd(0), "if condition1:\n    1\n    if condition2:\n        2\n        3\n    4\n");

  }

  #[test]
  fn for_loop() {
    let expr = Expr::Var(String::from("collection"));
    let stmt = Stmt::Expr(Expr::Literal(Literal::Int(1)));

    let for1 = Stmt::ForLoop(ForLoop {
      iter_var: String::from("i"),
      collection: expr.clone(),
      body: vec!(),
    });
    assert_eq!(for1.to_gd(0), "for i in collection:\n    pass\n");

    let for2 = Stmt::ForLoop(ForLoop {
      iter_var: String::from("i"),
      collection: expr.clone(),
      body: vec!(stmt.clone()),
    });
    assert_eq!(for2.to_gd(0), "for i in collection:\n    1\n");

  }

  #[test]
  fn while_loop() {
    let expr = Expr::Var(String::from("condition"));
    let stmt = Stmt::Expr(Expr::Literal(Literal::Int(1)));

    let while1 = Stmt::WhileLoop(WhileLoop {
      condition: expr.clone(),
      body: vec!(),
    });
    assert_eq!(while1.to_gd(0), "while condition:\n    pass\n");

    let while2 = Stmt::WhileLoop(WhileLoop {
      condition: expr.clone(),
      body: vec!(stmt.clone()),
    });
    assert_eq!(while2.to_gd(0), "while condition:\n    1\n");

  }

}
