
use crate::gdscript::expr::Expr;
use crate::gdscript::pattern::Pattern;
use crate::gdscript::indent;

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
  Expr(Expr),
  IfStmt(IfStmt),
  ForLoop(ForLoop),
  WhileLoop(WhileLoop),
  PassStmt,
  MatchStmt(Expr, Vec<(Pattern, Vec<Stmt>)>),
  VarDecl(String, Expr),
  ReturnStmt(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStmt {
  pub if_clause: (Expr, Vec<Stmt>),
  pub elif_clauses: Vec<(Expr, Vec<Stmt>)>,
  pub else_clause: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForLoop {
  pub iter_var: String,
  pub collection: Expr,
  pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileLoop {
  pub condition: Expr,
  pub body: Vec<Stmt>,
}

impl Stmt {

  pub fn write_gd<W : fmt::Write>(&self, w: &mut W, ind: u32) -> Result<(), fmt::Error> {
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
      Stmt::MatchStmt(expr, clauses) => {
        write!(w, "match {}:\n", expr.to_gd())?;
        if clauses.is_empty() {
          // If you try to have an empty match body, you kinda deserve
          // the program to crash. But hey, I'm in a good mood, so
          // I'll handle the wonky corner case. :)
          indent(w, ind + 4)?;
          write!(w, "{}:\n", Pattern::Wildcard.to_gd())?;
          Stmt::write_gd_stmts(vec!(), w, ind + 8)
        } else {
          for (ptn, body) in clauses {
            indent(w, ind + 4)?;
            write!(w, "{}:\n", ptn.to_gd())?;
            Stmt::write_gd_stmts(body, w, ind + 8)?;
          }
          Ok(())
        }
      }
      Stmt::VarDecl(name, expr) => {
        write!(w, "var {} = {}\n", name, expr.to_gd())
      }
      Stmt::ReturnStmt(expr) => {
        write!(w, "return {}\n", expr.to_gd())
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
  fn simple_stmts() {
    let expr = Expr::Literal(Literal::Int(1000));
    assert_eq!(Stmt::VarDecl(String::from("var_name"), expr.clone()).to_gd(0), "var var_name = 1000\n");
    assert_eq!(Stmt::ReturnStmt(expr.clone()).to_gd(0), "return 1000\n");
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

  #[test]
  fn match_stmt() {
    let expr = Expr::Var(String::from("expr"));

    let ptn1 = Pattern::Literal(Literal::Int(100));
    let ptn2 = Pattern::Literal(Literal::Int(200));

    let body1 = Stmt::Expr(Expr::Var(String::from("body1")));
    let body2 = Stmt::Expr(Expr::Var(String::from("body2")));

    let match1 = Stmt::MatchStmt(expr.clone(), vec!());
    assert_eq!(match1.to_gd(0), "match expr:\n    _:\n        pass\n");

    let match2 = Stmt::MatchStmt(expr.clone(), vec!((ptn1.clone(), vec!(body1.clone()))));
    assert_eq!(match2.to_gd(0), "match expr:\n    100:\n        body1\n");

    let match3 = Stmt::MatchStmt(expr.clone(), vec!((ptn1.clone(), vec!(body1.clone())),
                                                    (ptn2.clone(), vec!(body2.clone()))));
    assert_eq!(match3.to_gd(0), "match expr:\n    100:\n        body1\n    200:\n        body2\n");

  }

}
