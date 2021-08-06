
//! GDScript statements.
//!
//! This module defines a [datatype](Stmt) for representing statements
//! in the GDScript language, as well as [`Stmt::write_gd`] for
//! writing statements as GDScript syntax to a [`fmt::Write`]
//! instance.

use crate::gdscript::expr::Expr;
use crate::gdscript::op::{self, AssignOp, OperatorHasInfo};
use crate::gdscript::pattern::Pattern;
use crate::gdscript::indent;

use std::fmt;

/// The type of GDScript statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
  /// An expression alone can stand as a statement.
  Expr(Expr),
  IfStmt(IfStmt),
  ForLoop(ForLoop),
  WhileLoop(WhileLoop),
  PassStmt,
  BreakStmt,
  ContinueStmt,
  MatchStmt(Expr, Vec<(Pattern, Vec<Stmt>)>),
  /// A variable declaration. Note that while GDScript allows variable
  /// declarations which do *not* assign a value, here we require that
  /// all declarations assign an initial value.
  VarDecl(String, Expr),
  ReturnStmt(Expr),
  /// An assignment. Note that GDScript restricts what can appear on
  /// the left-hand side of an assignment, whereas this type allows
  /// any [`Expr`]. Care must be taken to ensure that the left-hand
  /// side is syntactically valid.
  Assign(Box<Expr>, AssignOp, Box<Expr>),
}

/// The type of if statements, used in [`Stmt::IfStmt`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStmt {
  pub if_clause: (Expr, Vec<Stmt>),
  pub elif_clauses: Vec<(Expr, Vec<Stmt>)>,
  pub else_clause: Option<Vec<Stmt>>,
}

/// The type of for loops, used in [`Stmt::ForLoop`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForLoop {
  pub iter_var: String,
  pub collection: Expr,
  pub body: Vec<Stmt>,
}

/// The type of while loops, used in [`Stmt::WhileLoop`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileLoop {
  pub condition: Expr,
  pub body: Vec<Stmt>,
}

/// Construct an `if` statement with no `elif` or `else` branches.
pub fn if_then(cond: Expr, true_branch: Vec<Stmt>) -> Stmt {
  Stmt::IfStmt(IfStmt {
    if_clause: (cond, true_branch),
    elif_clauses: vec!(),
    else_clause: None,
  })
}

/// Construct an `if` statement with an `else` branch but no `elif`
/// branches.
pub fn if_else(cond: Expr, true_branch: Vec<Stmt>, false_branch: Vec<Stmt>) -> Stmt {
  Stmt::IfStmt(IfStmt {
    if_clause: (cond, true_branch),
    elif_clauses: vec!(),
    else_clause: Some(false_branch),
  })
}

/// General-purpose `if` statement constructor.
///
/// Construct a sequence of statements which represents the branches.
/// The first element of `cases` will be the `if_clause` of the
/// statement, all remaining `cases` will be `elif_clauses`, and the
/// `default` shall be `else_clause`.
///
/// As a special corner case, if `cases` is empty, then `default` is
/// returned unmodified. This is the only case in which more than one
/// value might be returned. If `cases` is nonempty, then
/// `if_branches` will always return a vector containing a single
/// statement.
pub fn if_branches(cases: Vec<(Expr, Vec<Stmt>)>, default: Vec<Stmt>) -> Vec<Stmt> {
  if cases.is_empty() {
    default
  } else {
    let if_clause = cases[0].clone();
    let elif_clauses = cases[1..].to_vec();
    vec!(
      Stmt::IfStmt(IfStmt {
        if_clause,
        elif_clauses,
        else_clause: Some(default),
      })
    )
  }
}

impl Stmt {

  /// Simple assignment to a given target.
  pub fn simple_assign(lhs: Expr, rhs: Expr) -> Stmt {
    Stmt::Assign(Box::new(lhs), AssignOp::Eq, Box::new(rhs))
  }

  /// Write the statement, as GDScript code, to the [`fmt::Write`]
  /// instance `w`.
  ///
  /// We are assumed to be at the indentation level `ind`, so that all
  /// lines in the result will be indented to that level.
  ///
  /// The writer `w` should currently be either empty or immediately
  /// after a newline. The statement will always end by printing a
  /// newline, making it suitable for writing a subsequent statement
  /// immediately after.
  pub fn write_gd<W : fmt::Write>(&self, w: &mut W, ind: u32) -> Result<(), fmt::Error> {
    indent(w, ind)?;
    match self {
      Stmt::Expr(expr) => {
        writeln!(w, "{}", expr.to_gd())
      }
      Stmt::IfStmt(IfStmt { if_clause, elif_clauses, else_clause }) => {
        writeln!(w, "if {}:", if_clause.0.to_gd())?;
        Stmt::write_gd_stmts(&if_clause.1, w, ind + 4)?;
        for clause in elif_clauses {
          indent(w, ind)?;
          writeln!(w, "elif {}:", clause.0.to_gd())?;
          Stmt::write_gd_stmts(&clause.1, w, ind + 4)?;
        }
        if let Some(else_clause) = else_clause {
          indent(w, ind)?;
          writeln!(w, "else:")?;
          Stmt::write_gd_stmts(else_clause, w, ind + 4)?;
        }
        Ok(())
      }
      Stmt::PassStmt => writeln!(w, "pass"),
      Stmt::BreakStmt => writeln!(w, "break"),
      Stmt::ContinueStmt => writeln!(w, "continue"),
      Stmt::ForLoop(ForLoop { iter_var, collection, body }) => {
        writeln!(w, "for {} in {}:", iter_var, collection.to_gd())?;
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
      Stmt::WhileLoop(WhileLoop { condition, body }) => {
        writeln!(w, "while {}:", condition.to_gd())?;
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
      Stmt::MatchStmt(expr, clauses) => {
        writeln!(w, "match {}:", expr.to_gd())?;
        if clauses.is_empty() {
          // If you try to have an empty match body, you kinda deserve
          // the program to crash. But hey, I'm in a good mood, so
          // I'll handle the wonky corner case. :)
          indent(w, ind + 4)?;
          writeln!(w, "{}:", Pattern::Wildcard.to_gd())?;
          Stmt::write_gd_stmts(vec!(), w, ind + 8)
        } else {
          for (ptn, body) in clauses {
            indent(w, ind + 4)?;
            writeln!(w, "{}:", ptn.to_gd())?;
            Stmt::write_gd_stmts(body, w, ind + 8)?;
          }
          Ok(())
        }
      }
      Stmt::VarDecl(name, expr) => {
        writeln!(w, "var {} = {}", name, expr.to_gd())
      }
      Stmt::ReturnStmt(expr) => {
        writeln!(w, "return {}", expr.to_gd())
      }
      Stmt::Assign(lhs, op, rhs) => {
        let info = op.op_info();
        let lhs = lhs.to_gd();
        let rhs = rhs.to_gd();
        if info.padding == op::Padding::NotRequired {
          writeln!(w, "{}{}{}", lhs, info.name, rhs)
        } else {
          writeln!(w, "{} {} {}", lhs, info.name, rhs)
        }
      },
    }
  }

  /// Write several statements in sequence, using [`Stmt::write_gd`].
  ///
  /// If `iter` is empty, then `"pass\n"` will be written.
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

  /// Write the statement to a string, using [`Stmt::write_gd`].
  ///
  /// # Panics
  ///
  /// This function panics if there is a write error to the string. If
  /// you wish to handle that case yourself, use [`Stmt::write_gd`]
  /// explicitly.
  pub fn to_gd(&self, ind: u32) -> String {
    let mut string = String::new();
    self.write_gd(&mut string, ind).expect("Could not write to string in Stmt::to_gd");
    string
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::expr::{Expr, ExprF};
  use crate::gdscript::literal::Literal;
  use crate::pipeline::source::SourceOffset;

  fn assign(a: &Expr, op: AssignOp, b: &Expr) -> Stmt {
    Stmt::Assign(Box::new(a.clone()), op, Box::new(b.clone()))
  }

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  #[test]
  fn expr_stmt() {
    assert_eq!(Stmt::Expr(e(ExprF::Var(String::from("foobar")))).to_gd(0), "foobar\n");
    assert_eq!(Stmt::Expr(e(ExprF::from(9))).to_gd(0), "9\n");
  }

  #[test]
  fn basic_indent() {
    assert_eq!(Stmt::Expr(e(ExprF::Var(String::from("foobar")))).to_gd(0), "foobar\n");
    assert_eq!(Stmt::Expr(e(ExprF::Var(String::from("foobar")))).to_gd(4), "    foobar\n");
    assert_eq!(Stmt::Expr(e(ExprF::Var(String::from("foobar")))).to_gd(8), "        foobar\n");
  }

  #[test]
  fn simple_stmts() {
    let expr = e(ExprF::from(1000));
    assert_eq!(Stmt::VarDecl(String::from("var_name"), expr.clone()).to_gd(0), "var var_name = 1000\n");
    assert_eq!(Stmt::ReturnStmt(expr.clone()).to_gd(0), "return 1000\n");
  }

  #[test]
  fn if_stmt() {
    let cond1 = e(ExprF::Var(String::from("condition1")));
    let cond2 = e(ExprF::Var(String::from("condition2")));
    let cond3 = e(ExprF::Var(String::from("condition3")));

    let stmt1 = Stmt::Expr(e(ExprF::from(1)));
    let stmt2 = Stmt::Expr(e(ExprF::from(2)));
    let stmt3 = Stmt::Expr(e(ExprF::from(3)));
    let stmt4 = Stmt::Expr(e(ExprF::from(4)));
    let stmt5 = Stmt::Expr(e(ExprF::from(5)));

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
    let cond1 = e(ExprF::Var(String::from("condition1")));
    let cond2 = e(ExprF::Var(String::from("condition2")));

    let stmt1 = Stmt::Expr(e(ExprF::from(1)));
    let stmt2 = Stmt::Expr(e(ExprF::from(2)));
    let stmt3 = Stmt::Expr(e(ExprF::from(3)));
    let stmt4 = Stmt::Expr(e(ExprF::from(4)));

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
    let expr = e(ExprF::Var(String::from("collection")));
    let stmt = Stmt::Expr(e(ExprF::from(1)));

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
    let expr = e(ExprF::Var(String::from("condition")));
    let stmt = Stmt::Expr(e(ExprF::from(1)));

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
    let expr = e(ExprF::Var(String::from("expr")));

    let ptn1 = Pattern::Literal(Literal::Int(100));
    let ptn2 = Pattern::Literal(Literal::Int(200));

    let body1 = Stmt::Expr(e(ExprF::Var(String::from("body1"))));
    let body2 = Stmt::Expr(e(ExprF::Var(String::from("body2"))));

    let match1 = Stmt::MatchStmt(expr.clone(), vec!());
    assert_eq!(match1.to_gd(0), "match expr:\n    _:\n        pass\n");

    let match2 = Stmt::MatchStmt(expr.clone(), vec!((ptn1.clone(), vec!(body1.clone()))));
    assert_eq!(match2.to_gd(0), "match expr:\n    100:\n        body1\n");

    let match3 = Stmt::MatchStmt(expr.clone(), vec!((ptn1.clone(), vec!(body1.clone())),
                                                    (ptn2.clone(), vec!(body2.clone()))));
    assert_eq!(match3.to_gd(0), "match expr:\n    100:\n        body1\n    200:\n        body2\n");

  }

  #[test]
  fn assign_ops() {
    let a = e(ExprF::Var(String::from("a")));
    let b = e(ExprF::Var(String::from("b")));
    assert_eq!(assign(&a, AssignOp::Eq, &b).to_gd(0), "a = b\n");
    assert_eq!(assign(&a, AssignOp::Add, &b).to_gd(0), "a += b\n");
    assert_eq!(assign(&a, AssignOp::Sub, &b).to_gd(0), "a -= b\n");
    assert_eq!(assign(&a, AssignOp::Times, &b).to_gd(0), "a *= b\n");
    assert_eq!(assign(&a, AssignOp::Div, &b).to_gd(0), "a /= b\n");
    assert_eq!(assign(&a, AssignOp::Mod, &b).to_gd(0), "a %= b\n");
    assert_eq!(assign(&a, AssignOp::BitAnd, &b).to_gd(0), "a &= b\n");
    assert_eq!(assign(&a, AssignOp::BitOr, &b).to_gd(0), "a |= b\n");
  }

}
