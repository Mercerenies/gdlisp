// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

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
use crate::pipeline::source::{SourceOffset, Sourced};

use std::fmt;

/// The type of GDScript statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtF {
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

/// GDScript statement with its source offset. See [`Sourced`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
  pub value: StmtF,
  pub pos: SourceOffset,
}

/// The type of if statements, used in [`StmtF::IfStmt`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStmt {
  pub if_clause: (Expr, Vec<Stmt>),
  pub elif_clauses: Vec<(Expr, Vec<Stmt>)>,
  pub else_clause: Option<Vec<Stmt>>,
}

/// The type of for loops, used in [`StmtF::ForLoop`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForLoop {
  pub iter_var: String,
  pub collection: Expr,
  pub body: Vec<Stmt>,
}

/// The type of while loops, used in [`StmtF::WhileLoop`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileLoop {
  pub condition: Expr,
  pub body: Vec<Stmt>,
}

/// Construct an `if` statement with no `elif` or `else` branches.
pub fn if_then(cond: Expr, true_branch: Vec<Stmt>, pos: SourceOffset) -> Stmt {
  Stmt::new(
    StmtF::IfStmt(IfStmt {
      if_clause: (cond, true_branch),
      elif_clauses: vec!(),
      else_clause: None,
    }),
    pos,
  )
}

/// Construct an `if` statement with an `else` branch but no `elif`
/// branches.
pub fn if_else(cond: Expr, true_branch: Vec<Stmt>, false_branch: Vec<Stmt>, pos: SourceOffset) -> Stmt {
  Stmt::new(
    StmtF::IfStmt(IfStmt {
      if_clause: (cond, true_branch),
      elif_clauses: vec!(),
      else_clause: Some(false_branch),
    }),
    pos,
  )
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
pub fn if_branches(cases: Vec<(Expr, Vec<Stmt>)>, default: Vec<Stmt>, pos: SourceOffset) -> Vec<Stmt> {
  if cases.is_empty() {
    default
  } else {
    let if_clause = cases[0].clone();
    let elif_clauses = cases[1..].to_vec();
    vec!(
      Stmt::new(
        StmtF::IfStmt(IfStmt {
          if_clause,
          elif_clauses,
          else_clause: Some(default),
        }),
        pos,
      )
    )
  }
}

impl Stmt {

  /// A new `Stmt` at the given position in the source.
  pub fn new(value: StmtF, pos: SourceOffset) -> Stmt {
    Stmt { value, pos }
  }

  /// An expression, as a statement.
  pub fn expr(expr: Expr) -> Stmt {
    let pos = expr.pos;
    Stmt::new(StmtF::Expr(expr), pos)
  }

  /// A return statement.
  pub fn return_stmt(expr: Expr, pos: SourceOffset) -> Stmt {
    Stmt::new(StmtF::ReturnStmt(expr), pos)
  }

  /// Simple assignment to a given target.
  pub fn simple_assign(lhs: Expr, rhs: Expr, pos: SourceOffset) -> Stmt {
    Stmt::new(StmtF::Assign(Box::new(lhs), AssignOp::Eq, Box::new(rhs)), pos)
  }

  /// Declaration of a new variable.
  pub fn var_decl(var_name: String, value: Expr, pos: SourceOffset) -> Stmt {
    Stmt::new(StmtF::VarDecl(var_name, value), pos)
  }

  /// A `break` statement.
  pub fn break_stmt(pos: SourceOffset) -> Stmt {
    Stmt::new(StmtF::BreakStmt, pos)
  }

  /// A `continue` statement.
  pub fn continue_stmt(pos: SourceOffset) -> Stmt {
    Stmt::new(StmtF::ContinueStmt, pos)
  }

  /// A `pass` statement.
  pub fn pass_stmt(pos: SourceOffset) -> Stmt {
    Stmt::new(StmtF::PassStmt, pos)
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
    match &self.value {
      StmtF::Expr(expr) => {
        writeln!(w, "{}", expr.to_gd())
      }
      StmtF::IfStmt(IfStmt { if_clause, elif_clauses, else_clause }) => {
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
      StmtF::PassStmt => writeln!(w, "pass"),
      StmtF::BreakStmt => writeln!(w, "break"),
      StmtF::ContinueStmt => writeln!(w, "continue"),
      StmtF::ForLoop(ForLoop { iter_var, collection, body }) => {
        writeln!(w, "for {} in {}:", iter_var, collection.to_gd())?;
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
      StmtF::WhileLoop(WhileLoop { condition, body }) => {
        writeln!(w, "while {}:", condition.to_gd())?;
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
      StmtF::MatchStmt(expr, clauses) => {
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
      StmtF::VarDecl(name, expr) => {
        writeln!(w, "var {} = {}", name, expr.to_gd())
      }
      StmtF::ReturnStmt(expr) => {
        writeln!(w, "return {}", expr.to_gd())
      }
      StmtF::Assign(lhs, op, rhs) => {
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
      Stmt::new(StmtF::PassStmt, SourceOffset::default()).write_gd(w, ind)?;
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

impl Sourced for Stmt {
  type Item = StmtF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &StmtF {
    &self.value
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::expr::{Expr, ExprF};
  use crate::gdscript::literal::Literal;
  use crate::pipeline::source::SourceOffset;

  fn assign(a: &Expr, op: AssignOp, b: &Expr) -> Stmt {
    s(StmtF::Assign(Box::new(a.clone()), op, Box::new(b.clone())))
  }

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  fn s(stmt: StmtF) -> Stmt {
    Stmt::new(stmt, SourceOffset::default())
  }

  #[test]
  fn expr_stmt() {
    assert_eq!(Stmt::expr(e(ExprF::Var(String::from("foobar")))).to_gd(0), "foobar\n");
    assert_eq!(Stmt::expr(e(ExprF::from(9))).to_gd(0), "9\n");
  }

  #[test]
  fn basic_indent() {
    assert_eq!(Stmt::expr(e(ExprF::Var(String::from("foobar")))).to_gd(0), "foobar\n");
    assert_eq!(Stmt::expr(e(ExprF::Var(String::from("foobar")))).to_gd(4), "    foobar\n");
    assert_eq!(Stmt::expr(e(ExprF::Var(String::from("foobar")))).to_gd(8), "        foobar\n");
  }

  #[test]
  fn simple_stmts() {
    let expr = e(ExprF::from(1000));
    assert_eq!(s(StmtF::VarDecl(String::from("var_name"), expr.clone())).to_gd(0), "var var_name = 1000\n");
    assert_eq!(s(StmtF::ReturnStmt(expr.clone())).to_gd(0), "return 1000\n");
  }

  #[test]
  fn if_stmt() {
    let cond1 = e(ExprF::Var(String::from("condition1")));
    let cond2 = e(ExprF::Var(String::from("condition2")));
    let cond3 = e(ExprF::Var(String::from("condition3")));

    let stmt1 = Stmt::expr(e(ExprF::from(1)));
    let stmt2 = Stmt::expr(e(ExprF::from(2)));
    let stmt3 = Stmt::expr(e(ExprF::from(3)));
    let stmt4 = Stmt::expr(e(ExprF::from(4)));
    let stmt5 = Stmt::expr(e(ExprF::from(5)));

    let if1 = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!()),
      elif_clauses: vec!(),
      else_clause: None
    }));
    assert_eq!(if1.to_gd(0), "if condition1:\n    pass\n");

    let if2 = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone())),
      elif_clauses: vec!(),
      else_clause: None
    }));
    assert_eq!(if2.to_gd(0), "if condition1:\n    1\n");

    let if3 = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!(),
      else_clause: None
    }));
    assert_eq!(if3.to_gd(0), "if condition1:\n    1\n    2\n");

    let if4 = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!(),
      else_clause: Some(vec!())
    }));
    assert_eq!(if4.to_gd(0), "if condition1:\n    1\n    2\nelse:\n    pass\n");

    let if5 = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!(),
      else_clause: Some(vec!(stmt3.clone()))
    }));
    assert_eq!(if5.to_gd(0), "if condition1:\n    1\n    2\nelse:\n    3\n");

    let if6 = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!((cond2.clone(), vec!(stmt3.clone()))),
      else_clause: Some(vec!(stmt4.clone()))
    }));
    assert_eq!(if6.to_gd(0), "if condition1:\n    1\n    2\nelif condition2:\n    3\nelse:\n    4\n");

    let if7 = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), stmt2.clone())),
      elif_clauses: vec!((cond2.clone(), vec!(stmt3.clone())), (cond3.clone(), vec!(stmt4.clone()))),
      else_clause: Some(vec!(stmt5.clone()))
    }));
    assert_eq!(if7.to_gd(0), "if condition1:\n    1\n    2\nelif condition2:\n    3\nelif condition3:\n    4\nelse:\n    5\n");

  }

  #[test]
  fn nested_if() {
    let cond1 = e(ExprF::Var(String::from("condition1")));
    let cond2 = e(ExprF::Var(String::from("condition2")));

    let stmt1 = Stmt::expr(e(ExprF::from(1)));
    let stmt2 = Stmt::expr(e(ExprF::from(2)));
    let stmt3 = Stmt::expr(e(ExprF::from(3)));
    let stmt4 = Stmt::expr(e(ExprF::from(4)));

    let inner = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond2.clone(), vec!(stmt2.clone(), stmt3.clone())),
      elif_clauses: vec!(),
      else_clause: None,
    }));
    let outer = s(StmtF::IfStmt(IfStmt {
      if_clause: (cond1.clone(), vec!(stmt1.clone(), inner, stmt4.clone())),
      elif_clauses: vec!(),
      else_clause: None,
    }));
    assert_eq!(outer.to_gd(0), "if condition1:\n    1\n    if condition2:\n        2\n        3\n    4\n");

  }

  #[test]
  fn for_loop() {
    let expr = e(ExprF::Var(String::from("collection")));
    let stmt = Stmt::expr(e(ExprF::from(1)));

    let for1 = s(StmtF::ForLoop(ForLoop {
      iter_var: String::from("i"),
      collection: expr.clone(),
      body: vec!(),
    }));
    assert_eq!(for1.to_gd(0), "for i in collection:\n    pass\n");

    let for2 = s(StmtF::ForLoop(ForLoop {
      iter_var: String::from("i"),
      collection: expr.clone(),
      body: vec!(stmt.clone()),
    }));
    assert_eq!(for2.to_gd(0), "for i in collection:\n    1\n");

  }

  #[test]
  fn while_loop() {
    let expr = e(ExprF::Var(String::from("condition")));
    let stmt = Stmt::expr(e(ExprF::from(1)));

    let while1 = s(StmtF::WhileLoop(WhileLoop {
      condition: expr.clone(),
      body: vec!(),
    }));
    assert_eq!(while1.to_gd(0), "while condition:\n    pass\n");

    let while2 = s(StmtF::WhileLoop(WhileLoop {
      condition: expr.clone(),
      body: vec!(stmt.clone()),
    }));
    assert_eq!(while2.to_gd(0), "while condition:\n    1\n");

  }

  #[test]
  fn match_stmt() {
    let expr = e(ExprF::Var(String::from("expr")));

    let ptn1 = Pattern::Literal(Literal::Int(100));
    let ptn2 = Pattern::Literal(Literal::Int(200));

    let body1 = Stmt::expr(e(ExprF::Var(String::from("body1"))));
    let body2 = Stmt::expr(e(ExprF::Var(String::from("body2"))));

    let match1 = s(StmtF::MatchStmt(expr.clone(), vec!()));
    assert_eq!(match1.to_gd(0), "match expr:\n    _:\n        pass\n");

    let match2 = s(StmtF::MatchStmt(expr.clone(), vec!((ptn1.clone(), vec!(body1.clone())))));
    assert_eq!(match2.to_gd(0), "match expr:\n    100:\n        body1\n");

    let match3 = s(StmtF::MatchStmt(expr.clone(), vec!((ptn1.clone(), vec!(body1.clone())),
                                                      (ptn2.clone(), vec!(body2.clone())))));
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
