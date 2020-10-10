
use super::literal;
use super::arglist::ArgList;
//use crate::gdscript::op::{self, UnaryOp, BinaryOp, OperatorHasInfo};

use std::collections::HashSet;
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
  LocalVar(String),
  Literal(literal::Literal),
  Progn(Vec<Expr>),
  IfStmt(Box<Expr>, Box<Expr>, Box<Expr>),
  CondStmt(Vec<(Expr, Option<Expr>)>),
  Call(String, Vec<Expr>),
  Let(Vec<(String, Expr)>, Box<Expr>),
  Lambda(ArgList, Box<Expr>),
  FuncRef(FuncRefTarget),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncRefTarget {
  SimpleName(String),
}

impl Expr {

  pub fn if_stmt(cond: Expr, t: Expr, f: Expr) -> Expr {
    Expr::IfStmt(Box::new(cond), Box::new(t), Box::new(f))
  }

  // TODO We'll need a walk_locals for function names too. (FuncRef will respond to it)
  fn walk_locals(&self, acc: &mut HashSet<String>) {
    match self {
      Expr::LocalVar(s) => {
        acc.insert(s.to_owned());
      }
      Expr::Literal(_) => {}
//      Expr::Subscript(a, b) => {
//        a.walk_locals(acc);
//        b.walk_locals(acc);
//      }
      Expr::Progn(exprs) => {
        for expr in exprs {
          expr.walk_locals(acc);
        }
      }
      Expr::IfStmt(c, t, f) => {
        c.walk_locals(acc);
        t.walk_locals(acc);
        f.walk_locals(acc);
      }
      Expr::CondStmt(clauses) => {
        for clause in clauses {
          clause.0.walk_locals(acc);
          if let Some(body) = &clause.1 {
            body.walk_locals(acc);
          }
        }
      }
      Expr::Call(_, args) => {
        for expr in args {
          expr.walk_locals(acc);
        }
      }
      Expr::Let(clauses, body) => {
        let mut vars = HashSet::new();
        for clause in clauses {
          vars.insert(clause.0.to_owned());
          clause.1.walk_locals(acc);
        }
        let mut local_scope = HashSet::new();
        body.walk_locals(&mut local_scope);
        for var in local_scope.difference(&vars) {
          acc.insert(var.to_owned());
        }
      }
      Expr::Lambda(args, body) => {
        let vars = HashSet::from_iter(args.iter_vars().map(|x| x.to_owned()));
        let mut local_scope = HashSet::new();
        body.walk_locals(&mut local_scope);
        for var in local_scope.difference(&vars) {
          acc.insert(var.to_owned());
        }
      }
      Expr::FuncRef(_) => {}
    };
  }

  // Returns all of the variable names which appear unbound in the
  // current scope. Crucially, this excludes names which are bound to
  // lambda arguments or let instantiations.
  pub fn get_locals(&self) -> HashSet<String> {
    let mut result = HashSet::new();
    self.walk_locals(&mut result);
    result
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use literal::Literal;
  use std::hash::Hash;
  use std::cmp::Eq;

  fn hash<T : Hash + Eq>(vec: Vec<T>) -> HashSet<T> {
    vec.into_iter().collect()
  }

  fn nil() -> Expr {
    Expr::Literal(Literal::Nil)
  }

  #[test]
  fn test_locals_simple() {
    assert_eq!(Expr::LocalVar(String::from("foobar")).get_locals(), hash(vec!("foobar".to_owned())));
    assert_eq!(Expr::LocalVar(String::from("aaa")).get_locals(), hash(vec!("aaa".to_owned())));
    assert_eq!(Expr::Literal(Literal::Int(99)).get_locals(), hash(vec!()));
    assert_eq!(Expr::Literal(Literal::Nil).get_locals(), hash(vec!()));
    assert_eq!(Expr::Progn(vec!()).get_locals(), hash(vec!()));
  }

  #[test]
  fn test_locals_compound() {
    let progn = Expr::Progn(vec!(Expr::LocalVar(String::from("aa")),
                                 Expr::LocalVar(String::from("bb"))));
    assert_eq!(progn.get_locals(), hash(vec!("aa".to_owned(), "bb".to_owned())));
  }

  #[test]
  fn test_locals_let() {

    // Declared variable
    let e1 = Expr::Let(vec!(("var".to_owned(), nil())),
                       Box::new(nil()));
    assert_eq!(e1.get_locals(), hash(vec!()));

    // Declared and used variable
    let e2 = Expr::Let(vec!(("var".to_owned(), nil())),
                       Box::new(Expr::LocalVar("var".to_owned())));
    assert_eq!(e2.get_locals(), hash(vec!()));

    // Different variable
    let e3 = Expr::Let(vec!(("var_unused".to_owned(), nil())),
                       Box::new(Expr::LocalVar("var1".to_owned())));
    assert_eq!(e3.get_locals(), hash(vec!("var1".to_owned())));

    // Variable in decl
    let e4 = Expr::Let(vec!(("var_unused".to_owned(), Expr::LocalVar("var".to_owned()))),
                       Box::new(nil()));
    assert_eq!(e4.get_locals(), hash(vec!("var".to_owned())));

    // Variable in decl (soon to be shadowed)
    let e4 = Expr::Let(vec!(("var".to_owned(), Expr::LocalVar("var".to_owned()))),
                       Box::new(nil()));
    assert_eq!(e4.get_locals(), hash(vec!("var".to_owned())));

  }

}
