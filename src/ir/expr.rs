
use super::literal;
use super::arglist::ArgList;
//use crate::gdscript::op::{self, UnaryOp, BinaryOp, OperatorHasInfo};
use super::locals::{Locals, AccessType};
use super::functions::Functions;

use std::collections::HashSet;
use std::collections::hash_map::RandomState;
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
  LocalVar(String),
  Literal(literal::Literal),
  Progn(Vec<Expr>),
  IfStmt(Box<Expr>, Box<Expr>, Box<Expr>),
  CondStmt(Vec<(Expr, Option<Expr>)>),
  WhileStmt(Box<Expr>, Box<Expr>),
  Call(String, Vec<Expr>),
  Let(Vec<(String, Expr)>, Box<Expr>),
  Lambda(ArgList, Box<Expr>),
  FuncRef(FuncRefTarget),
  Assign(String, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncRefTarget {
  SimpleName(String),
}

impl Expr {

  pub fn if_stmt(cond: Expr, t: Expr, f: Expr) -> Expr {
    Expr::IfStmt(Box::new(cond), Box::new(t), Box::new(f))
  }

  pub fn while_stmt(cond: Expr, body: Expr) -> Expr {
    Expr::WhileStmt(Box::new(cond), Box::new(body))
  }

  fn walk_locals(&self, acc_vars: &mut Locals, acc_fns: &mut Functions) {
    match self {
      Expr::LocalVar(s) => {
        acc_vars.visited(s, AccessType::Read);
      }
      Expr::Literal(_) => {}
//      Expr::Subscript(a, b) => {
//        a.walk_locals(acc_vars, acc_fns);
//        b.walk_locals(acc_vars, acc_fns);
//      }
      Expr::Progn(exprs) => {
        for expr in exprs {
          expr.walk_locals(acc_vars, acc_fns);
        }
      }
      Expr::IfStmt(c, t, f) => {
        c.walk_locals(acc_vars, acc_fns);
        t.walk_locals(acc_vars, acc_fns);
        f.walk_locals(acc_vars, acc_fns);
      }
      Expr::CondStmt(clauses) => {
        for clause in clauses {
          clause.0.walk_locals(acc_vars, acc_fns);
          if let Some(body) = &clause.1 {
            body.walk_locals(acc_vars, acc_fns);
          }
        }
      }
      Expr::WhileStmt(cond, body) => {
        cond.walk_locals(acc_vars, acc_fns);
        body.walk_locals(acc_vars, acc_fns);
      }
      Expr::Call(name, args) => {
        acc_fns.visited(name);
        for expr in args {
          expr.walk_locals(acc_vars, acc_fns);
        }
      }
      Expr::Let(clauses, body) => {
        let mut vars = HashSet::new();
        for clause in clauses {
          vars.insert(clause.0.to_owned());
          clause.1.walk_locals(acc_vars, acc_fns);
        }
        let mut local_scope = Locals::new();
        body.walk_locals(&mut local_scope, acc_fns);
        for var in local_scope.names() {
          if !vars.contains(var) {
            acc_vars.visited(var, local_scope.get(var));
          }
        }
      }
      Expr::Lambda(args, body) => {
        let vars: HashSet<_, RandomState> = HashSet::from_iter(args.iter_vars().map(|x| x.to_owned()));
        let mut local_scope = Locals::new();
        body.walk_locals(&mut local_scope, acc_fns);
        for var in local_scope.names() {
          if !vars.contains(var) {
            acc_vars.visited(var, local_scope.get(var).closed());
          }
        }
      }
      Expr::Assign(s, expr) => {
        acc_vars.visited(s, AccessType::RW);
        expr.walk_locals(acc_vars, acc_fns);
      }
      Expr::FuncRef(target) => {
        match target {
          FuncRefTarget::SimpleName(name) => acc_fns.visited(name),
        }
      }
    };
  }

  // Returns all of the variable names which appear unbound in the
  // current scope. Crucially, this excludes names which are bound to
  // lambda arguments or let instantiations.
  pub fn get_locals(&self) -> Locals {
    self.get_names().0
  }

  // Returns all of the function names which appear unbound in the
  // current scope.
  pub fn get_functions(&self) -> Functions {
    self.get_names().1
  }

  pub fn get_names(&self) -> (Locals, Functions) {
    let mut vars = Locals::new();
    let mut fns = Functions::new();
    self.walk_locals(&mut vars, &mut fns);
    (vars, fns)
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use literal::Literal;

  fn lhash(vec: Vec<String>) -> Locals {
    Locals::from_hashmap(vec.into_iter().map(|x| (x, AccessType::Read)).collect())
  }

  fn lhash_rw(vec: Vec<(String, AccessType)>) -> Locals {
    Locals::from_hashmap(vec.into_iter().collect())
  }

  fn fhash(vec: Vec<String>) -> Functions {
    Functions::from_hashset(vec.into_iter().collect())
  }

  fn nil() -> Expr {
    Expr::Literal(Literal::Nil)
  }

  #[test]
  fn test_locals_simple() {
    assert_eq!(Expr::LocalVar(String::from("foobar")).get_locals(), lhash(vec!("foobar".to_owned())));
    assert_eq!(Expr::LocalVar(String::from("aaa")).get_locals(), lhash(vec!("aaa".to_owned())));
    assert_eq!(Expr::Literal(Literal::Int(99)).get_locals(), lhash(vec!()));
    assert_eq!(Expr::Literal(Literal::Nil).get_locals(), lhash(vec!()));
    assert_eq!(Expr::Progn(vec!()).get_locals(), lhash(vec!()));
  }

  #[test]
  fn test_locals_compound() {
    let progn = Expr::Progn(vec!(Expr::LocalVar(String::from("aa")),
                                 Expr::LocalVar(String::from("bb"))));
    assert_eq!(progn.get_locals(), lhash(vec!("aa".to_owned(), "bb".to_owned())));
  }

  #[test]
  fn test_locals_let() {

    // Declared variable
    let e1 = Expr::Let(vec!(("var".to_owned(), nil())),
                       Box::new(nil()));
    assert_eq!(e1.get_locals(), lhash(vec!()));

    // Declared and used variable
    let e2 = Expr::Let(vec!(("var".to_owned(), nil())),
                       Box::new(Expr::LocalVar("var".to_owned())));
    assert_eq!(e2.get_locals(), lhash(vec!()));

    // Different variable
    let e3 = Expr::Let(vec!(("var_unused".to_owned(), nil())),
                       Box::new(Expr::LocalVar("var1".to_owned())));
    assert_eq!(e3.get_locals(), lhash(vec!("var1".to_owned())));

    // Variable in decl
    let e4 = Expr::Let(vec!(("var_unused".to_owned(), Expr::LocalVar("var".to_owned()))),
                       Box::new(nil()));
    assert_eq!(e4.get_locals(), lhash(vec!("var".to_owned())));

    // Variable in decl (soon to be shadowed)
    let e4 = Expr::Let(vec!(("var".to_owned(), Expr::LocalVar("var".to_owned()))),
                       Box::new(nil()));
    assert_eq!(e4.get_locals(), lhash(vec!("var".to_owned())));

  }

  #[test]
  fn test_locals_assignment() {

    // Simple assignment
    let e1 = Expr::Assign(String::from("var"), Box::new(Expr::Literal(Literal::Nil)));
    assert_eq!(e1.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Assignment including RHS
    let e2 = Expr::Assign(String::from("var1"), Box::new(Expr::LocalVar("var2".to_owned())));
    assert_eq!(e2.get_locals(), lhash_rw(vec!(("var1".to_owned(), AccessType::RW), ("var2".to_owned(), AccessType::Read))));

    // Reading and writing (I)
    let e3 = Expr::Progn(vec!(
      Expr::Assign(String::from("var"), Box::new(Expr::Literal(Literal::Nil))),
      Expr::LocalVar("var".to_owned()),
    ));
    assert_eq!(e3.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Reading and writing (II)
    let e4 = Expr::Progn(vec!(
      Expr::LocalVar("var".to_owned()),
      Expr::Assign(String::from("var"), Box::new(Expr::Literal(Literal::Nil))),
    ));
    assert_eq!(e4.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

  }

  #[test]
  fn test_functions_trivial() {
    let e1 = Expr::Literal(Literal::Int(1));
    assert_eq!(e1.get_functions(), fhash(vec!()));
  }

  #[test]
  fn test_functions_calls() {
    let e1 = Expr::Call("abc".to_owned(), vec!(Expr::Call("def".to_owned(), vec!())));
    assert_eq!(e1.get_functions(), fhash(vec!("abc".to_owned(), "def".to_owned())));
  }

  #[test]
  fn test_functions_ref() {
    let e1 = Expr::FuncRef(FuncRefTarget::SimpleName("abc".to_owned()));
    assert_eq!(e1.get_functions(), fhash(vec!("abc".to_owned())));
  }

}
