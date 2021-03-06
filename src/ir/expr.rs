
use super::literal;
use super::decl;
use super::arglist::ArgList;
//use crate::gdscript::op::{self, UnaryOp, BinaryOp, OperatorHasInfo};
use super::locals::{Locals, AccessType};
use super::functions::Functions;
use super::identifier::{Namespace, Id};
use crate::sxp::ast::AST;

use std::collections::HashSet;
use std::collections::hash_map::RandomState;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
  LocalVar(String),
  Literal(literal::Literal),
  Progn(Vec<Expr>),
  CondStmt(Vec<(Expr, Option<Expr>)>),
  WhileStmt(Box<Expr>, Box<Expr>),
  ForStmt(String, Box<Expr>, Box<Expr>),
  Call(String, Vec<Expr>),
  Let(Vec<(String, Expr)>, Box<Expr>),
  FLet(Vec<(String, ArgList, Expr)>, Box<Expr>),
  Labels(Vec<(String, ArgList, Expr)>, Box<Expr>),
  Lambda(ArgList, Box<Expr>),
  FuncRef(FuncRefTarget),
  Assign(AssignTarget, Box<Expr>),
  Array(Vec<Expr>),
  Dictionary(Vec<(Expr, Expr)>),
  Quote(AST),
  FieldAccess(Box<Expr>, String),
  MethodCall(Box<Expr>, String, Vec<Expr>),
  Vector2(Box<Expr>, Box<Expr>),
  Vector3(Box<Expr>, Box<Expr>, Box<Expr>),
  LambdaClass(Box<LambdaClass>),
  Yield(Option<(Box<Expr>, Box<Expr>)>),
  Return(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignTarget {
  Variable(String),
  InstanceField(Box<Expr>, String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncRefTarget {
  SimpleName(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LambdaClass {
  pub extends: String,
  pub args: Vec<Expr>,
  pub constructor: decl::ConstructorDecl,
  pub decls: Vec<decl::ClassInnerDecl>,
}

impl Expr {

  pub fn while_stmt(cond: Expr, body: Expr) -> Expr {
    Expr::WhileStmt(Box::new(cond), Box::new(body))
  }

  pub fn for_stmt(name: String, iter: Expr, body: Expr) -> Expr {
    Expr::ForStmt(name, Box::new(iter), Box::new(body))
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
      Expr::ForStmt(var, iter, body) => {
        let mut local_vars = Locals::new();
        iter.walk_locals(acc_vars, acc_fns);
        body.walk_locals(&mut local_vars, acc_fns);
        local_vars.remove(&var);
        acc_vars.merge_with(local_vars);
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
      Expr::FLet(clauses, body) => {
        let mut fns = HashSet::new();
        for clause in clauses {
          let (name, args, fbody) = clause;
          fns.insert(name.to_owned());
          Expr::Lambda(args.to_owned(), Box::new(fbody.to_owned())).walk_locals(acc_vars, acc_fns);
        }
        let mut local_scope = Functions::new();
        body.walk_locals(acc_vars, &mut local_scope);
        for func in local_scope.names() {
          if !fns.contains(func) {
            acc_fns.visited(func);
          }
        }
      }
      Expr::Labels(clauses, body) => {
        let mut fns = HashSet::new();
        for clause in clauses {
          let (name, _, _) = clause;
          fns.insert(name.to_owned());
        }
        // Note that we consider the bodies of the local functions to
        // be part of the local scope. This is in contrast to the
        // (simpler) FLet case, where only the body of the FLet itself
        // is localized.
        let mut local_scope = Functions::new();
        for clause in clauses {
          let (_, args, fbody) = clause;
          Expr::Lambda(args.to_owned(), Box::new(fbody.to_owned())).walk_locals(acc_vars, &mut local_scope);
        }
        body.walk_locals(acc_vars, &mut local_scope);
        for func in local_scope.names() {
          if !fns.contains(func) {
            acc_fns.visited(func);
          }
        }
      }
      Expr::Lambda(args, body) => {
        let vars: HashSet<_, RandomState> = args.iter_vars().map(|x| x.to_owned()).collect();
        let mut local_scope = Locals::new();
        body.walk_locals(&mut local_scope, acc_fns);
        for var in local_scope.names() {
          if !vars.contains(var) {
            acc_vars.visited(var, local_scope.get(var).closed());
          }
        }
      }
      Expr::Assign(target, expr) => {
        match target {
          AssignTarget::Variable(s) => {
            acc_vars.visited(s, AccessType::RW);
          }
          AssignTarget::InstanceField(lhs, _) => {
            lhs.walk_locals(acc_vars, acc_fns);
          }
        }
        expr.walk_locals(acc_vars, acc_fns);
      }
      Expr::FuncRef(target) => {
        match target {
          FuncRefTarget::SimpleName(name) => acc_fns.visited(name),
        }
      }
      Expr::Array(vec) => {
        for x in vec {
          x.walk_locals(acc_vars, acc_fns);
        }
      }
      Expr::Dictionary(vec) => {
        for (k, v) in vec {
          k.walk_locals(acc_vars, acc_fns);
          v.walk_locals(acc_vars, acc_fns);
        }
      }
      Expr::Quote(_) => {}
      Expr::FieldAccess(lhs, _) => {
        lhs.walk_locals(acc_vars, acc_fns);
      }
      Expr::MethodCall(lhs, _, args) => {
        lhs.walk_locals(acc_vars, acc_fns);
        for expr in args {
          expr.walk_locals(acc_vars, acc_fns);
        }
      }
      Expr::Vector2(x, y) => {
        x.walk_locals(acc_vars, acc_fns);
        y.walk_locals(acc_vars, acc_fns);
      }
      Expr::Vector3(x, y, z) => {
        x.walk_locals(acc_vars, acc_fns);
        y.walk_locals(acc_vars, acc_fns);
        z.walk_locals(acc_vars, acc_fns);
      }
      Expr::LambdaClass(cls) => {
        let LambdaClass { extends, args, constructor, decls } = &**cls;
        for arg in args {
          arg.walk_locals(acc_vars, acc_fns);
        }
        acc_vars.visited(extends, AccessType::ClosedRead);
        let (con_vars, con_fns) = constructor.get_names();
        acc_vars.merge_with(con_vars.closed());
        acc_fns.merge_with(con_fns);
        for decl in decls {
          let (decl_vars, decl_fns) = decl.get_names();
          acc_vars.merge_with(decl_vars.closed());
          acc_fns.merge_with(decl_fns);
        }
      }
      Expr::Yield(arg) => {
        if let Some((x, y)) = arg {
          x.walk_locals(acc_vars, acc_fns);
          y.walk_locals(acc_vars, acc_fns);
        }
      }
      Expr::Return(expr) => {
        expr.walk_locals(acc_vars, acc_fns);
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

  pub fn get_ids(&self) -> impl Iterator<Item=Id> {
    let (vars, fns) = self.get_names();
    let vars = vars.into_names().map(|x| Id::new(Namespace::Value, x));
    let fns = fns.into_names().map(|x| Id::new(Namespace::Function, x));
    Iterator::chain(vars, fns)
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
    let e1 = Expr::Assign(AssignTarget::Variable(String::from("var")), Box::new(Expr::Literal(Literal::Nil)));
    assert_eq!(e1.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Assignment including RHS
    let e2 = Expr::Assign(AssignTarget::Variable(String::from("var1")), Box::new(Expr::LocalVar("var2".to_owned())));
    assert_eq!(e2.get_locals(), lhash_rw(vec!(("var1".to_owned(), AccessType::RW), ("var2".to_owned(), AccessType::Read))));

    // Reading and writing (I)
    let e3 = Expr::Progn(vec!(
      Expr::Assign(AssignTarget::Variable(String::from("var")), Box::new(Expr::Literal(Literal::Nil))),
      Expr::LocalVar("var".to_owned()),
    ));
    assert_eq!(e3.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Reading and writing (II)
    let e4 = Expr::Progn(vec!(
      Expr::LocalVar("var".to_owned()),
      Expr::Assign(AssignTarget::Variable(String::from("var")), Box::new(Expr::Literal(Literal::Nil))),
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
