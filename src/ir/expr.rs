
use super::literal;
use super::decl;
use super::arglist::ArgList;
use super::closure_names::ClosureNames;
use super::access_type::AccessType;
use super::identifier::{Namespace, Id};
use crate::sxp::ast::AST;
use crate::pipeline::source::{SourceOffset, Sourced};

use std::collections::HashSet;
use std::collections::hash_map::RandomState;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprF {
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
  LambdaClass(Box<LambdaClass>),
  Yield(Option<(Box<Expr>, Box<Expr>)>),
  Return(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
  pub value: ExprF,
  pub pos: SourceOffset,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignTarget {
  Variable(SourceOffset, String),
  InstanceField(SourceOffset, Box<Expr>, String),
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

/// A collection of local variables, as well as the broadest
/// [`AccessType`] the variables need.
pub type Locals = ClosureNames<AccessType>;

/// A collection of functions, either local or global.
///
/// Unlike [`Locals`], `Functions` does not need to keep track of
/// access types, since it is impossible to reassign a function in the
/// function namespace after its been declared. It's possible to
/// shadow functions with local ones, but this doesn't mutate the
/// existing one and a closure around the existing function will still
/// reflect the old value of the name.
pub type Functions = ClosureNames<()>;

impl Expr {

  pub fn new(value: ExprF, pos: SourceOffset) -> Expr {
    Expr { value, pos }
  }

  pub fn while_stmt(cond: Expr, body: Expr, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::WhileStmt(Box::new(cond), Box::new(body)), pos)
  }

  pub fn for_stmt(name: String, iter: Expr, body: Expr, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::ForStmt(name, Box::new(iter), Box::new(body)), pos)
  }

  pub fn literal(literal: literal::Literal, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Literal(literal), pos)
  }

  pub fn progn(body: Vec<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Progn(body), pos)
  }

  pub fn call(name: String, args: Vec<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Call(name, args), pos)
  }

  fn walk_locals(&self, acc_vars: &mut Locals, acc_fns: &mut Functions) {
    match &self.value {
      ExprF::LocalVar(s) => {
        acc_vars.visit(s.to_owned(), AccessType::Read);
      }
      ExprF::Literal(_) => {}
//      ExprF::Subscript(a, b) => {
//        a.walk_locals(acc_vars, acc_fns);
//        b.walk_locals(acc_vars, acc_fns);
//      }
      ExprF::Progn(exprs) => {
        for expr in exprs {
          expr.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::CondStmt(clauses) => {
        for clause in clauses {
          clause.0.walk_locals(acc_vars, acc_fns);
          if let Some(body) = &clause.1 {
            body.walk_locals(acc_vars, acc_fns);
          }
        }
      }
      ExprF::WhileStmt(cond, body) => {
        cond.walk_locals(acc_vars, acc_fns);
        body.walk_locals(acc_vars, acc_fns);
      }
      ExprF::ForStmt(var, iter, body) => {
        let mut local_vars = Locals::new();
        iter.walk_locals(acc_vars, acc_fns);
        body.walk_locals(&mut local_vars, acc_fns);
        local_vars.remove(&var);
        acc_vars.merge_with(local_vars);
      }
      ExprF::Call(name, args) => {
        acc_fns.visit(name.to_owned(), ());
        for expr in args {
          expr.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::Let(clauses, body) => {
        let mut vars = HashSet::new();
        for clause in clauses {
          vars.insert(clause.0.to_owned());
          clause.1.walk_locals(acc_vars, acc_fns);
        }
        let mut local_scope = Locals::new();
        body.walk_locals(&mut local_scope, acc_fns);
        for var in local_scope.names() {
          if !vars.contains(var) {
            if let Some(access_type) = local_scope.get(var) {
              acc_vars.visit(var.to_owned(), *access_type);
            }
          }
        }
      }
      ExprF::FLet(clauses, body) => {
        let mut fns = HashSet::new();
        for clause in clauses {
          let (name, args, fbody) = clause;
          fns.insert(name.to_owned());
          let lambda_body = ExprF::Lambda(args.to_owned(), Box::new(fbody.to_owned()));
          Expr::new(lambda_body, self.pos).walk_locals(acc_vars, acc_fns);
        }
        let mut local_scope = Functions::new();
        body.walk_locals(acc_vars, &mut local_scope);
        for func in local_scope.names() {
          if !fns.contains(func) {
            acc_fns.visit(func.to_owned(), ());
          }
        }
      }
      ExprF::Labels(clauses, body) => {
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
          let lambda_body = ExprF::Lambda(args.to_owned(), Box::new(fbody.to_owned()));
          Expr::new(lambda_body, self.pos).walk_locals(acc_vars, acc_fns);
        }
        body.walk_locals(acc_vars, &mut local_scope);
        for func in local_scope.names() {
          if !fns.contains(func) {
            acc_fns.visit(func.to_owned(), ());
          }
        }
      }
      ExprF::Lambda(args, body) => {
        let vars: HashSet<_, RandomState> = args.iter_vars().map(|x| x.to_owned()).collect();
        let mut local_scope = Locals::new();
        body.walk_locals(&mut local_scope, acc_fns);
        for var in local_scope.names() {
          if !vars.contains(var) {
            if let Some(access_type) = local_scope.get(var) {
              acc_vars.visit(var.to_owned(), access_type.closed());
            }
          }
        }
      }
      ExprF::Assign(target, expr) => {
        match target {
          AssignTarget::Variable(_, s) => {
            acc_vars.visit(s.to_owned(), AccessType::RW);
          }
          AssignTarget::InstanceField(_, lhs, _) => {
            lhs.walk_locals(acc_vars, acc_fns);
          }
        }
        expr.walk_locals(acc_vars, acc_fns);
      }
      ExprF::FuncRef(target) => {
        match target {
          FuncRefTarget::SimpleName(name) => acc_fns.visit(name.to_owned(), ()),
        }
      }
      ExprF::Array(vec) => {
        for x in vec {
          x.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::Dictionary(vec) => {
        for (k, v) in vec {
          k.walk_locals(acc_vars, acc_fns);
          v.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::Quote(_) => {}
      ExprF::FieldAccess(lhs, _) => {
        lhs.walk_locals(acc_vars, acc_fns);
      }
      ExprF::MethodCall(lhs, _, args) => {
        lhs.walk_locals(acc_vars, acc_fns);
        for expr in args {
          expr.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::LambdaClass(cls) => {
        let LambdaClass { extends, args, constructor, decls } = &**cls;
        for arg in args {
          arg.walk_locals(acc_vars, acc_fns);
        }
        acc_vars.visit(extends.to_owned(), AccessType::ClosedRead);

        let (mut con_vars, con_fns) = constructor.get_names();
        for (_, access_type) in con_vars.iter_mut() {
          *access_type = access_type.closed();
        }
        acc_vars.merge_with(con_vars);
        acc_fns.merge_with(con_fns);

        for decl in decls {
          let (mut decl_vars, decl_fns) = decl.get_names();
          for (_, access_type) in decl_vars.iter_mut() {
            *access_type = access_type.closed();
          }
          acc_vars.merge_with(decl_vars);
          acc_fns.merge_with(decl_fns);
        }
      }
      ExprF::Yield(arg) => {
        if let Some((x, y)) = arg {
          x.walk_locals(acc_vars, acc_fns);
          y.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::Return(expr) => {
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

impl Sourced for Expr {
  type Item = ExprF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &ExprF {
    &self.value
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

  // For most of these tests, we don't care about SourceOffset, so
  // let's just make it easier to construct values with SourceOffset
  // of 0.
  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  #[test]
  fn test_locals_simple() {
    assert_eq!(e(ExprF::LocalVar(String::from("foobar"))).get_locals(), lhash(vec!("foobar".to_owned())));
    assert_eq!(e(ExprF::LocalVar(String::from("aaa"))).get_locals(), lhash(vec!("aaa".to_owned())));
    assert_eq!(e(ExprF::Literal(Literal::Int(99))).get_locals(), lhash(vec!()));
    assert_eq!(e(ExprF::Literal(Literal::Nil)).get_locals(), lhash(vec!()));
    assert_eq!(e(ExprF::Progn(vec!())).get_locals(), lhash(vec!()));
  }

  #[test]
  fn test_locals_compound() {
    let progn = e(ExprF::Progn(vec!(e(ExprF::LocalVar(String::from("aa"))),
                                    e(ExprF::LocalVar(String::from("bb"))))));
    assert_eq!(progn.get_locals(), lhash(vec!("aa".to_owned(), "bb".to_owned())));
  }

  #[test]
  fn test_locals_let() {

    // Declared variable
    let e1 = e(ExprF::Let(vec!(("var".to_owned(), e(ExprF::Literal(Literal::Nil)))),
                          Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e1.get_locals(), lhash(vec!()));

    // Declared and used variable
    let e2 = e(ExprF::Let(vec!(("var".to_owned(), e(ExprF::Literal(Literal::Nil)))),
                          Box::new(e(ExprF::LocalVar("var".to_owned())))));
    assert_eq!(e2.get_locals(), lhash(vec!()));

    // Different variable
    let e3 = e(ExprF::Let(vec!(("var_unused".to_owned(), e(ExprF::Literal(Literal::Nil)))),
                          Box::new(e(ExprF::LocalVar("var1".to_owned())))));
    assert_eq!(e3.get_locals(), lhash(vec!("var1".to_owned())));

    // Variable in decl
    let e4 = e(ExprF::Let(vec!(("var_unused".to_owned(), e(ExprF::LocalVar("var".to_owned())))),
                          Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e4.get_locals(), lhash(vec!("var".to_owned())));

    // Variable in decl (soon to be shadowed)
    let e4 = e(ExprF::Let(vec!(("var".to_owned(), e(ExprF::LocalVar("var".to_owned())))),
                          Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e4.get_locals(), lhash(vec!("var".to_owned())));

  }

  #[test]
  fn test_locals_assignment() {

    // Simple assignment
    let e1 = e(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("var")), Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e1.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Assignment including RHS
    let e2 = e(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("var1")), Box::new(e(ExprF::LocalVar("var2".to_owned())))));
    assert_eq!(e2.get_locals(), lhash_rw(vec!(("var1".to_owned(), AccessType::RW), ("var2".to_owned(), AccessType::Read))));

    // Reading and writing (I)
    let e3 = e(ExprF::Progn(vec!(
      e(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("var")), Box::new(e(ExprF::Literal(Literal::Nil))))),
      e(ExprF::LocalVar("var".to_owned())),
    )));
    assert_eq!(e3.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Reading and writing (II)
    let e4 = e(ExprF::Progn(vec!(
      e(ExprF::LocalVar("var".to_owned())),
      e(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("var")), Box::new(e(ExprF::Literal(Literal::Nil))))),
    )));
    assert_eq!(e4.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

  }

  #[test]
  fn test_functions_trivial() {
    let e1 = e(ExprF::Literal(Literal::Int(1)));
    assert_eq!(e1.get_functions(), fhash(vec!()));
  }

  #[test]
  fn test_functions_calls() {
    let e1 = e(ExprF::Call("abc".to_owned(), vec!(e(ExprF::Call("def".to_owned(), vec!())))));
    assert_eq!(e1.get_functions(), fhash(vec!("abc".to_owned(), "def".to_owned())));
  }

  #[test]
  fn test_functions_ref() {
    let e1 = e(ExprF::FuncRef(FuncRefTarget::SimpleName("abc".to_owned())));
    assert_eq!(e1.get_functions(), fhash(vec!("abc".to_owned())));
  }

}
