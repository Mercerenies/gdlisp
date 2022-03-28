
use super::literal;
use super::decl;
use super::arglist::ArgList;
use super::closure_names::ClosureNames;
use super::access_type::AccessType;
use super::identifier::{Namespace, Id};
use super::special_ref::SpecialRef;
use crate::sxp::ast::AST;
use crate::pipeline::source::{SourceOffset, Sourced};
use crate::runner::path::RPathBuf;

use std::collections::HashSet;
use std::collections::hash_map::RandomState;
use std::borrow::Cow;

pub const DEFAULT_SPLIT_NAME: &str = "_split";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprF {
  LocalVar(String),
  Literal(literal::Literal),
  Progn(Vec<Expr>),
  CondStmt(Vec<(Expr, Option<Expr>)>),
  WhileStmt(Box<Expr>, Box<Expr>),
  ForStmt(String, Box<Expr>, Box<Expr>),
  Call(String, Vec<Expr>),
  Let(Vec<LocalVarClause>, Box<Expr>),
  FLet(Vec<LocalFnClause>, Box<Expr>),
  Labels(Vec<LocalFnClause>, Box<Expr>),
  Lambda(ArgList, Box<Expr>),
  FuncRef(FuncRefTarget),
  Assign(AssignTarget, Box<Expr>),
  Array(Vec<Expr>),
  Dictionary(Vec<(Expr, Expr)>),
  Quote(AST),
  FieldAccess(Box<Expr>, String),
  MethodCall(Box<Expr>, String, Vec<Expr>),
  SuperCall(String, Vec<Expr>),
  LambdaClass(Box<LambdaClass>),
  Yield(Option<(Box<Expr>, Box<Expr>)>),
  Return(Box<Expr>),
  SpecialRef(SpecialRef),
  ContextualFilename(RPathBuf),
  AtomicName(String),
  AtomicCall(String, Vec<Expr>),
  Split(String, Box<Expr>), // Compiles the inner expression, but forces it to be stored in a local variable with a generated name (the string argument is a prefix for the name)
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
  pub constructor: Option<decl::ConstructorDecl>,
  pub decls: Vec<decl::ClassInnerDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalFnClause {
  pub name: String,
  pub args: ArgList,
  pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVarClause {
  pub name: String,
  pub value: Expr,
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

  pub fn yield_none(pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Yield(None), pos)
  }

  pub fn yield_some(lhs: Expr, rhs: Expr, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Yield(Some((Box::new(lhs), Box::new(rhs)))), pos)
  }

  /// Wraps the expression in a 0-ary lambda which is immediately
  /// invoked.
  ///
  /// If `expr` is the starting expression, then the result is
  /// conceptually `(funcall (lambda () expr))`. This is used in
  /// certain contexts in compilation, such as in a parent constructor
  /// invocation, where we have expression context but have nowhere to
  /// place helper statements produced by a
  /// [`StmtBuilder`](crate::compile::body::builder::StmtBuilder).
  pub fn self_evaluating_lambda(self) -> Expr {
    let pos = self.pos;
    Expr::call(
      String::from("sys/funcall"),
      vec!(Expr::new(ExprF::Lambda(ArgList::empty(), Box::new(self)), pos)),
      pos,
    )
  }

  pub fn from_value<T>(value: T, pos: SourceOffset) -> Expr
  where ExprF : From<T> {
    Expr::new(ExprF::from(value), pos)
  }

  pub fn named_split(self, name: &str, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Split(name.to_owned(), Box::new(self)), pos)
  }

  pub fn split(self, pos: SourceOffset) -> Expr {
    self.named_split(DEFAULT_SPLIT_NAME, pos)
  }

  fn walk_locals(&self, acc_vars: &mut Locals, acc_fns: &mut Functions) {
    match &self.value {
      ExprF::LocalVar(s) => {
        acc_vars.visit(s.to_owned(), AccessType::Read, self.pos);
      }
      ExprF::Literal(_) => {}
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
        acc_fns.visit(name.to_owned(), (), self.pos);
        for expr in args {
          expr.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::Let(clauses, body) => {
        let mut vars = HashSet::new();
        for clause in clauses {
          vars.insert(clause.name.to_owned());
          clause.value.walk_locals(acc_vars, acc_fns);
        }
        let mut local_scope = Locals::new();
        body.walk_locals(&mut local_scope, acc_fns);
        for (var, access_type, pos) in local_scope.into_iter_with_offset() {
          if !vars.contains(&var) {
            acc_vars.visit(var, access_type, pos);
          }
        }
      }
      ExprF::FLet(clauses, body) => {
        let mut fns = HashSet::new();
        for clause in clauses {
          let LocalFnClause { name, args, body: fbody } = clause;
          fns.insert(name.to_owned());
          let lambda_body = ExprF::Lambda(args.to_owned(), Box::new(fbody.to_owned()));
          Expr::new(lambda_body, self.pos).walk_locals(acc_vars, acc_fns);
        }
        let mut local_scope = Functions::new();
        body.walk_locals(acc_vars, &mut local_scope);
        for (func, (), pos) in local_scope.into_iter_with_offset() {
          if !fns.contains(&func) {
            acc_fns.visit(func, (), pos);
          }
        }
      }
      ExprF::Labels(clauses, body) => {
        let mut fns = HashSet::new();
        for clause in clauses {
          fns.insert(clause.name.to_owned());
        }
        // Note that we consider the bodies of the local functions to
        // be part of the local scope. This is in contrast to the
        // (simpler) FLet case, where only the body of the FLet itself
        // is localized.
        let mut local_scope = Functions::new();
        for clause in clauses {
          let LocalFnClause { name: _, args, body: fbody } = clause;
          let lambda_body = ExprF::Lambda(args.to_owned(), Box::new(fbody.to_owned()));
          Expr::new(lambda_body, self.pos).walk_locals(acc_vars, acc_fns);
        }
        body.walk_locals(acc_vars, &mut local_scope);
        for (func, (), pos) in local_scope.into_iter_with_offset() {
          if !fns.contains(&func) {
            acc_fns.visit(func, (), pos);
          }
        }
      }
      ExprF::Lambda(args, body) => {
        let vars: HashSet<_, RandomState> = args.iter_vars().map(|x| x.to_owned()).collect();
        let mut local_scope = Locals::new();
        body.walk_locals(&mut local_scope, acc_fns);
        for (var, access_type, pos) in local_scope.into_iter_with_offset() {
          if !vars.contains(&var) {
            acc_vars.visit(var, access_type.closed(), pos);
          }
        }
      }
      ExprF::Assign(target, expr) => {
        match target {
          AssignTarget::Variable(pos, s) => {
            acc_vars.visit(s.to_owned(), AccessType::RW, *pos);
          }
          AssignTarget::InstanceField(_, lhs, _) => {
            lhs.walk_locals(acc_vars, acc_fns);
            // If the LHS is specifically a variable, then that
            // variable also becomes RW, since it might contain a COW
            // value.
            if let ExprF::LocalVar(v) = &lhs.value {
              acc_vars.visit(v.to_owned(), AccessType::RW, lhs.pos);
            }
          }
        }
        expr.walk_locals(acc_vars, acc_fns);
      }
      ExprF::FuncRef(target) => {
        match target {
          FuncRefTarget::SimpleName(name) => acc_fns.visit(name.to_owned(), (), self.pos),
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
      ExprF::SuperCall(_, args) => {
        acc_vars.visit(String::from("self"), AccessType::Read, self.pos);
        for expr in args {
          expr.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::LambdaClass(cls) => {
        let LambdaClass { extends, args, constructor, decls } = &**cls;
        for arg in args {
          arg.walk_locals(acc_vars, acc_fns);
        }
        acc_vars.visit(extends.to_owned(), AccessType::ClosedRead, self.pos);

        let (mut con_vars, con_fns) =
          constructor.as_ref().map_or_else(|| (ClosureNames::new(), ClosureNames::new()),
                                           |x| x.get_names());
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
      ExprF::SpecialRef(_) => {}
      ExprF::ContextualFilename(_) => {}
      ExprF::AtomicName(_) => {}
      ExprF::AtomicCall(_, args) => {
        for arg in args {
          arg.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::Split(_, expr) => {
        // The "name" of the split is not a GDLisp-level name; it's a
        // hint to the later stages of the compiler. So it doesn't
        // matter for our purposes.
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

  pub fn get_ids(&self) -> impl Iterator<Item=(Id, SourceOffset)> {
    let (vars, fns) = self.get_names();
    let vars = vars.into_iter_with_offset().map(|(name, _, pos)| (Id::new(Namespace::Value, name), pos));
    let fns = fns.into_iter_with_offset().map(|(name, (), pos)| (Id::new(Namespace::Function, name), pos));
    Iterator::chain(vars, fns)
  }

}

impl LambdaClass {

  /// See [`decl::ClassDecl::constructor_or_default`].
  pub fn constructor_or_default(&self, default_pos: SourceOffset) -> Cow<decl::ConstructorDecl> {
    self.constructor.as_ref().map_or_else(|| Cow::Owned(decl::ConstructorDecl::empty(default_pos)), Cow::Borrowed)
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
    Locals::from_hashmap(vec.into_iter().map(|x| (x, (AccessType::Read, SourceOffset(0)))).collect())
  }

  fn lhash_rw(vec: Vec<(String, AccessType)>) -> Locals {
    Locals::from_hashmap(vec.into_iter().map(|(x, t)| (x, (t, SourceOffset(0)))).collect())
  }

  fn fhash(vec: Vec<String>) -> Functions {
    Functions::from_hashmap(vec.into_iter().map(|x| (x, ((), SourceOffset(0)))).collect())
  }

  fn lvc(name: &str, value: Expr) -> LocalVarClause {
    LocalVarClause {
      name: name.to_owned(),
      value,
    }
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
  fn test_locals_super_call() {
    let super_call = e(ExprF::SuperCall(String::from("foobar"), vec!(e(ExprF::LocalVar(String::from("aa"))))));
    assert_eq!(super_call.get_locals(), lhash(vec!("aa".to_owned(), "self".to_owned())));
  }

  #[test]
  fn test_locals_let() {

    // Declared variable
    let e1 = e(ExprF::Let(vec!(lvc("var", e(ExprF::Literal(Literal::Nil)))),
                          Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e1.get_locals(), lhash(vec!()));

    // Declared and used variable
    let e2 = e(ExprF::Let(vec!(lvc("var", e(ExprF::Literal(Literal::Nil)))),
                          Box::new(e(ExprF::LocalVar("var".to_owned())))));
    assert_eq!(e2.get_locals(), lhash(vec!()));

    // Different variable
    let e3 = e(ExprF::Let(vec!(lvc("var_unused", e(ExprF::Literal(Literal::Nil)))),
                          Box::new(e(ExprF::LocalVar("var1".to_owned())))));
    assert_eq!(e3.get_locals(), lhash(vec!("var1".to_owned())));

    // Variable in decl
    let e4 = e(ExprF::Let(vec!(lvc("var_unused", e(ExprF::LocalVar("var".to_owned())))),
                          Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e4.get_locals(), lhash(vec!("var".to_owned())));

    // Variable in decl (soon to be shadowed)
    let e4 = e(ExprF::Let(vec!(lvc("var", e(ExprF::LocalVar("var".to_owned())))),
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
  fn test_locals_slot_assignment() {

    // Simple slot assignment
    let e1 = e(ExprF::Assign(
      AssignTarget::InstanceField(SourceOffset::default(), Box::new(e(ExprF::LocalVar(String::from("var")))), String::from("foo")),
      Box::new(e(ExprF::Literal(Literal::Nil))),
    ));
    assert_eq!(e1.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Nested slot assignment
    let e2 = e(ExprF::Assign(
      AssignTarget::InstanceField(SourceOffset::default(),
                                  Box::new(e(ExprF::FieldAccess(Box::new(e(ExprF::LocalVar(String::from("var")))), String::from("foo")))),
                                  String::from("baro")),
      Box::new(e(ExprF::Literal(Literal::Nil))),
    ));
    assert_eq!(e2.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::Read))));

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
