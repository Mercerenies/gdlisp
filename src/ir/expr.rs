
use super::literal;
use super::decl;
use super::arglist::ordinary::ArgList;
use super::closure_names::ClosureNames;
use super::access_type::AccessType;
use super::identifier::{Namespace, Id};
use super::special_ref::SpecialRef;
use super::special_form::local_binding;
use crate::sxp::ast::AST;
use crate::sxp::literal::{Literal as ASTLiteral};
use crate::compile::names;
use crate::pipeline::source::{SourceOffset, Sourced};
use crate::runner::path::RPathBuf;

use std::collections::HashSet;
use std::collections::hash_map::RandomState;
use std::borrow::Cow;

pub const DEFAULT_SPLIT_NAME: &str = "_split";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprF {
  BareName(BareName), // A (possibly atomic) bare name, referring to a variable.
  Call(CallTarget, String, Vec<Expr>),
  Literal(literal::Literal),
  Progn(Vec<Expr>),
  CondStmt(Vec<(Expr, Option<Expr>)>),
  WhileStmt(Box<Expr>, Box<Expr>),
  ForStmt(String, Box<Expr>, Box<Expr>),
  Let(Vec<LocalVarClause>, Box<Expr>),
  FunctionLet(FunctionBindingType, Vec<LocalFnClause>, Box<Expr>),
  Lambda(ArgList, Box<Expr>),
  FuncRef(FuncRefTarget),
  Assign(AssignTarget, Box<Expr>),
  Quote(AST),
  FieldAccess(Box<Expr>, String),
  LambdaClass(Box<LambdaClass>),
  Yield(Option<(Box<Expr>, Box<Expr>)>),
  Assert(Box<Expr>, Option<Box<Expr>>),
  Return(Box<Expr>),
  Break,
  Continue,
  SpecialRef(SpecialRef),
  ContextualFilename(RPathBuf),
  Split(String, Box<Expr>), // Compiles the inner expression, but forces it to be stored in a local variable with a generated name (the string argument is a prefix for the name)
  Preload(String),
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

/// The object on which a function call is being made, if any.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallTarget {
  /// The call is being made on the current scope itself, not on any
  /// particular object.
  Scoped,
  /// The call is being made on the special `super` object.
  ///
  /// This does *not* include superclass *constructor* calls, which
  /// are handled by a special method modifier in the syntax itself.
  /// See
  /// [`compile_class_inner_decl`](crate::ir::incremental::IncCompiler::compile_class_inner_decl)
  /// for details on those super calls.
  Super,
  /// The call is being made on a function whose name is considered
  /// atomic. This is similar to `Scoped` but the name will not be
  /// considered during any semantic analysis and will be passed
  /// through to GDScript unmodified.
  Atomic,
  /// The call is being made on an ordinary object in GDLisp. That is,
  /// this is a method call.
  Object(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BareName {
  /// An ordinary name, referring to a variable in GDLisp.
  Plain(String),
  /// An atomic name, which will be translated literally into GDScript
  /// without any regard for semantics.
  Atomic(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncRefTarget {
  SimpleName(String),
}

/// The type of binding to use in a function-namespaced let-binding,
/// i.e. [`ExprF::FunctionLet`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionBindingType {
  /// Outer-scoped binding, a la `flet`. Each function name is bound
  /// in an inner scope, with the function body interpreted in the
  /// outer scope that `flet` was invoked in. That is, functions
  /// cannot see themselves or other functions in the same `flet`
  /// block.
  OuterScoped,
  /// Recursive binding, a la `labels`. Each function name is bound in
  /// an inner scope, with the function body interpreted in that same
  /// inner scope. Functions in this binding type can see their own
  /// name and other functions from the same block.
  Recursive,
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

  pub fn var(name: impl Into<String>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::BareName(BareName::Plain(name.into())), pos)
  }

  pub fn atomic_var(name: impl Into<String>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::BareName(BareName::Atomic(name.into())), pos)
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

  pub fn nil(pos: SourceOffset) -> Expr {
    Expr::literal(literal::Literal::Nil, pos)
  }

  pub fn progn(body: Vec<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Progn(body), pos)
  }

  pub fn call(name: impl Into<String>, args: Vec<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Call(CallTarget::Scoped, name.into(), args), pos)
  }

  pub fn atomic_call(name: impl Into<String>, args: Vec<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Call(CallTarget::Atomic, name.into(), args), pos)
  }

  pub fn super_call(name: impl Into<String>, args: Vec<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Call(CallTarget::Super, name.into(), args), pos)
  }

  pub fn method_call(self, name: impl Into<String>, args: Vec<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Call(CallTarget::Object(Box::new(self)), name.into(), args), pos)
  }

  pub fn yield_none(pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Yield(None), pos)
  }

  pub fn yield_some(lhs: Expr, rhs: Expr, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Yield(Some((Box::new(lhs), Box::new(rhs)))), pos)
  }

  pub fn assert_expr(cond: Expr, message: Option<Expr>, pos: SourceOffset) -> Expr {
    Expr::new(ExprF::Assert(Box::new(cond), message.map(Box::new)), pos)
  }

  /// If `self` is a [`BareName::Plain`], then returns a reference to
  /// the inside of the name. If not, returns `None`.
  pub fn as_plain_name(&self) -> Option<&str> {
    if let ExprF::BareName(BareName::Plain(s)) = &self.value {
      Some(s)
    } else {
      None
    }
  }

  /// Converts the AST [`Literal`](crate::sxp::literal::Literal) value
  /// into an [`Expr`].
  pub fn from_ast_literal(ast_literal: &ASTLiteral, pos: SourceOffset) -> Expr {
    match ast_literal {
      ASTLiteral::Nil => {
        Expr::new(ExprF::Literal(literal::Literal::Nil), pos)
      }
      ASTLiteral::Int(n) => {
        Expr::new(ExprF::Literal(literal::Literal::Int(*n)), pos)
      }
      ASTLiteral::Bool(b) => {
        Expr::new(ExprF::Literal(literal::Literal::Bool(*b)), pos)
      }
      ASTLiteral::Float(f) => {
        Expr::new(ExprF::Literal(literal::Literal::Float(*f)), pos)
      }
      ASTLiteral::String(s) => {
        Expr::new(ExprF::Literal(literal::Literal::String(s.to_owned())), pos)
      }
      ASTLiteral::Symbol(s) => {
        Expr::new(ExprF::Literal(literal::Literal::Symbol(s.to_owned())), pos)
      }
    }
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
      ExprF::BareName(name) => {
        match name {
          BareName::Plain(s) => {
            acc_vars.visit(s.to_owned(), AccessType::Read, self.pos);
          }
          BareName::Atomic(_) => {
            // Never try to reason about these; they have no semantics
            // by definition.
          }
        }
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
        local_vars.remove(var);
        acc_vars.merge_with(local_vars);
      }
      ExprF::Call(object, name, args) => {
        match object {
          CallTarget::Scoped => {
            acc_fns.visit(name.to_owned(), (), self.pos);
          }
          CallTarget::Atomic => {
            // Do not log any additional names, other than those
            // visited in the arguments.
          }
          CallTarget::Super => {
            // A super call implicitly requires read access to a
            // `self` variable.
            acc_vars.visit(String::from("self"), AccessType::Read, self.pos);
          }
          CallTarget::Object(object) => {
            object.walk_locals(acc_vars, acc_fns);
          }
        }
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
      ExprF::FunctionLet(FunctionBindingType::OuterScoped, clauses, body) => {
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
      ExprF::FunctionLet(FunctionBindingType::Recursive, clauses, body) => {
        let mut fns = HashSet::new();
        let mut local_scope = Functions::new();
        for clause in clauses {
          let LocalFnClause { name, args, body: fbody } = clause;
          fns.insert(name.to_owned());
          let lambda_body = ExprF::Lambda(args.to_owned(), Box::new(fbody.to_owned()));
          Expr::new(lambda_body, self.pos).walk_locals(acc_vars, &mut local_scope);
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
            if let Some(v) = lhs.as_plain_name() {
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
      ExprF::Quote(_) => {}
      ExprF::FieldAccess(lhs, _) => {
        lhs.walk_locals(acc_vars, acc_fns);
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
      ExprF::Assert(cond, message) => {
        cond.walk_locals(acc_vars, acc_fns);
        if let Some(message) = message {
          message.walk_locals(acc_vars, acc_fns);
        }
      }
      ExprF::Return(expr) => {
        expr.walk_locals(acc_vars, acc_fns);
      }
      ExprF::Break => {}
      ExprF::Continue => {}
      ExprF::SpecialRef(_) => {}
      ExprF::ContextualFilename(_) => {}
      ExprF::Split(_, expr) => {
        // The "name" of the split is not a GDLisp-level name; it's a
        // hint to the later stages of the compiler. So it doesn't
        // matter for our purposes.
        expr.walk_locals(acc_vars, acc_fns);
      }
      ExprF::Preload(_) => {}
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

impl<T> From<T> for ExprF
where literal::Literal: From<T> {
  fn from(value: T) -> ExprF {
    ExprF::Literal(literal::Literal::from(value))
  }
}

impl BareName {

  pub fn to_gd_name(&self) -> String {
    match self {
      BareName::Plain(name) => names::lisp_to_gd(name),
      BareName::Atomic(name) => name.to_owned(),
    }
  }

  pub fn to_gd_name_bare(&self) -> String {
    match self {
      BareName::Plain(name) => names::lisp_to_gd_bare(name),
      BareName::Atomic(name) => name.to_owned(),
    }
  }

}

impl FunctionBindingType {

  /// Returns a correct [`local_binding::LocalBinding`] implementation
  /// for the function binding type.
  ///
  /// This function is the two-sided inverse of
  /// [`local_binding::LocalBinding::function_binding_type`].
  pub fn into_local_binding(self) -> Box<dyn local_binding::LocalBinding> {
    match self {
      FunctionBindingType::OuterScoped => Box::new(local_binding::FLetLocalBinding),
      FunctionBindingType::Recursive => Box::new(local_binding::LabelsLocalBinding),
    }
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
    assert_eq!(Expr::var("foobar", SourceOffset(0)).get_locals(), lhash(vec!("foobar".to_owned())));
    assert_eq!(Expr::var("aaa", SourceOffset(0)).get_locals(), lhash(vec!("aaa".to_owned())));
    assert_eq!(e(ExprF::Literal(Literal::Int(99))).get_locals(), lhash(vec!()));
    assert_eq!(e(ExprF::Literal(Literal::Nil)).get_locals(), lhash(vec!()));
    assert_eq!(e(ExprF::Progn(vec!())).get_locals(), lhash(vec!()));
  }

  #[test]
  fn test_locals_compound() {
    let progn = e(ExprF::Progn(vec!(Expr::var("aa", SourceOffset(0)),
                                    Expr::var("bb", SourceOffset(0)))));
    assert_eq!(progn.get_locals(), lhash(vec!("aa".to_owned(), "bb".to_owned())));
  }

  #[test]
  fn test_locals_super_call() {
    let super_call = Expr::super_call("foobar", vec!(Expr::var("aa", SourceOffset(0))), SourceOffset(0));
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
                          Box::new(Expr::var("var", SourceOffset(0)))));
    assert_eq!(e2.get_locals(), lhash(vec!()));

    // Different variable
    let e3 = e(ExprF::Let(vec!(lvc("var_unused", e(ExprF::Literal(Literal::Nil)))),
                          Box::new(Expr::var("var1", SourceOffset(0)))));
    assert_eq!(e3.get_locals(), lhash(vec!("var1".to_owned())));

    // Variable in decl
    let e4 = e(ExprF::Let(vec!(lvc("var_unused", Expr::var("var", SourceOffset(0)))),
                          Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e4.get_locals(), lhash(vec!("var".to_owned())));

    // Variable in decl (soon to be shadowed)
    let e4 = e(ExprF::Let(vec!(lvc("var", Expr::var("var", SourceOffset(0)))),
                          Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e4.get_locals(), lhash(vec!("var".to_owned())));

  }

  #[test]
  fn test_locals_assignment() {

    // Simple assignment
    let e1 = e(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("var")), Box::new(e(ExprF::Literal(Literal::Nil)))));
    assert_eq!(e1.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Assignment including RHS
    let e2 = e(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("var1")), Box::new(Expr::var("var2", SourceOffset(0)))));
    assert_eq!(e2.get_locals(), lhash_rw(vec!(("var1".to_owned(), AccessType::RW), ("var2".to_owned(), AccessType::Read))));

    // Reading and writing (I)
    let e3 = e(ExprF::Progn(vec!(
      e(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("var")), Box::new(e(ExprF::Literal(Literal::Nil))))),
      Expr::var("var", SourceOffset(0)),
    )));
    assert_eq!(e3.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Reading and writing (II)
    let e4 = e(ExprF::Progn(vec!(
      Expr::var("var", SourceOffset(0)),
      e(ExprF::Assign(AssignTarget::Variable(SourceOffset::default(), String::from("var")), Box::new(e(ExprF::Literal(Literal::Nil))))),
    )));
    assert_eq!(e4.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

  }

  #[test]
  fn test_locals_slot_assignment() {

    // Simple slot assignment
    let e1 = e(ExprF::Assign(
      AssignTarget::InstanceField(SourceOffset::default(), Box::new(Expr::var("var", SourceOffset(0))), String::from("foo")),
      Box::new(e(ExprF::Literal(Literal::Nil))),
    ));
    assert_eq!(e1.get_locals(), lhash_rw(vec!(("var".to_owned(), AccessType::RW))));

    // Nested slot assignment
    let e2 = e(ExprF::Assign(
      AssignTarget::InstanceField(SourceOffset::default(),
                                  Box::new(e(ExprF::FieldAccess(Box::new(Expr::var("var", SourceOffset(0))), String::from("foo")))),
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
    let e1 = Expr::call("abc", vec!(Expr::call("def", vec!(), SourceOffset(0))), SourceOffset(0));
    assert_eq!(e1.get_functions(), fhash(vec!("abc".to_owned(), "def".to_owned())));
  }

  #[test]
  fn test_functions_ref() {
    let e1 = e(ExprF::FuncRef(FuncRefTarget::SimpleName("abc".to_owned())));
    assert_eq!(e1.get_functions(), fhash(vec!("abc".to_owned())));
  }

  #[test]
  fn test_function_binding_type_local_binding_inverse() {
    assert_eq!(FunctionBindingType::OuterScoped.into_local_binding().function_binding_type(),
               FunctionBindingType::OuterScoped);
    assert_eq!(FunctionBindingType::Recursive.into_local_binding().function_binding_type(),
               FunctionBindingType::Recursive);
  }

}
