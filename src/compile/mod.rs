
pub mod names;
pub mod body;
pub mod error;
pub mod stmt_wrapper;
pub mod symbol_table;
pub mod special_form;
pub mod builtin;

use body::builder::StmtBuilder;
use names::fresh::FreshNameGenerator;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::literal::Literal;
use crate::gdscript::library;
use crate::gdscript::op;
use crate::gdscript::arglist::ArgList;
use error::Error;
use stmt_wrapper::StmtWrapper;
use symbol_table::{HasSymbolTable, SymbolTable};
use crate::ir;

type IRExpr = ir::expr::Expr;
type IRLiteral = ir::literal::Literal;

pub struct Compiler<'a> {
  gen: FreshNameGenerator<'a>,
}

#[derive(Debug, Clone)]
pub struct StExpr(pub Expr, pub bool); // An expression and a declaration of whether or not it's stateful.

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NeedsResult { No, Yes }

impl From<NeedsResult> for bool {
  fn from(s: NeedsResult) -> bool {
    s == NeedsResult::Yes
  }
}

impl From<bool> for NeedsResult {
  fn from(b: bool) -> NeedsResult {
    if b { NeedsResult::Yes } else { NeedsResult::No }
  }
}

impl<'a> Compiler<'a> {

  pub fn new(gen: FreshNameGenerator<'a>) -> Compiler<'a> {
    Compiler { gen }
  }

  pub fn compile_stmts(&mut self,
                       builder: &mut StmtBuilder,
                       table: &mut SymbolTable,
                       stmts: &[&IRExpr],
                       needs_result: NeedsResult)
                       -> Result<StExpr, Error> {
    if stmts.is_empty() {
      Ok(Compiler::nil_expr())
    } else {
      let prefix = &stmts[..stmts.len()-1];
      let end = &stmts[stmts.len()-1];
      for x in prefix {
        self.compile_stmt(builder, table, &mut stmt_wrapper::Vacuous, x)?;
      }
      self.compile_expr(builder, table, end, needs_result)
    }
  }

  pub fn compile_stmt(&mut self,
                      builder: &mut StmtBuilder,
                      table: &mut SymbolTable,
                      destination: &dyn StmtWrapper,
                      stmt: &IRExpr)
                      -> Result<(), Error> {
    let needs_result = NeedsResult::from(!destination.is_vacuous());
    let expr = self.compile_expr(builder, table, stmt, needs_result)?;
    destination.wrap_to_builder(builder, expr);
    Ok(())
  }

  pub fn compile_expr(&mut self,
                      builder: &mut StmtBuilder,
                      table: &mut SymbolTable,
                      expr: &IRExpr,
                      needs_result: NeedsResult)
                      -> Result<StExpr, Error> {
    // TODO I made a mess of this when converting to IR. Separate this
    // into many helper functions, probably over multiple files.
    match expr {
      IRExpr::LocalVar(s) => {
        table.get_var(s).ok_or_else(|| Error::NoSuchVar(s.clone())).map(|var| {
          StExpr(Expr::Var(var.to_string()), false)
        })
      }
      IRExpr::Literal(lit) => {
        match lit {
          IRLiteral::Nil => Ok(Compiler::nil_expr()),
          IRLiteral::Int(n) => Ok(StExpr(Expr::Literal(Literal::Int(*n)), false)),
        }
      }
      IRExpr::Progn(body) => {
        let body: Vec<_> = body.iter().map(|x| x).collect(); // TODO Hilarious copy that should be removable.
        self.compile_stmts(builder, table, &body[..], needs_result)
      }
      IRExpr::IfStmt(c, t, f) => {
        let (destination, result) = if needs_result.into() {
          let var_name = self.declare_var(builder, "_if", None);
          let destination = Box::new(stmt_wrapper::AssignToVar(var_name.clone())) as Box<dyn StmtWrapper>;
          (destination, StExpr(Expr::Var(var_name), false))
        } else {
          let destination = Box::new(stmt_wrapper::Vacuous) as Box<dyn StmtWrapper>;
          (destination, Compiler::nil_expr())
        };
        let cond_expr = self.compile_expr(builder, table, c, NeedsResult::Yes)?.0;
        let mut true_builder = StmtBuilder::new();
        let mut false_builder = StmtBuilder::new();
        self.compile_stmt(&mut true_builder , table, destination.as_ref(), t)?;
        self.compile_stmt(&mut false_builder, table, destination.as_ref(), f)?;
        let true_body  =  true_builder.build_into(builder);
        let false_body = false_builder.build_into(builder);
        builder.append(stmt::if_else(cond_expr, true_body, false_body));
        Ok(result)
      }
      IRExpr::CondStmt(clauses) => {
        let (destination, result) = if needs_result.into() {
          let var_name = self.declare_var(builder, "_cond", None);
          let destination = Box::new(stmt_wrapper::AssignToVar(var_name.clone())) as Box<dyn StmtWrapper>;
          (destination, StExpr(Expr::Var(var_name), false))
        } else {
          let destination = Box::new(stmt_wrapper::Vacuous) as Box<dyn StmtWrapper>;
          (destination, Compiler::nil_expr())
        };
        let init: Vec<Stmt> = destination.wrap_to_stmts(Compiler::nil_expr());
        let body = clauses.iter().rev().fold(Ok(init), |acc: Result<_, Error>, curr| {
          let acc = acc?;
          let (cond, body) = curr;
          match body {
            None => {
              let mut outer_builder = StmtBuilder::new();
              let mut inner_builder = StmtBuilder::new();
              let cond = self.compile_expr(&mut outer_builder, table, cond, NeedsResult::Yes)?.0;
              let var_name = self.declare_var(&mut outer_builder, "_cond", Some(cond));
              destination.wrap_to_builder(&mut inner_builder, StExpr(Expr::Var(var_name.clone()), false));
              let if_branch = inner_builder.build_into(builder);
              outer_builder.append(stmt::if_else(Expr::Var(var_name.clone()), if_branch, acc));
              Ok(outer_builder.build_into(builder))
            }
            Some(body) => {
              let mut outer_builder = StmtBuilder::new();
              let mut inner_builder = StmtBuilder::new();
              let cond = self.compile_expr(&mut outer_builder, table, cond, NeedsResult::Yes)?.0;
              self.compile_stmt(&mut inner_builder, table, destination.as_ref(), body)?;
              let if_branch = inner_builder.build_into(builder);
              outer_builder.append(stmt::if_else(cond, if_branch, acc));
              Ok(outer_builder.build_into(builder))
            }
          }
        })?;
        builder.append_all(&mut body.into_iter());
        Ok(result)
      }
      IRExpr::Call(f, args) => {
        let fcall = match table.get_fn(f) {
          None => return Err(Error::NoSuchFn(f.clone())),
          Some(x) => x.clone(),
        };
        let args = args.into_iter()
                       .map(|x| self.compile_expr(builder, table, x, NeedsResult::Yes))
                       .map(|x| x.map(|y| y.0))
                       .collect::<Result<Vec<_>, _>>()?;
        Ok(StExpr(fcall.into_expr(args), true))
      }
      IRExpr::Let(clauses, body) => {
        let var_names = clauses.iter().map::<Result<(String, String), Error>, _>(|clause| {
          let (ast_name, expr) = clause;
          let ast_name = ast_name.to_owned();
          let result_value = self.compile_expr(builder, table, &expr, NeedsResult::Yes)?.0;
          let gd_name = self.declare_var(builder, &ast_name, Some(result_value));
          Ok((ast_name, gd_name))
        }).collect::<Result<Vec<_>, _>>()?;
        table.with_local_vars(&mut var_names.into_iter(), |table| {
          self.compile_expr(builder, table, body, needs_result)
        })
      }
      IRExpr::Lambda(args, body) => {
        let arg_names = args.iter().map(|s| {
          let ast_name = s.clone();
          let gd_name = self.name_generator().generate_with(&ast_name);
          (ast_name, gd_name)
        }).collect::<Vec<_>>();

        let mut lambda_builder = StmtBuilder::new();
        let mut closure_vars = body.get_locals();
        for arg in args {
          closure_vars.remove(arg);
        }
        // I want them in a consistent order for the constructor
        // function. I don't care which order, but I need an order, so
        // let's make a Vec now.
        let closure_vars: Vec<_> = closure_vars.into_iter().collect();

        let mut lambda_table = SymbolTable::new();
        for arg in &arg_names {
          lambda_table.set_var(arg.0.to_owned(), arg.1.to_owned());
        }
        for var in &closure_vars {
          // Ensure the variable actually exists
          match table.get_var(var) {
            None => return Err(Error::NoSuchVar(var.clone())),
            Some(gdvar) => lambda_table.set_var(var.clone(), gdvar.to_owned()), // TODO Generate new names here
          };
        }

        let gd_closure_vars = closure_vars.iter().map(|ast_name| {
          lambda_table.get_var(&ast_name).unwrap_or_else(|| {
            panic!("Internal error compiling lambda variable {}", ast_name)
          }).to_owned()
        }).collect();

        self.compile_stmt(&mut lambda_builder, &mut lambda_table, &stmt_wrapper::Return, body)?;
        let arglist = ArgList::required(arg_names.into_iter().map(|x| x.1.clone()).collect());
        let class = self.generate_lambda_class(arglist, &gd_closure_vars, builder, lambda_builder);
        let class_name = class.name.clone();
        builder.add_helper(Decl::ClassDecl(class));
        let constructor_args = gd_closure_vars.into_iter().map(|s| Expr::Var(s.to_owned())).collect();
        let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), constructor_args);
        Ok(StExpr(expr, false))
      }
      IRExpr::Funcall(f, args) => {
        let func_expr = self.compile_expr(builder, table, f, NeedsResult::Yes)?.0;
        let args_expr = args.iter().map(|arg| {
          self.compile_expr(builder, table, arg, NeedsResult::Yes).map(|x| x.0)
        }).collect::<Result<Vec<_>, _>>()?;
        let fn_name = String::from("call_func");
        let expr = Expr::Call(Some(Box::new(func_expr)), fn_name, args_expr);
        Ok(StExpr(expr, true))
      }
    }
  }

  fn generate_lambda_class(&mut self,
                           args: ArgList,
                           closed_vars: &Vec<String>,
                           parent_builder: &mut StmtBuilder,
                           lambda_builder: StmtBuilder)
                           -> decl::ClassDecl {
    let class_name = self.gen.generate_with("_LambdaBlock");
    let func_name = String::from("call_func");
    let func_body = lambda_builder.build_into(parent_builder);
    let func = decl::FnDecl {
      name: func_name,
      args: args,
      body: func_body,
    };
    let constructor =
    decl::FnDecl {
      name: String::from("_init"),
      args: ArgList::required(closed_vars.iter().map(|x| (*x).to_owned()).collect()),
      body: closed_vars.iter().map(|name| Compiler::assign_to_self(name.to_string(), name.to_string())).collect(),
    };
    let mut class_body = vec!();
    for var in closed_vars {
      class_body.push(Decl::VarDecl(var.clone(), None));
    }
    class_body.append(&mut vec!(
      Decl::FnDecl(decl::Static::NonStatic, constructor),
      Decl::FnDecl(decl::Static::NonStatic, func),
    ));
    decl::ClassDecl {
      name: class_name,
      extends: decl::ClassExtends::Named(String::from("Reference")),
      body: class_body,
    }
  }

  fn assign_to_self(inst_var: String, local_var: String) -> Stmt {
    let self_target = Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("self"))), inst_var));
    let value = Box::new(Expr::Var(local_var));
    Stmt::Assign(self_target, op::AssignOp::Eq, value)
  }

  pub fn nil_expr() -> StExpr {
    StExpr(library::nil(), false)
  }

  pub fn name_generator(&mut self) -> &mut FreshNameGenerator<'a> {
    &mut self.gen
  }

  fn declare_var(&mut self, builder: &mut StmtBuilder, prefix: &str, value: Option<Expr>) -> String {
    let var_name = self.gen.generate_with(prefix);
    let value = value.unwrap_or(Compiler::nil_expr().0);
    builder.append(Stmt::VarDecl(var_name.clone(), value));
    var_name
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::decl::Decl;
  use crate::sxp::ast::{self, AST};
  use crate::compile::symbol_table::function_call::{FnCall, FnScope};

  // TODO A lot more of this

  fn bind_helper_symbols(table: &mut SymbolTable) {
    // Binds a few helper names to the symbol table for the sake of
    // debugging.
    table.set_fn(String::from("foobar"), FnCall::unqualified(FnScope::Global, String::from("foobar")));
    table.set_fn(String::from("foo"), FnCall::unqualified(FnScope::Global, String::from("foo")));
    table.set_fn(String::from("bar"), FnCall::unqualified(FnScope::Global, String::from("bar")));
    table.set_var(String::from("foobar"), String::from("foobar"));
  }

  fn compile_stmt(ast: &AST) -> Result<(Vec<Stmt>, Vec<Decl>), Error> {
    let used_names = ast.all_symbols();
    let mut compiler = Compiler::new(FreshNameGenerator::new(used_names));
    let mut table = SymbolTable::new();
    bind_helper_symbols(&mut table);
    library::bind_builtins(&mut table);
    let mut builder = StmtBuilder::new();
    let expr = ir::compile_expr(ast)?;
    let () = compiler.compile_stmt(&mut builder, &mut table, &mut stmt_wrapper::Return, &expr)?;
    Ok(builder.build())
  }

  #[test]
  fn compile_var() {
    let ast = AST::Symbol(String::from("foobar"));
    let expected = Stmt::ReturnStmt(Expr::Var(String::from("foobar")));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_call() {
    let ast = ast::list(vec!(AST::Symbol(String::from("foobar")), AST::Int(10)));
    let expected = Stmt::ReturnStmt(Expr::Call(None, String::from("foobar"), vec!(Expr::Literal(Literal::Int(10)))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_int() {
    let ast = AST::Int(99);
    let expected = Stmt::ReturnStmt(Expr::Literal(Literal::Int(99)));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_vacuous() {
    let ast = ast::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1), AST::Int(2)));
    let expected = vec!(Stmt::ReturnStmt(Expr::Literal(Literal::Int(2))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_stateful() {
    let ast = ast::list(vec!(AST::Symbol(String::from("progn")),
                             ast::list(vec!(AST::Symbol(String::from("foo")))),
                             ast::list(vec!(AST::Symbol(String::from("bar"))))));
    let expected = vec!(Stmt::Expr(Expr::Call(None, String::from("foo"), vec!())),
                        Stmt::ReturnStmt(Expr::Call(None, String::from("bar"), vec!())));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_nil() {
    let result1 = compile_stmt(&AST::Nil).unwrap();
    assert_eq!(result1, (vec!(Stmt::ReturnStmt(Compiler::nil_expr().0)), vec!()));

    let result2 = compile_stmt(&ast::list(vec!(AST::Symbol(String::from("progn"))))).unwrap();
    assert_eq!(result2, (vec!(Stmt::ReturnStmt(Compiler::nil_expr().0)), vec!()));
  }

}
