
pub mod names;
pub mod body;
pub mod error;
pub mod stmt_wrapper;
pub mod symbol_table;
pub mod special_form;
pub mod builtin;

use body::builder::{CodeBuilder, StmtBuilder, HasDecls};
use names::fresh::FreshNameGenerator;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::{self, Stmt};
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::library;
use crate::gdscript::op;
use crate::gdscript::arglist::ArgList;
use error::Error;
use stmt_wrapper::StmtWrapper;
use symbol_table::{HasSymbolTable, SymbolTable};
use symbol_table::function_call::{self, FnSpecs};
use crate::ir;

use std::convert::TryInto;

type IRDecl = ir::decl::Decl;
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
          IRLiteral::Int(n) => Ok(StExpr(Expr::from(*n), false)),
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
        Ok(StExpr(fcall.into_expr(args)?, true))
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
        let (arglist, gd_args) = args.clone().into_gd_arglist(&mut self.gen);

        let mut lambda_builder = StmtBuilder::new();
        let mut closure_vars = body.get_locals();
        for arg in &gd_args {
          closure_vars.remove(&arg.0);
        }
        // I want them in a consistent order for the constructor
        // function. I don't care which order, but I need an order, so
        // let's make a Vec now.
        let closure_vars: Vec<_> = closure_vars.into_iter().collect();

        let mut lambda_table = SymbolTable::new();
        for arg in &gd_args {
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
        let class = self.generate_lambda_class(args.clone().into(), arglist, &gd_closure_vars, builder, lambda_builder);
        let class_name = class.name.clone();
        builder.add_helper(Decl::ClassDecl(class));
        let constructor_args = gd_closure_vars.into_iter().map(|s| Expr::Var(s.to_owned())).collect();
        let expr = Expr::Call(Some(Box::new(Expr::Var(class_name))), String::from("new"), constructor_args);
        Ok(StExpr(expr, false))
      }
      /* // This will eventually be an optimization.
      IRExpr::Funcall(f, args) => {
        let func_expr = self.compile_expr(builder, table, f, NeedsResult::Yes)?.0;
        let args_expr = args.iter().map(|arg| {
          self.compile_expr(builder, table, arg, NeedsResult::Yes).map(|x| x.0)
        }).collect::<Result<Vec<_>, _>>()?;
        let fn_name = String::from("call_func");
        let expr = Expr::Call(Some(Box::new(func_expr)), fn_name, args_expr);
        Ok(StExpr(expr, true))
      }
      */
    }
  }

  fn generate_lambda_vararg(&mut self, specs: FnSpecs) -> decl::FnDecl {
    let mut builder = StmtBuilder::new();
    let args = String::from("args");
    let required: Vec<_> = (0..specs.required).map(|_| self.gen.generate_with("required")).collect();
    let optional: Vec<_> = (0..specs.optional).map(|_| self.gen.generate_with("optional")).collect();

    // TODO Make these nulls actually part of the GDScript AST

    for req in &required {
      builder.append(Stmt::VarDecl(req.to_owned(), Expr::null()));
      builder.append(stmt::if_else(
        Expr::Binary(Box::new(Expr::Var(String::from("args"))), op::BinaryOp::Is, Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("GDLisp"))), String::from("NilClass")))),
        vec!(
          Stmt::Expr(Expr::Call(None, String::from("push_error"), vec!(Expr::str_lit("Not enough arguments"))))
        ),
        vec!(
          Stmt::Assign(Box::new(Expr::Var(req.to_owned())), op::AssignOp::Eq, Box::new(Expr::Attribute(Box::new(Expr::Var(args.clone())), String::from("car")))),
          Stmt::Assign(Box::new(Expr::Var(args.clone())), op::AssignOp::Eq, Box::new(Expr::Attribute(Box::new(Expr::Var(args.clone())), String::from("cdr"))))
        ),
      ));
    }

    for opt in &optional {
      builder.append(Stmt::VarDecl(opt.to_owned(), Expr::null()));
      builder.append(stmt::if_else(
        Expr::Binary(Box::new(Expr::Var(String::from("args"))), op::BinaryOp::Is, Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("GDLisp"))), String::from("NilClass")))),
        vec!(
          Stmt::Assign(Box::new(Expr::Var(opt.to_owned())), op::AssignOp::Eq, Box::new(library::nil()))
        ),
        vec!(
          Stmt::Assign(Box::new(Expr::Var(opt.to_owned())), op::AssignOp::Eq, Box::new(Expr::Attribute(Box::new(Expr::Var(args.clone())), String::from("car")))),
          Stmt::Assign(Box::new(Expr::Var(args.clone())), op::AssignOp::Eq, Box::new(Expr::Attribute(Box::new(Expr::Var(args.clone())), String::from("cdr"))))
        ),
      ));
    }

    let mut all_args: Vec<_> =
      required.into_iter()
      .chain(optional.into_iter())
      .map(|x| Expr::Var(x))
      .collect();
    if specs.rest {
      all_args.push(Expr::Var(args));
      builder.append(Stmt::ReturnStmt(Expr::Call(None, String::from("call_func"), all_args)));
    } else {
      builder.append(
        stmt::if_else(
          Expr::Binary(Box::new(Expr::Var(String::from("args"))), op::BinaryOp::Is, Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("GDLisp"))), String::from("NilClass")))),
          vec!(
            Stmt::ReturnStmt(Expr::Call(None, String::from("call_func"), all_args)),
          ),
          vec!(
            Stmt::Expr(Expr::Call(None, String::from("push_error"), vec!(Expr::str_lit("Too many arguments")))),
          ),
        )
      );
    }

    // I'm only really using the builder for the sake of convenience
    // here. I know exactly what it does, and it should never generate
    // helpers. Thus, if it somehow does, that's an error in my code
    // and you should report it as a bug. :)
    let (stmts, decls) = builder.build();
    if decls.len() > 0 {
      panic!("Helper declarations in synthetic lambda! (This is a bug in GDLisp)");
    }
    decl::FnDecl {
      name: String::from("call_funcv"),
      args: ArgList::required(vec!(String::from("args"))),
      body: stmts,
    }

  }

  // ///// (function ...) and #'... syntax
  fn generate_lambda_class(&mut self,
                           specs: FnSpecs,
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
    let funcv = self.generate_lambda_vararg(specs);
    let mut constructor_body = Vec::new();
    for name in closed_vars.iter() {
      constructor_body.push(Compiler::assign_to_self(name.to_string(), name.to_string()));
    }
    let r: i32  = specs.required.try_into().unwrap();
    let o: i32  = specs.optional.try_into().unwrap();
    let x: bool = specs.rest;
    constructor_body.push(Compiler::assign_expr_to_self(String::from("__gdlisp_required"), Expr::from(r)));
    constructor_body.push(Compiler::assign_expr_to_self(String::from("__gdlisp_optional"), Expr::from(o)));
    constructor_body.push(Compiler::assign_expr_to_self(String::from("__gdlisp_rest"), Expr::from(x)));
    let constructor =
      decl::FnDecl {
        name: String::from("_init"),
        args: ArgList::required(closed_vars.iter().map(|x| (*x).to_owned()).collect()),
        body: constructor_body,
      };
    let mut class_body = vec!();
    for var in closed_vars {
      class_body.push(Decl::VarDecl(var.clone(), None));
    }
    class_body.append(&mut vec!(
      Decl::FnDecl(decl::Static::NonStatic, constructor),
      Decl::FnDecl(decl::Static::NonStatic, func),
      Decl::FnDecl(decl::Static::NonStatic, funcv),
    ));
    decl::ClassDecl {
      name: class_name,
      extends: decl::ClassExtends::Qualified(String::from("GDLisp"), String::from("Function")),
      body: class_body,
    }
  }

  fn assign_to_self(inst_var: String, local_var: String) -> Stmt {
    Compiler::assign_expr_to_self(inst_var, Expr::Var(local_var))
  }

  fn assign_expr_to_self(inst_var: String, expr: Expr) -> Stmt {
    let self_target = Box::new(Expr::Attribute(Box::new(Expr::Var(String::from("self"))), inst_var));
    let value = Box::new(expr);
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

  pub fn compile_decl(&mut self,
                      builder: &mut CodeBuilder,
                      table: &mut SymbolTable,
                      decl: &IRDecl)
                      -> Result<(), Error> {
    match decl {
      IRDecl::FnDecl(ir::decl::FnDecl { name, args, body }) => {
        let gd_name = names::lisp_to_gd(&name);
        let (arglist, gd_args) = args.clone().into_gd_arglist(&mut self.gen);
        let mut stmt_builder = StmtBuilder::new();
        table.with_local_vars(&mut gd_args.clone().into_iter(), |table| {
          self.compile_stmt(&mut stmt_builder, table, &stmt_wrapper::Return, body)
        })?;
        let gd_body = stmt_builder.build_into(builder);
        builder.add_decl(Decl::FnDecl(decl::Static::IsStatic, decl::FnDecl {
          name: gd_name,
          args: arglist,
          body: gd_body,
        }));
        Ok(())
      }
    }
  }

  fn bind_decl(table: &mut SymbolTable,
               decl: &IRDecl)
               -> Result<(), Error> {
    match decl {
      IRDecl::FnDecl(ir::decl::FnDecl { name, args, body: _ }) => {
        let func = function_call::FnCall::unqualified(
          function_call::FnSpecs::from(args.to_owned()),
          function_call::FnScope::Global,
          names::lisp_to_gd(name)
        );
        table.set_fn(name.clone(), func);
      }
    };
    Ok(())
  }

  pub fn compile_decls(&mut self,
                       builder: &mut CodeBuilder,
                       table: &SymbolTable,
                       decls: &Vec<IRDecl>)
                       -> Result<(), Error> {
    // Since we're going to be altering it a lot (and this function
    // should be getting called infrequently), it's going to be easier
    // to just copy the symbol table now, rather than try to track all
    // changes and undo them.
    let mut table = table.clone();
    for decl in decls {
      Compiler::bind_decl(&mut table, decl)?;
    }
    for decl in decls {
      self.compile_decl(builder, &mut table, decl)?;
    }
    Ok(())
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::decl::Decl;
  use crate::sxp::ast::{self, AST};
  use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};

  // TODO A lot more of this

  fn bind_helper_symbols(table: &mut SymbolTable) {
    // Binds a few helper names to the symbol table for the sake of
    // debugging.
    table.set_fn(String::from("foo1"), FnCall::unqualified(FnSpecs::new(1, 0, false), FnScope::Global, String::from("foo1")));
    table.set_fn(String::from("foo"), FnCall::unqualified(FnSpecs::new(0, 0, false), FnScope::Global, String::from("foo")));
    table.set_fn(String::from("bar"), FnCall::unqualified(FnSpecs::new(0, 0, false), FnScope::Global, String::from("bar")));
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

  fn compile_vararg(specs: FnSpecs) -> String {
    let mut compiler = Compiler::new(FreshNameGenerator::new(vec!()));
    let result = compiler.generate_lambda_vararg(specs);
    Decl::FnDecl(decl::Static::NonStatic, result).to_gd(0)
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
    let ast = ast::list(vec!(AST::Symbol(String::from("foo1")), AST::Int(10)));
    let expected = Stmt::ReturnStmt(Expr::Call(None, String::from("foo1"), vec!(Expr::from(10))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_int() {
    let ast = AST::Int(99);
    let expected = Stmt::ReturnStmt(Expr::from(99));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_vacuous() {
    let ast = ast::list(vec!(AST::Symbol(String::from("progn")), AST::Int(1), AST::Int(2)));
    let expected = vec!(Stmt::ReturnStmt(Expr::from(2)));
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

  #[test]
  fn test_lambda_vararg() {
    assert_eq!(compile_vararg(FnSpecs::new(0, 0, false)), "func call_funcv(args):\n    if args is GDLisp.NilClass:\n        return call_func()\n    else:\n        push_error(\"Too many arguments\")\n");
    assert_eq!(compile_vararg(FnSpecs::new(0, 0, true)), "func call_funcv(args):\n    return call_func(args)\n");
    assert_eq!(compile_vararg(FnSpecs::new(1, 0, true)), "func call_funcv(args):\n    var required_0 = null\n    if args is GDLisp.NilClass:\n        push_error(\"Not enough arguments\")\n    else:\n        required_0 = args.car\n        args = args.cdr\n    return call_func(required_0, args)\n");
    assert_eq!(compile_vararg(FnSpecs::new(0, 1, true)), "func call_funcv(args):\n    var optional_0 = null\n    if args is GDLisp.NilClass:\n        optional_0 = GDLisp.Nil\n    else:\n        optional_0 = args.car\n        args = args.cdr\n    return call_func(optional_0, args)\n");
  }

}
