
pub mod names;
pub mod body;
pub mod error;
pub mod stmt_wrapper;
pub mod symbol_table;
pub mod special_form;
pub mod stateful;

use body::builder::{CodeBuilder, StmtBuilder, HasDecls};
use names::fresh::FreshNameGenerator;
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::op;
use crate::gdscript::library;
use error::Error;
use stmt_wrapper::StmtWrapper;
use symbol_table::{HasSymbolTable, SymbolTable, LocalVar};
use symbol_table::function_call;
use symbol_table::call_magic::DefaultCall;
use crate::ir;
use crate::ir::expr::FuncRefTarget;
use special_form::lambda;
use special_form::flet;
use stateful::{StExpr, NeedsResult, SideEffects};

type IRDecl = ir::decl::Decl;
type IRExpr = ir::expr::Expr;
type IRArgList = ir::arglist::ArgList;
type IRLiteral = ir::literal::Literal;

pub struct Compiler<'a> {
  gen: FreshNameGenerator<'a>,
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
        self.compile_stmt(builder, table, &stmt_wrapper::Vacuous, x)?;
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
          StExpr(var.expr(), SideEffects::from(var.access_type))
        })
      }
      IRExpr::Literal(lit) => {
        match lit {
          IRLiteral::Nil => Ok(Compiler::nil_expr()),
          IRLiteral::Int(n) => Ok(StExpr(Expr::from(*n), SideEffects::None)),
          IRLiteral::Float(f) => Ok(StExpr(Expr::from(*f), SideEffects::None)),
          IRLiteral::Bool(b) => Ok(StExpr(Expr::from(*b), SideEffects::None)),
        }
      }
      IRExpr::Progn(body) => {
        let body: Vec<_> = body.iter().collect();
        self.compile_stmts(builder, table, &body[..], needs_result)
      }
      IRExpr::IfStmt(c, t, f) => {
        special_form::compile_if_stmt(self, builder, table, c, t, f, needs_result)
      }
      IRExpr::CondStmt(clauses) => {
        special_form::compile_cond_stmt(self, builder, table, clauses, needs_result)
      }
      IRExpr::WhileStmt(cond, body) => {
        special_form::compile_while_stmt(self, builder, table, cond, body, needs_result)
      }
      IRExpr::Call(f, args) => {
        let (fcall, call_magic) = match table.get_fn(f) {
          None => return Err(Error::NoSuchFn(f.clone())),
          Some((p, m)) => (p.clone(), dyn_clone::clone_box(m))
        };
        // Call magic is used to implement some commonly used wrappers
        // for simple GDScript operations.
        let args = args.iter()
                       .map(|x| self.compile_expr(builder, table, x, NeedsResult::Yes))
                       .collect::<Result<Vec<_>, _>>()?;
        Ok(StExpr(fcall.into_expr_with_magic(&*call_magic, self, builder, table, args)?, SideEffects::ModifiesState))
      }
      IRExpr::Let(clauses, body) => {
        let closure_vars = body.get_locals();
        let var_names = clauses.iter().map::<Result<(String, String), Error>, _>(|clause| {
          let (ast_name, expr) = clause;
          let ast_name = ast_name.to_owned();
          let result_value = self.compile_expr(builder, table, &expr, NeedsResult::Yes)?.0;
          let result_value =
            if closure_vars.get(&ast_name).requires_cell() {
              library::construct_cell(result_value)
            } else {
              result_value
            };
          let gd_name = self.declare_var(builder, &ast_name, Some(result_value));
          Ok((ast_name, gd_name))
        }).collect::<Result<Vec<_>, _>>()?;
        table.with_local_vars(&mut var_names.into_iter().map(|x| (x.0.clone(), LocalVar::new(x.1, closure_vars.get(&x.0)))), |table| {
          self.compile_expr(builder, table, body, needs_result)
        })
      }
      IRExpr::FLet(clauses, body) => {
        flet::compile_flet(self, builder, table, clauses, body, needs_result)
      }
      IRExpr::Labels(clauses, body) => {
        flet::compile_labels(self, builder, table, clauses, body, needs_result)
      }
      IRExpr::Lambda(args, body) => {
        lambda::compile_lambda_stmt(self, builder, table, args, body)
      }
      IRExpr::FuncRef(name) => {
        match name {
          FuncRefTarget::SimpleName(name) => {
            let func = table.get_fn(name).ok_or_else(|| Error::NoSuchFn(name.clone()))?.0.clone();
            lambda::compile_function_ref(self, builder, table, func)
          }
        }
      }
      IRExpr::Assign(name, expr) => {
        let var = table.get_var(name).ok_or_else(|| Error::NoSuchVar(name.clone()))?.to_owned();
        self.compile_stmt(builder, table, &stmt_wrapper::AssignToExpr(var.expr()), expr)?;
        Ok(StExpr(var.expr(), SideEffects::from(var.access_type)))
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

  pub fn nil_expr() -> StExpr {
    StExpr(library::nil(), SideEffects::None)
  }

  pub fn name_generator(&mut self) -> &mut FreshNameGenerator<'a> {
    &mut self.gen
  }

  pub fn declare_var(&mut self, builder: &mut StmtBuilder, prefix: &str, value: Option<Expr>)
                     -> String {
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
        let function = self.declare_function(builder, table, gd_name, args.clone(), body)?;
        builder.add_decl(Decl::FnDecl(decl::Static::IsStatic, function));
        Ok(())
      }
    }
  }

  pub fn declare_function(&mut self,
                          builder: &mut impl HasDecls,
                          table: &mut SymbolTable,
                          gd_name: String,
                          args: IRArgList,
                          body: &IRExpr)
                          -> Result<decl::FnDecl, Error> {
    let local_vars = body.get_locals();
    let (arglist, gd_args) = args.into_gd_arglist(&mut self.gen);
    let mut stmt_builder = StmtBuilder::new();
    for arg in &gd_args {
      if local_vars.get(&arg.0).requires_cell() {
            // Special behavior to wrap the argument in a cell.
        stmt_builder.append(Stmt::Assign(Box::new(Expr::var(&arg.1)),
                                         op::AssignOp::Eq,
                                         Box::new(library::construct_cell(Expr::var(&arg.1)))));
      }
    }
    table.with_local_vars(&mut gd_args.into_iter().map(|x| (x.0.to_owned(), LocalVar::new(x.1, local_vars.get(&x.0)))), |table| {
      self.compile_stmt(&mut stmt_builder, table, &stmt_wrapper::Return, body)
    })?;
    Ok(decl::FnDecl {
      name: gd_name,
      args: arglist,
      body: stmt_builder.build_into(builder),
    })
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
        table.set_fn(name.clone(), func, Box::new(DefaultCall));
      }
    };
    Ok(())
  }

  pub fn compile_decls(&mut self,
                       builder: &mut CodeBuilder,
                       table: &SymbolTable,
                       decls: &[IRDecl])
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
    table.set_fn(String::from("foo1"), FnCall::unqualified(FnSpecs::new(1, 0, false), FnScope::Global, String::from("foo1")), Box::new(DefaultCall));
    table.set_fn(String::from("foo"), FnCall::unqualified(FnSpecs::new(0, 0, false), FnScope::Global, String::from("foo")), Box::new(DefaultCall));
    table.set_fn(String::from("bar"), FnCall::unqualified(FnSpecs::new(0, 0, false), FnScope::Global, String::from("bar")), Box::new(DefaultCall));
    table.set_var(String::from("foobar"), LocalVar::read(String::from("foobar")));
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
  fn compile_bool_t() {
    let ast = AST::Bool(true);
    let expected = Stmt::ReturnStmt(Expr::from(true));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_bool_f() {
    let ast = AST::Bool(false);
    let expected = Stmt::ReturnStmt(Expr::from(false));
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

}
