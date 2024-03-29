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

use crate::ir::expr::{self, Expr, ExprF};
use crate::ir::decl::{self, Decl, DeclF};
use crate::util::extract_err;

// Post-order traversal

struct ExprWalker<'a, E> {
  imp: Box<WalkFn<'a, E>>,
}

type WalkFn<'a, E> = dyn (FnMut(&Expr) -> Result<Expr, E>) + 'a;

impl<'a, E> ExprWalker<'a, E> {

  fn new(function: impl FnMut(&Expr) -> Result<Expr, E> + 'a) -> ExprWalker<'a, E> {
    ExprWalker { imp: Box::new(function) }
  }

  fn walk_exprs(&mut self, exprs: &[Expr]) -> Result<Vec<Expr>, E> {
    exprs.iter().map(|e| {
      self.walk_expr(e)
    }).collect()
  }

  fn walk_expr(&mut self, expr: &Expr) -> Result<Expr, E> {
    let new_expr = match &expr.value {
      ExprF::BareName(_) => {
        expr.value.clone()
      }
      ExprF::Literal(_) => {
        expr.value.clone()
      }
      ExprF::Quote(_) => {
        expr.value.clone()
      }
      ExprF::SpecialRef(_) => {
        expr.value.clone()
      }
      ExprF::ContextualFilename(_) => {
        expr.value.clone()
      }
      ExprF::Progn(body) => {
        ExprF::Progn(self.walk_exprs(body)?)
      }
      ExprF::CondStmt(options) => {
        ExprF::CondStmt(options.iter().map(|(cond, body)| {
          Ok((self.walk_expr(cond)?, body.as_ref().map(|b| self.walk_expr(b)).transpose()?))
        }).collect::<Result<Vec<_>, _>>()?)
      }
      ExprF::WhileStmt(cond, body) => {
        ExprF::WhileStmt(
          Box::new(self.walk_expr(cond)?),
          Box::new(self.walk_expr(body)?),
        )
      }
      ExprF::ForStmt(name, iter, body) => {
        ExprF::ForStmt(
          name.clone(),
          Box::new(self.walk_expr(iter)?),
          Box::new(self.walk_expr(body)?),
        )
      }
      ExprF::Call(object, name, body) => {
        let object = match object {
          expr::CallTarget::Scoped => expr::CallTarget::Scoped,
          expr::CallTarget::Super => expr::CallTarget::Super,
          expr::CallTarget::Atomic => expr::CallTarget::Atomic,
          expr::CallTarget::Object(inner) => expr::CallTarget::Object(Box::new(self.walk_expr(inner)?)),
        };
        ExprF::Call(
          object,
          name.clone(),
          self.walk_exprs(body)?,
        )
      }
      ExprF::Let(clauses, body) => {
        let clauses = clauses.iter().map(|clause| {
          Ok(expr::LocalVarClause {
            name: clause.name.clone(),
            value: self.walk_expr(&clause.value)?,
          })
        }).collect::<Result<Vec<_>, _>>()?;
        ExprF::Let(
          clauses,
          Box::new(self.walk_expr(body)?),
        )
      }
      ExprF::FunctionLet(binding_type, clauses, body) => {
        let clauses = clauses.iter().map(|clause| {
          Ok(expr::LocalFnClause {
            name: clause.name.clone(),
            args: clause.args.clone(),
            body: self.walk_expr(&clause.body)?,
          })
        }).collect::<Result<Vec<_>, _>>()?;
        ExprF::FunctionLet(
          *binding_type,
          clauses,
          Box::new(self.walk_expr(body)?),
        )
      }
      ExprF::Lambda(args, body) => {
        ExprF::Lambda(
          args.clone(),
          Box::new(self.walk_expr(body)?),
        )
      }
      ExprF::FuncRef(expr::FuncRefTarget::SimpleName(s)) => {
        ExprF::FuncRef(
          expr::FuncRefTarget::SimpleName(s.clone()),
        )
      }
      ExprF::Assign(target, rhs) => {
        let target = match target {
          expr::AssignTarget::Variable(_, _) => {
            target.clone()
          }
          expr::AssignTarget::InstanceField(pos, inner, name) => {
            expr::AssignTarget::InstanceField(
              *pos,
              Box::new(self.walk_expr(inner)?),
              name.clone(),
            )
          }
        };
        ExprF::Assign(
          target,
          Box::new(self.walk_expr(rhs)?),
        )
      }
      ExprF::FieldAccess(lhs, name) => {
        ExprF::FieldAccess(
          Box::new(self.walk_expr(lhs)?),
          name.clone(),
        )
      }
      ExprF::LambdaClass(cls) => {
        ExprF::LambdaClass(
          Box::new(self.walk_lambda_class(cls)?),
        )
      }
      ExprF::Yield(None) => {
        ExprF::Yield(None)
      }
      ExprF::Yield(Some((a, b))) => {
        ExprF::Yield(Some((
          Box::new(self.walk_expr(a)?),
          Box::new(self.walk_expr(b)?),
        )))
      }
      ExprF::Assert(cond, message) => {
        let cond = self.walk_expr(cond)?;
        let message = message.as_ref().map(|x| self.walk_expr(x)).transpose()?;
        ExprF::Assert(Box::new(cond), message.map(Box::new))
      }
      ExprF::Return(v) => {
        ExprF::Return(Box::new(self.walk_expr(v)?))
      }
      ExprF::Break => {
        ExprF::Break
      }
      ExprF::Continue => {
        ExprF::Continue
      }
      ExprF::Split(name, body) => {
        ExprF::Split(
          name.clone(),
          Box::new(self.walk_expr(body)?),
        )
      }
      ExprF::Preload(name) => {
        ExprF::Preload(name.to_owned())
      }
    };
    let new_expr = Expr::new(new_expr, expr.pos);
    (self.imp)(&new_expr)
  }

  fn walk_lambda_class(&mut self, cls: &expr::LambdaClass) -> Result<expr::LambdaClass, E> {
    // TODO Once we can walk declarations, unify parts of this with
    // that implementation.
    let extends = cls.extends.clone();
    let args = self.walk_exprs(&cls.args)?;
    let constructor = cls.constructor.as_ref().map(|c| {
      let super_call = decl::SuperCall { call: self.walk_exprs(&c.super_call.call)?, pos: c.super_call.pos };
      Ok(decl::ConstructorDecl {
        args: c.args.clone(),
        super_call: super_call,
        body: self.walk_expr(&c.body)?,
      })
    }).transpose()?;
    let decls = cls.decls.iter().map(|d| {
      // TODO Technically ClassConstDecl and ClassVarDecl contain
      // expressions. We should walk those too.
      let new_decl = match &d.value {
        decl::ClassInnerDeclF::ClassSignalDecl(_) => d.value.clone(),
        decl::ClassInnerDeclF::ClassConstDecl(_) => d.value.clone(),
        decl::ClassInnerDeclF::ClassVarDecl(_) => d.value.clone(),
        decl::ClassInnerDeclF::ClassFnDecl(inner) => {
          decl::ClassInnerDeclF::ClassFnDecl(decl::ClassFnDecl {
            is_static: inner.is_static,
            is_nullargs: inner.is_nullargs,
            name: inner.name.clone(),
            args: inner.args.clone(),
            body: self.walk_expr(&inner.body)?,
          })
        }
      };
      Ok(decl::ClassInnerDecl { value: new_decl, pos: d.pos })
    }).collect::<Result<Vec<_>, _>>()?;
    Ok(expr::LambdaClass { extends, args, constructor, decls })
  }

}

pub fn walk_expr<'a, E>(expr: &Expr, walker: impl FnMut(&Expr) -> Result<Expr, E> + 'a)
                        -> Result<Expr, E> {
  let mut walker = ExprWalker::new(walker);
  walker.walk_expr(expr)
}

pub fn walk_expr_ok<'a>(expr: &Expr, mut walker: impl FnMut(&Expr) -> Expr + 'a)
                        -> Expr {
  let result = walk_expr(expr, move |x| Ok(walker(x)));
  extract_err(result)
}

pub fn walk_exprs_in_decl<'a, E>(decl: &Decl, walker: impl FnMut(&Expr) -> Result<Expr, E> + 'a)
                                 -> Result<Decl, E> {
  let new_decl = match &decl.value {
    DeclF::ConstDecl(d) => {
      DeclF::ConstDecl(decl::ConstDecl {
        visibility: d.visibility,
        name: d.name.clone(),
        value: walk_expr(&d.value, walker)?,
      })
    }
    DeclF::FnDecl(d) => {
      DeclF::FnDecl(decl::FnDecl {
        visibility: d.visibility,
        call_magic: d.call_magic.clone(),
        name: d.name.clone(),
        args: d.args.clone(),
        body: walk_expr(&d.body, walker)?,
      })
    }
    DeclF::MacroDecl(d) => {
      DeclF::MacroDecl(decl::MacroDecl {
        visibility: d.visibility,
        name: d.name.clone(),
        args: d.args.clone(),
        body: walk_expr(&d.body, walker)?,
      })
    }
    DeclF::SymbolMacroDecl(d) => {
      DeclF::SymbolMacroDecl(decl::SymbolMacroDecl {
        visibility: d.visibility,
        name: d.name.clone(),
        body: walk_expr(&d.body, walker)?,
      })
    }
    DeclF::EnumDecl(d) => {
      let mut walker = walker;
      let clauses = d.clauses.iter().map(|(s, e)| {
        Ok((s.clone(), e.as_ref().map(|e1| walk_expr(e1, |x| walker(x))).transpose()?))
      }).collect::<Result<Vec<_>, _>>()?;
      DeclF::EnumDecl(decl::EnumDecl {
        visibility: d.visibility,
        name: d.name.clone(),
        clauses: clauses,
      })
    }
    DeclF::DeclareDecl(d) => {
      DeclF::DeclareDecl(d.clone())
    }
    DeclF::ClassDecl(d) => {
      let mut walker = walker;
      let mut walker = ExprWalker::new(|x| walker(x));
      let constructor = d.constructor.as_ref().map(|c| {
        let super_call = decl::SuperCall { call: walker.walk_exprs(&c.super_call.call)?, pos: c.super_call.pos };
        Ok(decl::ConstructorDecl {
          args: c.args.clone(),
          super_call: super_call,
          body: walker.walk_expr(&c.body)?,
        })
      }).transpose()?;
      let decls = d.decls.iter().map(|d| {
        // TODO Technically ClassConstDecl and ClassVarDecl contain
        // expressions. We should walk those too.
        let new_decl = match &d.value {
          decl::ClassInnerDeclF::ClassSignalDecl(_) => d.value.clone(),
          decl::ClassInnerDeclF::ClassConstDecl(_) => d.value.clone(),
          decl::ClassInnerDeclF::ClassVarDecl(_) => d.value.clone(),
          decl::ClassInnerDeclF::ClassFnDecl(inner) => {
            decl::ClassInnerDeclF::ClassFnDecl(decl::ClassFnDecl {
              is_static: inner.is_static,
              is_nullargs: inner.is_nullargs,
              name: inner.name.clone(),
              args: inner.args.clone(),
              body: walker.walk_expr(&inner.body)?,
            })
          }
        };
        Ok(decl::ClassInnerDecl { value: new_decl, pos: d.pos })
      }).collect::<Result<Vec<_>, _>>()?;
      DeclF::ClassDecl(decl::ClassDecl {
        visibility: d.visibility,
        name: d.name.clone(),
        extends: d.extends.clone(),
        main_class: d.main_class,
        constructor: constructor,
        decls: decls,
      })
    }
  };
  Ok(Decl::new(new_decl, decl.pos))
}

pub fn walk_exprs_in_toplevel<'a, E>(decl: &decl::TopLevel, mut walker: impl FnMut(&Expr) -> Result<Expr, E> + 'a)
                                     -> Result<decl::TopLevel, E> {
  Ok(decl::TopLevel {
    imports: decl.imports.clone(),
    decls: decl.decls.iter().map(|d| walk_exprs_in_decl(d, |x| walker(x))).collect::<Result<Vec<_>, _>>()?,
    minimalist_flag: decl.minimalist_flag,
  })
}

pub fn walk_exprs_in_toplevel_ok<'a>(decl: &decl::TopLevel, mut walker: impl FnMut(&Expr) -> Expr + 'a)
                                     -> decl::TopLevel {
  let result = walk_exprs_in_toplevel(decl, move |x| Ok(walker(x)));
  extract_err(result)
}

// TODO Test me :)
