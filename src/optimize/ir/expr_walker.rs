
use crate::ir::expr::{self, Expr, ExprF};
use crate::ir::decl;
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
    exprs.into_iter().map(|e| {
      self.walk_expr(e)
    }).collect()
  }

  fn walk_expr(&mut self, expr: &Expr) -> Result<Expr, E> {
    let new_expr = match &expr.value {
      ExprF::LocalVar(_) => {
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
      ExprF::AtomicName(_) => {
        expr.value.clone()
      }
      ExprF::Progn(body) => {
        ExprF::Progn(self.walk_exprs(body)?)
      }
      ExprF::CondStmt(options) => {
        ExprF::CondStmt(options.into_iter().map(|(cond, body)| {
          Ok((self.walk_expr(cond)?, body.as_ref().map(|b| self.walk_expr(b)).transpose()?))
        }).collect::<Result<Vec<_>, _>>()?)
      }
      ExprF::WhileStmt(cond, body) => {
        ExprF::WhileStmt(
          Box::new(self.walk_expr(&*cond)?),
          Box::new(self.walk_expr(&*body)?),
        )
      }
      ExprF::ForStmt(name, iter, body) => {
        ExprF::ForStmt(
          name.clone(),
          Box::new(self.walk_expr(&*iter)?),
          Box::new(self.walk_expr(&*body)?),
        )
      }
      ExprF::Call(name, body) => {
        ExprF::Call(
          name.clone(),
          self.walk_exprs(body)?,
        )
      }
      ExprF::Let(clauses, body) => {
        let clauses = clauses.into_iter().map(|clause| {
          Ok(expr::LocalVarClause {
            name: clause.name.clone(),
            value: self.walk_expr(&clause.value)?,
          })
        }).collect::<Result<Vec<_>, _>>()?;
        ExprF::Let(
          clauses,
          Box::new(self.walk_expr(&*body)?),
        )
      }
      ExprF::FLet(clauses, body) => {
        let clauses = clauses.into_iter().map(|clause| {
          Ok(expr::LocalFnClause {
            name: clause.name.clone(),
            args: clause.args.clone(),
            body: self.walk_expr(&clause.body)?,
          })
        }).collect::<Result<Vec<_>, _>>()?;
        ExprF::FLet(
          clauses,
          Box::new(self.walk_expr(&*body)?),
        )
      }
      ExprF::Labels(clauses, body) => {
        let clauses = clauses.into_iter().map(|clause| {
          Ok(expr::LocalFnClause {
            name: clause.name.clone(),
            args: clause.args.clone(),
            body: self.walk_expr(&clause.body)?,
          })
        }).collect::<Result<Vec<_>, _>>()?;
        ExprF::Labels(
          clauses,
          Box::new(self.walk_expr(&*body)?),
        )
      }
      ExprF::Lambda(args, body) => {
        ExprF::Lambda(
          args.clone(),
          Box::new(self.walk_expr(&*body)?),
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
              Box::new(self.walk_expr(&*inner)?),
              name.clone(),
            )
          }
        };
        ExprF::Assign(
          target,
          Box::new(self.walk_expr(&*rhs)?),
        )
      }
      ExprF::Array(body) => {
        ExprF::Array(self.walk_exprs(body)?)
      }
      ExprF::Dictionary(options) => {
        ExprF::Dictionary(options.into_iter().map(|(k, v)| {
          Ok((self.walk_expr(k)?, self.walk_expr(v)?))
        }).collect::<Result<Vec<_>, _>>()?)
      }
      ExprF::FieldAccess(lhs, name) => {
        ExprF::FieldAccess(
          Box::new(self.walk_expr(&*lhs)?),
          name.clone(),
        )
      }
      ExprF::MethodCall(lhs, name, args) => {
        ExprF::MethodCall(
          Box::new(self.walk_expr(&*lhs)?),
          name.clone(),
          self.walk_exprs(args)?,
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
      ExprF::Return(v) => {
        ExprF::Return(Box::new(self.walk_expr(v)?))
      }
      ExprF::AtomicCall(name, body) => {
        ExprF::AtomicCall(
          name.clone(),
          self.walk_exprs(body)?,
        )
      }
      ExprF::Split(name, body) => {
        ExprF::Split(
          name.clone(),
          Box::new(self.walk_expr(&*body)?),
        )
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
      Ok(decl::ConstructorDecl {
        args: c.args.clone(),
        super_call: self.walk_exprs(&c.super_call)?,
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

// TODO Test me :)
