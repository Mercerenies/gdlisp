
use crate::sxp::ast::AST;
use crate::ir::incremental::IncCompiler;
use super::expr::Expr;
use super::literal::Literal;
use crate::pipeline::error::Error;
use crate::pipeline::Pipeline;

pub fn quasiquote(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  arg: &AST)
                  -> Result<Expr, Error> {
  if let Some(ast) = check_for_unquote(arg) {
    icompiler.compile_expr(pipeline, ast)
  } else {
    match arg {
      AST::Nil => {
        Ok(Expr::Literal(Literal::Nil))
      }
      AST::Int(n) => {
        Ok(Expr::Literal(Literal::Int(*n)))
      }
      AST::Bool(b) => {
        Ok(Expr::Literal(Literal::Bool(*b)))
      }
      AST::Float(f) => {
        Ok(Expr::Literal(Literal::Float(*f)))
      }
      AST::String(s) => {
        Ok(Expr::Literal(Literal::String(s.to_owned())))
      }
      AST::Symbol(s) => {
        Ok(Expr::Literal(Literal::Symbol(s.to_owned())))
      }
      AST::Cons(car, cdr) => {
        Ok(Expr::Call(String::from("cons"), vec!(quasiquote(icompiler, pipeline, car)?, quasiquote(icompiler, pipeline, cdr)?)))
      }
      AST::Array(v) => {
        let v1 = v.iter().map(|x| quasiquote(icompiler, pipeline, x)).collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::Array(v1))
      }
      AST::Dictionary(v) => {
        let v1 = v.iter().map(|(k, v)| Ok((quasiquote(icompiler, pipeline, k)?, quasiquote(icompiler, pipeline, v)?))).collect::<Result<Vec<_>, Error>>()?;
        Ok(Expr::Dictionary(v1))
      }
      AST::Vector2(x, y) => {
        let x = quasiquote(icompiler, pipeline, x)?;
        let y = quasiquote(icompiler, pipeline, y)?;
        Ok(Expr::Vector2(Box::new(x), Box::new(y)))
      }
      AST::Vector3(x, y, z) => {
        let x = quasiquote(icompiler, pipeline, x)?;
        let y = quasiquote(icompiler, pipeline, y)?;
        let z = quasiquote(icompiler, pipeline, z)?;
        Ok(Expr::Vector3(Box::new(x), Box::new(y), Box::new(z)))
      }
    }
  }
}

fn check_for_unquote(arg: &AST) -> Option<&AST> {
  if let AST::Cons(car, cdr) = arg {
    if let AST::Symbol(name) = &**car {
      if name == "unquote" {
        if let AST::Cons(cadr, cddr) = &**cdr {
          if **cddr == AST::Nil {
            return Some(&*cadr);
          }
        }
      }
    }
  }
  None
}
