
use crate::sxp::ast::AST;
use crate::ir::incremental::IncCompiler;
use crate::compile::error::{Error as GDError};
use super::expr::Expr;
use super::literal::Literal;
use crate::pipeline::error::Error;
use crate::pipeline::Pipeline;

#[derive(Clone, Debug, PartialEq, Eq)]
enum QQSpliced {
  Single(Expr),
  Several(Expr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UnquotedValue<'a> {
  SimpleValue(&'a AST),
  Unquote(&'a AST),
  UnquoteSpliced(&'a AST),
}

impl QQSpliced {
  fn into_single(self, ast: &AST) -> Result<Expr, GDError> {
    match self {
      QQSpliced::Single(e) => Ok(e),
      QQSpliced::Several(_) => Err(GDError::BadUnquoteSpliced(ast.clone())),
    }
  }
}

impl<'a> From<&'a AST> for UnquotedValue<'a> {
  fn from(arg: &'a AST) -> UnquotedValue<'a> {
    if let AST::Cons(car, cdr) = arg {
      if let AST::Symbol(name) = &**car {
        if let AST::Cons(cadr, cddr) = &**cdr {
          if **cddr == AST::Nil {
            if name == "unquote" {
              return UnquotedValue::Unquote(&*cadr);
            } else if name == "unquote-spliced" {
              return UnquotedValue::UnquoteSpliced(&*cadr);
            }
          }
        }
      }
    }
    UnquotedValue::SimpleValue(arg)
  }
}

pub fn quasiquote(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  arg: &AST)
                  -> Result<Expr, Error> {
  quasiquote_spliced(icompiler, pipeline, arg).and_then(|qq| {
    qq.into_single(arg).map_err(Error::from)
  })
}

fn quasiquote_spliced(icompiler: &mut IncCompiler,
                      pipeline: &mut Pipeline,
                      arg: &AST)
                      -> Result<QQSpliced, Error> {
  match UnquotedValue::from(arg) {
    UnquotedValue::Unquote(arg) => {
      icompiler.compile_expr(pipeline, arg).map(QQSpliced::Single)
    }
    UnquotedValue::UnquoteSpliced(arg) => {
      icompiler.compile_expr(pipeline, arg).map(QQSpliced::Several)
    }
    UnquotedValue::SimpleValue(arg) => {
      let body = match arg {
        AST::Nil => {
          Expr::Literal(Literal::Nil)
        }
        AST::Int(n) => {
          Expr::Literal(Literal::Int(*n))
        }
        AST::Bool(b) => {
          Expr::Literal(Literal::Bool(*b))
        }
        AST::Float(f) => {
          Expr::Literal(Literal::Float(*f))
        }
        AST::String(s) => {
          Expr::Literal(Literal::String(s.to_owned()))
        }
        AST::Symbol(s) => {
          Expr::Literal(Literal::Symbol(s.to_owned()))
        }
        AST::Cons(car, cdr) => {
          let car = quasiquote_spliced(icompiler, pipeline, car)?;
          let cdr = quasiquote(icompiler, pipeline, cdr)?;
          match car {
            QQSpliced::Single(car) => {
              Expr::Call(String::from("cons"), vec!(car, cdr))
            }
            QQSpliced::Several(car) => {
              let converted_car = Expr::Call(String::from("sys/qq-smart-list"), vec!(car));
              Expr::Call(String::from("append"), vec!(converted_car, cdr))
            }
          }
        }
        AST::Array(v) => {
          let v1 = v.iter().map(|x| quasiquote_spliced(icompiler, pipeline, x)).collect::<Result<Vec<_>, _>>()?;

          let mut acc: Vec<Expr> = vec!();
          let mut current_vec: Vec<Expr> = vec!();
          for value in v1 {
            match value {
              QQSpliced::Single(x) => {
                current_vec.push(x)
              }
              QQSpliced::Several(x) => {
                let x = Expr::Call(String::from("sys/qq-smart-array"), vec!(x));
                if current_vec.len() > 0 {
                  acc.push(Expr::Array(current_vec));
                  current_vec = vec!();
                }
                acc.push(x);
              }
            }
          }
          if current_vec.len() > 0 {
            acc.push(Expr::Array(current_vec));
          }
          Expr::Call(String::from("+"), acc)
        }
        AST::Dictionary(v) => {
          // TODO Does unquote-spliced make sense in this context?
          let v1 = v.iter().map(|(k, v)| Ok((quasiquote(icompiler, pipeline, k)?, quasiquote(icompiler, pipeline, v)?))).collect::<Result<Vec<_>, Error>>()?;
          Expr::Dictionary(v1)
        }
        AST::Vector2(x, y) => {
          let x = quasiquote(icompiler, pipeline, x)?;
          let y = quasiquote(icompiler, pipeline, y)?;
          Expr::Vector2(Box::new(x), Box::new(y))
        }
        AST::Vector3(x, y, z) => {
          let x = quasiquote(icompiler, pipeline, x)?;
          let y = quasiquote(icompiler, pipeline, y)?;
          let z = quasiquote(icompiler, pipeline, z)?;
          Expr::Vector3(Box::new(x), Box::new(y), Box::new(z))
        }
      };
      Ok(QQSpliced::Single(body))
    }
  }
}
