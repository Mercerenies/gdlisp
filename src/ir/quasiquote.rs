
use crate::sxp::ast::{AST, ASTF};
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
  Quasiquote(&'a AST),
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

impl<'a> UnquotedValue<'a> {
  fn verbatim(arg: &'a AST) -> UnquotedValue<'a> {
    UnquotedValue::SimpleValue(arg)
  }
}

impl<'a> From<&'a AST> for UnquotedValue<'a> {
  fn from(arg: &'a AST) -> UnquotedValue<'a> {
    if let ASTF::Cons(car, cdr) = &arg.value {
      if let ASTF::Symbol(name) = &car.value {
        if let ASTF::Cons(cadr, cddr) = &cdr.value {
          if cddr.value == ASTF::Nil {
            if name == "quasiquote" {
              return UnquotedValue::Quasiquote(&*cadr);
            } else if name == "unquote" {
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
  quasiquote_indexed(icompiler, pipeline, arg, 0)
}

fn quasiquote_indexed(icompiler: &mut IncCompiler,
                      pipeline: &mut Pipeline,
                      arg: &AST,
                      depth: u32)
                      -> Result<Expr, Error> {
  quasiquote_spliced(icompiler, pipeline, arg, depth).and_then(|qq| {
    qq.into_single(arg).map_err(Error::from)
  })
}

fn quasiquote_spliced(icompiler: &mut IncCompiler,
                      pipeline: &mut Pipeline,
                      arg: &AST,
                      depth: u32)
                      -> Result<QQSpliced, Error> {
  let unquoted_value = UnquotedValue::from(arg);

  // Deal with nesting issues
  let (unquoted_value, depth) = match unquoted_value {
    UnquotedValue::SimpleValue(_) => {
      (UnquotedValue::verbatim(arg), depth)
    }
    UnquotedValue::Quasiquote(_) => {
      (UnquotedValue::verbatim(arg), depth + 1)
    }
    UnquotedValue::Unquote(_) | UnquotedValue::UnquoteSpliced(_) => {
      if depth > 0 {
        // We're inside a nested quasiquote, so do NOT unquote the value.
        (UnquotedValue::verbatim(arg), depth - 1)
      } else {
        (unquoted_value, depth)
      }
    }
  };

  match unquoted_value {
    UnquotedValue::Unquote(arg) => {
      icompiler.compile_expr(pipeline, arg).map(QQSpliced::Single)
    }
    UnquotedValue::UnquoteSpliced(arg) => {
      icompiler.compile_expr(pipeline, arg).map(QQSpliced::Several)
    }
    UnquotedValue::Quasiquote(_) => {
      // The above nesting handler should always eliminate
      // UnquotedValue::Quasiquote and convert it into
      // UnquotedValue::SimpleValue, so this should never happen.
      panic!("Internal error in quasiquote_spliced (impossible UnquotedValue::Quasiquote branch was reached)")
    }
    UnquotedValue::SimpleValue(arg) => {
      let body = match &arg.value {
        ASTF::Nil => {
          Expr::Literal(Literal::Nil)
        }
        ASTF::Int(n) => {
          Expr::Literal(Literal::Int(*n))
        }
        ASTF::Bool(b) => {
          Expr::Literal(Literal::Bool(*b))
        }
        ASTF::Float(f) => {
          Expr::Literal(Literal::Float(*f))
        }
        ASTF::String(s) => {
          Expr::Literal(Literal::String(s.to_owned()))
        }
        ASTF::Symbol(s) => {
          Expr::Literal(Literal::Symbol(s.to_owned()))
        }
        ASTF::Cons(car, cdr) => {
          let car = quasiquote_spliced(icompiler, pipeline, car, depth)?;
          let cdr = quasiquote_indexed(icompiler, pipeline, cdr, depth)?;
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
        ASTF::Array(v) => {
          let v1 = v.iter().map(|x| quasiquote_spliced(icompiler, pipeline, x, depth)).collect::<Result<Vec<_>, _>>()?;

          let mut acc: Vec<Expr> = vec!();
          let mut current_vec: Vec<Expr> = vec!();
          for value in v1 {
            match value {
              QQSpliced::Single(x) => {
                current_vec.push(x)
              }
              QQSpliced::Several(x) => {
                let x = Expr::Call(String::from("sys/qq-smart-array"), vec!(x));
                if !current_vec.is_empty() {
                  acc.push(Expr::Array(current_vec));
                  current_vec = vec!();
                }
                acc.push(x);
              }
            }
          }
          if !current_vec.is_empty() {
            acc.push(Expr::Array(current_vec));
          }
          Expr::Call(String::from("+"), acc)
        }
        ASTF::Dictionary(v) => {
          // TODO Does unquote-spliced make sense in this context?
          let v1 = v.iter().map(|(k, v)| Ok((quasiquote_indexed(icompiler, pipeline, k, depth)?, quasiquote_indexed(icompiler, pipeline, v, depth)?))).collect::<Result<Vec<_>, Error>>()?;
          Expr::Dictionary(v1)
        }
      };
      Ok(QQSpliced::Single(body))
    }
  }
}
