
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use super::expr::{Expr, FuncRefTarget, AssignTarget, LambdaClass};
use super::decl::{self, Decl};
use super::arglist::ArgList;
use super::quasiquote::quasiquote;
use crate::pipeline::error::Error;
use crate::compile::error::{Error as GDError};
use crate::ir::incremental::IncCompiler;
use crate::ir::identifier::{Id, Namespace};
use crate::pipeline::Pipeline;

use std::convert::{TryFrom, TryInto};

pub fn dispatch_form(icompiler: &mut IncCompiler,
                     pipeline: &mut Pipeline,
                     head: &str,
                     tail: &[&AST])
                     -> Result<Option<Expr>, Error> {
  match head {
    "progn" => progn_form(icompiler, pipeline, tail).map(Some),
    "cond" => cond_form(icompiler, pipeline, tail).map(Some),
    "while" => while_form(icompiler, pipeline, tail).map(Some),
    "for" => for_form(icompiler, pipeline, tail).map(Some),
    "let" => let_form(icompiler, pipeline, tail).map(Some),
    "flet" => flet_form(icompiler, pipeline, tail, Expr::FLet).map(Some),
    "labels" => flet_form(icompiler, pipeline, tail, Expr::Labels).map(Some),
    "lambda" => lambda_form(icompiler, pipeline, tail).map(Some),
    "function" => function_form(tail).map(Some),
    "setq" => assign_form(icompiler, pipeline, tail).map(Some),
    "quote" => quote_form(tail).map(Some),
    "quasiquote" => quasiquote_form(icompiler, pipeline, tail).map(Some),
    "unquote" => Err(Error::from(GDError::UnquoteOutsideQuasiquote)),
    "access-slot" => access_slot_form(icompiler, pipeline, tail).map(Some),
    "new" => new_form(icompiler, pipeline, tail).map(Some),
    "yield" => yield_form(icompiler, pipeline, tail).map(Some),
    "return" => return_form(icompiler, pipeline, tail).map(Some),
    "macrolet" => macrolet_form(icompiler, pipeline, tail).map(Some),
    _ => Ok(None),
  }
}

pub fn progn_form(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  tail: &[&AST])
                  -> Result<Expr, Error> {
  let body = tail.iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Progn(body))
}

pub fn cond_form(icompiler: &mut IncCompiler,
                 pipeline: &mut Pipeline,
                 tail: &[&AST])
                 -> Result<Expr, Error> {
  let body = tail.iter().map(|clause| {
    let vec: Vec<&AST> = DottedExpr::new(clause).try_into()?;
    match vec.len() {
      0 => {
        Err(Error::from(GDError::InvalidArg(String::from("cond"), (*clause).clone(), String::from("nonempty list"))))
      }
      1 => {
        let cond = icompiler.compile_expr(pipeline, vec[0])?;
        Ok((cond, None))
      }
      _ => {
        let cond = icompiler.compile_expr(pipeline, vec[0])?;
        let inner = vec[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
        Ok((cond, Some(Expr::Progn(inner))))
      }
    }
  }).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::CondStmt(body))
}

pub fn while_form(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  tail: &[&AST])
                  -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("while"), tail.len())));
  }
  let cond = icompiler.compile_expr(pipeline, tail[0])?;
  let body = tail[1..].iter().map(|x| icompiler.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::while_stmt(cond, Expr::Progn(body)))
}

pub fn for_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST])
                -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("for"), tail.len())));
  }
  let name = match tail[0] {
    AST::Symbol(s) => s.to_owned(),
    _ => return Err(Error::from(GDError::InvalidArg(String::from("for"), (*tail[0]).clone(), String::from("variable name")))),
  };
  let iter = icompiler.compile_expr(pipeline, tail[1])?;
  let body = tail[2..].iter().map(|x| icompiler.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::for_stmt(name, iter, Expr::Progn(body)))
}

pub fn let_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST])
                -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("let"), tail.len())));
  }
  let vars: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let var_clauses = vars.into_iter().map(|clause| {
    let var: Vec<_> = match DottedExpr::new(clause) {
      DottedExpr { elements, terminal: AST::Nil } if !elements.is_empty() => elements,
      DottedExpr { elements, terminal: tail@AST::Symbol(_) } if elements.is_empty() => vec!(tail),
      _ => return Err(Error::from(GDError::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration"))))
    };
    let result_value = var[1..].iter().map(|e| icompiler.compile_expr(pipeline, e)).collect::<Result<Vec<_>, _>>()?;
    let name = match var[0] {
      AST::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::from(GDError::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration")))),
    }?;
    Ok((name, Expr::Progn(result_value)))
  }).collect::<Result<Vec<_>, _>>()?;
  let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Let(var_clauses, Box::new(Expr::Progn(body))))
}

pub fn lambda_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST])
                   -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("lambda"), 1)));
  }
  let args: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let args = ArgList::parse(args)?;
  let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::Lambda(args, Box::new(Expr::Progn(body))))
}

pub fn function_form(tail: &[&AST])
                     -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("function"), 1)));
  }
  if tail.len() > 1 {
    return Err(Error::from(GDError::TooManyArgs(String::from("function"), 1)));
  }
  match tail[0] {
    AST::Symbol(s) => {
      Ok(Expr::FuncRef(FuncRefTarget::SimpleName(s.clone())))
    }
    x => {
      Err(Error::from(GDError::InvalidArg(String::from("function"), x.clone(), String::from("symbol"))))
    }
  }
}

pub fn assign_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST])
                   -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("setq"), 2)))
  }
  if tail.len() > 2 {
    return Err(Error::from(GDError::TooManyArgs(String::from("setq"), 2)))
  }
  let assign_target = match tail[0] {
    AST::Symbol(s) => {
      AssignTarget::Variable(s.to_owned())
    }
    x => {
      let inner: Vec<_> = DottedExpr::new(x).try_into()?;
      if inner[0] == &AST::Symbol(String::from("access-slot")) {
        if let Expr::FieldAccess(lhs, slot_name) = access_slot_form(icompiler, pipeline, &inner[1..])? {
          AssignTarget::InstanceField(lhs, slot_name)
        } else {
          return Err(Error::from(GDError::InvalidArg(String::from("setq"), x.clone(), String::from("symbol"))));
        }
      } else {
        return Err(Error::from(GDError::InvalidArg(String::from("setq"), x.clone(), String::from("symbol"))));
      }
    }
  };
  let value = icompiler.compile_expr(pipeline, tail[1])?;
  Ok(Expr::Assign(assign_target, Box::new(value)))
}

pub fn flet_form(icompiler: &mut IncCompiler,
                 pipeline: &mut Pipeline,
                 tail: &[&AST],
                 container: impl FnOnce(Vec<(String, ArgList, Expr)>, Box<Expr>) -> Expr)
                 -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("flet"), tail.len())));
  }
  let fns: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let fn_clauses = fns.into_iter().map(|clause| {
    let func: Vec<_> = DottedExpr::new(clause).try_into()?;
    if func.len() < 2 {
      return Err(Error::from(GDError::InvalidArg(String::from("flet"), clause.clone(), String::from("function declaration"))));
    }
    let name = match func[0] {
      AST::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::from(GDError::InvalidArg(String::from("flet"), (*clause).clone(), String::from("function declaration")))),
    }?;
    let args: Vec<_> = DottedExpr::new(func[1]).try_into()?;
    let args = ArgList::parse(args)?;
    let body = func[2..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok((name, args, Expr::Progn(body)))
  }).collect::<Result<Vec<_>, _>>()?;
  let names: Vec<_> = fn_clauses.iter().map(|x| x.0.to_owned()).collect();
  macrolet_unbind_macros(icompiler, pipeline, &mut names.iter().map(|x| &**x), |icompiler, pipeline| {
    let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(container(fn_clauses, Box::new(Expr::Progn(body))))
  })
}

pub fn quote_form(tail: &[&AST]) -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("quote"), 1)))
  }
  if tail.len() > 1 {
    return Err(Error::from(GDError::TooManyArgs(String::from("quote"), 1)))
  }
  Ok(Expr::Quote(tail[0].clone()))
}

pub fn quasiquote_form(icompiler: &mut IncCompiler,
                       pipeline: &mut Pipeline,
                       tail: &[&AST]) -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("quasiquote"), 1)))
  }
  if tail.len() > 1 {
    return Err(Error::from(GDError::TooManyArgs(String::from("quasiquote"), 1)))
  }
  quasiquote(icompiler, pipeline, tail[0])
}

pub fn access_slot_form(icompiler: &mut IncCompiler,
                        pipeline: &mut Pipeline,
                        tail: &[&AST]) -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("access-slot"), 2)))
  }
  if tail.len() > 2 {
    return Err(Error::from(GDError::TooManyArgs(String::from("access-slot"), 2)))
  }
  let lhs = icompiler.compile_expr(pipeline, tail[0])?;
  let slot_name = match tail[1] {
    AST::Symbol(s) => s.to_owned(),
    _ => return Err(Error::from(GDError::InvalidArg(String::from("access-slot"), tail[1].clone(), String::from("symbol")))),
  };
  Ok(Expr::FieldAccess(Box::new(lhs), slot_name))
}

pub fn new_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST]) -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("new"), 1)));
  }
  let super_call = match tail[0] {
    AST::Symbol(_) => AST::list(vec!((*tail[0]).clone())),
    _ => tail[0].clone(),
  };
  let super_call = Vec::try_from(DottedExpr::new(&super_call))?;
  if super_call.is_empty() {
    return Err(Error::from(GDError::InvalidArg(String::from("new"), tail[0].clone(), String::from("superclass declaration"))));
  }
  let superclass = match super_call[0] {
    AST::Symbol(superclass_name) => superclass_name.to_owned(),
    _ => return Err(Error::from(GDError::InvalidArg(String::from("new"), tail[0].clone(), String::from("superclass declaration")))),
  };
  let super_args = super_call[1..].iter().map(|arg| icompiler.compile_expr(pipeline, arg)).collect::<Result<Vec<_>, _>>()?;
  let mut cls = decl::ClassDecl::new(String::from("(local anonymous class)"), superclass);
  for decl in &tail[1..] {
    icompiler.compile_class_inner_decl(pipeline, &mut cls, decl)?;
  }
  let lambda_class = LambdaClass::from((cls, super_args));
  Ok(Expr::LambdaClass(Box::new(lambda_class)))
}

pub fn yield_form(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  tail: &[&AST]) -> Result<Expr, Error> {
  match tail.len() {
    0 => {
      Ok(Expr::Yield(None))
    }
    1 => {
      // TODO This isn't completely accurate, as we accept either 0 or 2
      // arguments, so TooFewArgs here is misleading.
      Err(Error::from(GDError::TooFewArgs(String::from("yield"), 2)))
    }
    2 => {
      let lhs = icompiler.compile_expr(pipeline, tail[0])?;
      let rhs = icompiler.compile_expr(pipeline, tail[1])?;
      Ok(Expr::Yield(Some((Box::new(lhs), Box::new(rhs)))))
    }
    _ => {
      Err(Error::from(GDError::TooManyArgs(String::from("yield"), 2)))
    }
  }
}

pub fn return_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST]) -> Result<Expr, Error> {
  match tail.len() {
    0 => {
      Err(Error::from(GDError::TooFewArgs(String::from("return"), 1)))
    }
    1 => {
      let expr = icompiler.compile_expr(pipeline, tail[0])?;
      Ok(Expr::Return(Box::new(expr)))
    }
    _ => {
      Err(Error::from(GDError::TooManyArgs(String::from("yield"), 1)))
    }
  }
}

pub fn macrolet_form(icompiler: &mut IncCompiler,
                     pipeline: &mut Pipeline,
                     tail: &[&AST])
                 -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("macrolet"), tail.len())));
  }
  let fns: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let fn_clauses = fns.into_iter().map(|clause| {
    let func: Vec<_> = DottedExpr::new(clause).try_into()?;
    if func.len() < 2 {
      return Err(Error::from(GDError::InvalidArg(String::from("macrolet"), clause.clone(), String::from("macro declaration"))));
    }
    let name = match func[0] {
      AST::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::from(GDError::InvalidArg(String::from("macrolet"), (*clause).clone(), String::from("macro declaration")))),
    }?;
    let args: Vec<_> = DottedExpr::new(func[1]).try_into()?;
    let args = ArgList::parse(args)?;
    let body = func[2..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(decl::MacroDecl { name, args, body: Expr::Progn(body) })
  }).collect::<Result<Vec<_>, _>>()?;

  macrolet_bind_locals(icompiler, pipeline, &mut fn_clauses.into_iter(), |icompiler, pipeline| {
    let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::Progn(body))
  })

}

fn macrolet_bind_locals<B, E, F, I>(icompiler: &mut IncCompiler,
                                    pipeline: &mut Pipeline,
                                    macros: &mut I,
                                    func: F)
                                    -> Result<B, E>
where E : From<Error>,
      F : FnOnce(&mut IncCompiler, &mut Pipeline) -> Result<B, E>,
      I : Iterator<Item=decl::MacroDecl> {
  match macros.next() {
    None => func(icompiler, pipeline),
    Some(m) => icompiler.locally_save_macro(&m.name.to_string(), |icompiler| {
      let name = m.name.to_string();
      let old_symbol_value = {
        let table = icompiler.symbol_table();
        table.get(&*Id::build(Namespace::Function, &name)).cloned()
      };
      icompiler.symbol_table().set(Id::new(Namespace::Function, name.to_string()), Decl::MacroDecl(m.clone()));
      icompiler.bind_macro(pipeline, &name, m)?;
      let result = macrolet_bind_locals(icompiler, pipeline, macros, func);
      if let Some(old_symbol_value) = old_symbol_value {
        let table = icompiler.symbol_table();
        table.set(Id::new(Namespace::Function, name.to_string()), old_symbol_value);
      }
      icompiler.unbind_macro(&name);
      result
    }),
  }
}

fn macrolet_unbind_macros<'a, B, E, F, I>(icompiler: &mut IncCompiler,
                                          pipeline: &mut Pipeline,
                                          macros: &mut I,
                                          func: F)
                                          -> Result<B, E>
where E : From<Error>,
      F : FnOnce(&mut IncCompiler, &mut Pipeline) -> Result<B, E>,
      I : Iterator<Item=&'a str> {
  match macros.next() {
    None => func(icompiler, pipeline),
    Some(name) => {
      if icompiler.has_macro(name) {
        icompiler.locally_save_macro(name, |icompiler| {
          let old_symbol_value = {
            let table = icompiler.symbol_table();
            table.get(&*Id::build(Namespace::Function, &name)).cloned()
          };
          icompiler.unbind_macro(name);
          let result = macrolet_unbind_macros(icompiler, pipeline, macros, func);
          if let Some(old_symbol_value) = old_symbol_value {
            let table = icompiler.symbol_table();
            table.set(Id::new(Namespace::Function, name.to_string()), old_symbol_value);
          }
          result
        })
      } else {
        macrolet_unbind_macros(icompiler, pipeline, macros, func)
      }
    }
  }
}
