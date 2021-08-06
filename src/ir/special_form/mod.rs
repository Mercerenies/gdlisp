
pub mod local_binding;
pub mod assignment;

use crate::sxp::ast::{AST, ASTF};
use crate::sxp::dotted::DottedExpr;
use super::expr::{ExprF, Expr, FuncRefTarget, AssignTarget, LambdaClass};
use super::decl::{self, Decl};
use super::arglist::ArgList;
use super::quasiquote::quasiquote;
use crate::compile::error::{Error as GDError};
use crate::ir::incremental::IncCompiler;
use crate::ir::identifier::{Id, Namespace};
use crate::ir::export::Visibility;
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::pipeline::error::Error;
use local_binding::{FLetLocalBinding, LabelsLocalBinding, LocalBinding};
use assignment::AssignmentForm;

use std::convert::{TryFrom, TryInto};

pub fn dispatch_form(icompiler: &mut IncCompiler,
                     pipeline: &mut Pipeline,
                     head: &str,
                     tail: &[&AST],
                     pos: SourceOffset)
                     -> Result<Option<Expr>, Error> {
  match head {
    "progn" => progn_form(icompiler, pipeline, tail, pos).map(Some),
    "cond" => cond_form(icompiler, pipeline, tail, pos).map(Some),
    "while" => while_form(icompiler, pipeline, tail, pos).map(Some),
    "for" => for_form(icompiler, pipeline, tail, pos).map(Some),
    "let" => let_form(icompiler, pipeline, tail, pos).map(Some),
    "flet" => flet_form(icompiler, pipeline, tail, pos, FLetLocalBinding).map(Some),
    "labels" => flet_form(icompiler, pipeline, tail, pos, LabelsLocalBinding).map(Some),
    "lambda" => lambda_form(icompiler, pipeline, tail, pos).map(Some),
    "function" => function_form(tail, pos).map(Some),
    "set" => assign_form(icompiler, pipeline, tail, pos).map(Some),
    "quote" => quote_form(tail, pos).map(Some),
    "quasiquote" => quasiquote_form(icompiler, pipeline, tail, pos).map(Some),
    "unquote" => Err(Error::from(GDError::UnquoteOutsideQuasiquote)),
    "unquote-spliced" => Err(Error::from(GDError::UnquoteSplicedOutsideQuasiquote)),
    "access-slot" => access_slot_form(icompiler, pipeline, tail, pos).map(Some),
    "new" => new_form(icompiler, pipeline, tail, pos).map(Some),
    "yield" => yield_form(icompiler, pipeline, tail, pos).map(Some),
    "return" => return_form(icompiler, pipeline, tail, pos).map(Some),
    "macrolet" => macrolet_form(icompiler, pipeline, tail, pos).map(Some),
    _ => Ok(None),
  }
}

pub fn progn_form(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  tail: &[&AST],
                  pos: SourceOffset)
                  -> Result<Expr, Error> {
  let body = tail.iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::progn(body, pos))
}

pub fn cond_form(icompiler: &mut IncCompiler,
                 pipeline: &mut Pipeline,
                 tail: &[&AST],
                 pos: SourceOffset)
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
        // In this branch, vec.len() > 1, so vec[1] is safe
        Ok((cond, Some(Expr::progn(inner, vec[1].pos))))
      }
    }
  }).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::new(ExprF::CondStmt(body), pos))
}

pub fn while_form(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  tail: &[&AST],
                  pos: SourceOffset)
                  -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("while"), tail.len())));
  }
  let cond = icompiler.compile_expr(pipeline, tail[0])?;
  let body = tail[1..].iter().map(|x| icompiler.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::while_stmt(cond, Expr::progn(body, pos), pos))
}

pub fn for_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST],
                pos: SourceOffset)
                -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("for"), tail.len())));
  }
  let name = match &tail[0].value {
    ASTF::Symbol(s) => s.to_owned(),
    _ => return Err(Error::from(GDError::InvalidArg(String::from("for"), (*tail[0]).clone(), String::from("variable name")))),
  };
  let iter = icompiler.compile_expr(pipeline, tail[1])?;
  let body = tail[2..].iter().map(|x| icompiler.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::for_stmt(name, iter, Expr::progn(body, pos), pos))
}

pub fn let_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST],
                pos: SourceOffset)
                -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("let"), tail.len())));
  }
  let vars: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let var_clauses = vars.into_iter().map(|clause| {
    let var: Vec<_> = match DottedExpr::new(clause) {
      DottedExpr { elements, terminal: AST { value: ASTF::Nil, pos: _ } } if !elements.is_empty() => elements,
      DottedExpr { elements, terminal: tail@AST { value: ASTF::Symbol(_), pos: _ } } if elements.is_empty() => vec!(tail),
      _ => return Err(Error::from(GDError::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration"))))
    };
    let result_value = var[1..].iter().map(|e| icompiler.compile_expr(pipeline, e)).collect::<Result<Vec<_>, _>>()?;
    let name = match &var[0].value {
      ASTF::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::from(GDError::InvalidArg(String::from("let"), (*clause).clone(), String::from("variable declaration")))),
    }?;
    Ok((name, Expr::progn(result_value, clause.pos)))
  }).collect::<Result<Vec<_>, _>>()?;
  let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::new(ExprF::Let(var_clauses, Box::new(Expr::progn(body, pos))), pos))
}

pub fn lambda_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST],
                   pos: SourceOffset)
                   -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("lambda"), 1)));
  }
  let args: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let args = ArgList::parse(args)?;
  let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::new(ExprF::Lambda(args, Box::new(Expr::progn(body, pos))), pos))
}

pub fn function_form(tail: &[&AST],
                     pos: SourceOffset)
                     -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("function"), 1)));
  }
  if tail.len() > 1 {
    return Err(Error::from(GDError::TooManyArgs(String::from("function"), 1)));
  }
  match &tail[0].value {
    ASTF::Symbol(s) => {
      Ok(Expr::new(ExprF::FuncRef(FuncRefTarget::SimpleName(s.clone())), pos))
    }
    _ => {
      Err(Error::from(GDError::InvalidArg(String::from("function"), tail[0].clone(), String::from("symbol"))))
    }
  }
}

pub fn assign_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST],
                   pos: SourceOffset)
                   -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("set"), 2)))
  }
  if tail.len() > 2 {
    return Err(Error::from(GDError::TooManyArgs(String::from("set"), 2)))
  }
  let assign_target = match &tail[0].value {
    ASTF::Symbol(s) => {
      AssignmentForm::Simple(AssignTarget::Variable(s.to_owned()))
    }
    _ => {
      let x = tail[0];
      let inner: Vec<_> = DottedExpr::new(x).try_into()?;
      if inner[0].value == ASTF::Symbol(String::from("access-slot")) {
        if let ExprF::FieldAccess(lhs, slot_name) = access_slot_form(icompiler, pipeline, &inner[1..], pos)?.value {
          AssignmentForm::Simple(AssignTarget::InstanceField(lhs, slot_name))
        } else {
          return Err(Error::from(GDError::InvalidArg(String::from("set"), x.clone(), String::from("symbol"))));
        }
      } else if let ASTF::Symbol(s) = &inner[0].value {
        let head = AssignmentForm::str_to_setter_prefix(s);
        let args = inner[1..].iter().map(|x| icompiler.compile_expr(pipeline, x)).collect::<Result<_, _>>()?;
        AssignmentForm::SetterCall(head, args)
      } else {
        return Err(Error::from(GDError::InvalidArg(String::from("set"), x.clone(), String::from("symbol"))));
      }
    }
  };
  let value = icompiler.compile_expr(pipeline, tail[1])?;
  Ok(assign_target.into_expr(value, pos))
}

pub fn flet_form(icompiler: &mut IncCompiler,
                 pipeline: &mut Pipeline,
                 tail: &[&AST],
                 pos: SourceOffset,
                 binding_rule: impl LocalBinding)
                 -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("flet"), tail.len())));
  }
  let fns: Vec<_> = DottedExpr::new(tail[0]).try_into()?;

  // Get all of the names of the declared functions
  let fn_names: Vec<_> = fns.iter().map(|clause| {
    let func: Vec<_> = DottedExpr::new(clause).try_into()?;
    if func.len() < 2 {
      return Err(Error::from(GDError::InvalidArg(String::from("flet"), (*clause).clone(), String::from("function declaration"))));
    }
    match &func[0].value {
      ASTF::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::from(GDError::InvalidArg(String::from("flet"), (*clause).clone(), String::from("function declaration")))),
    }
  }).collect::<Result<_, _>>()?;

  // If the recursive binding rule is in effect (i.e. for labels),
  // then we want to bind all of the names before compiling anything.
  // If the recursive binding rule is not in effect (i.e. for flet),
  // then we want to bind the names after compiling the clauses but
  // before compiling the body.
  let (pre_names, post_names) = if binding_rule.has_recursive_bindings() {
    (fn_names, vec!())
  } else {
    (vec!(), fn_names)
  };

  macrolet_unbind_macros(icompiler, pipeline, &mut pre_names.iter().map(|x| &**x), |icompiler, pipeline| {
    let fn_clauses = fns.into_iter().map(|clause| {
      let func: Vec<_> = DottedExpr::new(clause).try_into()?;
      if func.len() < 2 {
        return Err(Error::from(GDError::InvalidArg(String::from("flet"), clause.clone(), String::from("function declaration"))));
      }
      let name = match &func[0].value {
        ASTF::Symbol(s) => Ok(s.clone()),
        _ => Err(Error::from(GDError::InvalidArg(String::from("flet"), (*clause).clone(), String::from("function declaration")))),
      }?;
      let args: Vec<_> = DottedExpr::new(func[1]).try_into()?;
      let args = ArgList::parse(args)?;
      let body = func[2..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
      Ok((name, args, Expr::progn(body, clause.pos)))
    }).collect::<Result<Vec<_>, _>>()?;
    macrolet_unbind_macros(icompiler, pipeline, &mut post_names.iter().map(|x| &**x), |icompiler, pipeline| {
      let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
      Ok(binding_rule.wrap_in_expr(fn_clauses, Box::new(Expr::progn(body, pos)), pos))
    })
  })
}

pub fn quote_form(tail: &[&AST], pos: SourceOffset) -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("quote"), 1)))
  }
  if tail.len() > 1 {
    return Err(Error::from(GDError::TooManyArgs(String::from("quote"), 1)))
  }
  Ok(Expr::new(ExprF::Quote(tail[0].clone()), pos))
}

pub fn quasiquote_form(icompiler: &mut IncCompiler,
                       pipeline: &mut Pipeline,
                       tail: &[&AST],
                       _pos: SourceOffset)
                       -> Result<Expr, Error> {
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
                        tail: &[&AST],
                        pos: SourceOffset)
                        -> Result<Expr, Error> {
  if tail.len() < 2 {
    return Err(Error::from(GDError::TooFewArgs(String::from("access-slot"), 2)))
  }
  if tail.len() > 2 {
    return Err(Error::from(GDError::TooManyArgs(String::from("access-slot"), 2)))
  }
  let lhs = icompiler.compile_expr(pipeline, tail[0])?;
  let slot_name = match &tail[1].value {
    ASTF::Symbol(s) => s.to_owned(),
    _ => return Err(Error::from(GDError::InvalidArg(String::from("access-slot"), tail[1].clone(), String::from("symbol")))),
  };
  Ok(Expr::new(ExprF::FieldAccess(Box::new(lhs), slot_name), pos))
}

pub fn new_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST],
                pos: SourceOffset)
                -> Result<Expr, Error> {
  if tail.is_empty() {
    return Err(Error::from(GDError::TooFewArgs(String::from("new"), 1)));
  }
  let super_call = match &tail[0].value {
    ASTF::Symbol(_) => AST::dotted_list(vec!((*tail[0]).clone()), AST::new(ASTF::Nil, tail[0].pos)),
    _ => tail[0].clone(),
  };
  let super_call = Vec::try_from(DottedExpr::new(&super_call))?;
  if super_call.is_empty() {
    return Err(Error::from(GDError::InvalidArg(String::from("new"), tail[0].clone(), String::from("superclass declaration"))));
  }
  let superclass = match &super_call[0].value {
    ASTF::Symbol(superclass_name) => superclass_name.to_owned(),
    _ => return Err(Error::from(GDError::InvalidArg(String::from("new"), tail[0].clone(), String::from("superclass declaration")))),
  };
  let super_args = super_call[1..].iter().map(|arg| icompiler.compile_expr(pipeline, arg)).collect::<Result<Vec<_>, _>>()?;
  let mut cls = decl::ClassDecl::new(String::from("(local anonymous class)"), superclass, pos);
  for decl in &tail[1..] {
    icompiler.compile_class_inner_decl(pipeline, &mut cls, decl)?;
  }
  let lambda_class = LambdaClass::from((cls, super_args));
  Ok(Expr::new(ExprF::LambdaClass(Box::new(lambda_class)), pos))
}

pub fn yield_form(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  tail: &[&AST],
                  pos: SourceOffset)
                  -> Result<Expr, Error> {
  match tail.len() {
    0 => {
      Ok(Expr::new(ExprF::Yield(None), pos))
    }
    1 => {
      // TODO This isn't completely accurate, as we accept either 0 or 2
      // arguments, so TooFewArgs here is misleading.
      Err(Error::from(GDError::TooFewArgs(String::from("yield"), 2)))
    }
    2 => {
      let lhs = icompiler.compile_expr(pipeline, tail[0])?;
      let rhs = icompiler.compile_expr(pipeline, tail[1])?;
      Ok(Expr::new(ExprF::Yield(Some((Box::new(lhs), Box::new(rhs)))), pos))
    }
    _ => {
      Err(Error::from(GDError::TooManyArgs(String::from("yield"), 2)))
    }
  }
}

pub fn return_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST],
                   pos: SourceOffset)
                   -> Result<Expr, Error> {
  match tail.len() {
    0 => {
      Err(Error::from(GDError::TooFewArgs(String::from("return"), 1)))
    }
    1 => {
      let expr = icompiler.compile_expr(pipeline, tail[0])?;
      Ok(Expr::new(ExprF::Return(Box::new(expr)), pos))
    }
    _ => {
      Err(Error::from(GDError::TooManyArgs(String::from("yield"), 1)))
    }
  }
}

pub fn macrolet_form(icompiler: &mut IncCompiler,
                     pipeline: &mut Pipeline,
                     tail: &[&AST],
                     pos: SourceOffset)
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
    let name = match &func[0].value {
      ASTF::Symbol(s) => Ok(s.clone()),
      _ => Err(Error::from(GDError::InvalidArg(String::from("macrolet"), (*clause).clone(), String::from("macro declaration")))),
    }?;
    let args: Vec<_> = DottedExpr::new(func[1]).try_into()?;
    let args = ArgList::parse(args)?;
    let body = func[2..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(decl::MacroDecl { visibility: Visibility::MACRO, name, args, body: Expr::progn(body, clause.pos) })
  }).collect::<Result<Vec<_>, _>>()?;

  macrolet_bind_locals(icompiler, pipeline, &mut fn_clauses.into_iter(), |icompiler, pipeline| {
    let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::progn(body, pos))
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
    #[allow(clippy::redundant_clone)] // Clippy thinks this clone is
                                      // redundant but it doesn't
                                      // compile without it.
    Some(m) => icompiler.locally_save_macro(&m.name.to_string(), |icompiler| {
      let name = m.name.to_string();
      icompiler.bind_macro(pipeline, m.to_owned(), true)?;
      let old_symbol_value = {
        let table = icompiler.declaration_table();
        table.get(&*Id::build(Namespace::Function, &name)).cloned()
      };
      icompiler.declaration_table().add(Decl::MacroDecl(m.clone()));
      let result = macrolet_bind_locals(icompiler, pipeline, macros, func);
      if let Some(old_symbol_value) = old_symbol_value {
        let table = icompiler.declaration_table();
        table.add(old_symbol_value);
      } else {
        let table = icompiler.declaration_table();
        table.del(&*Id::build(Namespace::Function, &name));
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
            let table = icompiler.declaration_table();
            table.get(&*Id::build(Namespace::Function, &name)).cloned()
          };
          icompiler.unbind_macro(name);
          let result = macrolet_unbind_macros(icompiler, pipeline, macros, func);
          if let Some(old_symbol_value) = old_symbol_value {
            let table = icompiler.declaration_table();
            table.add(old_symbol_value);
          } else {
            let table = icompiler.declaration_table();
            table.del(&*Id::build(Namespace::Function, &name));
          }
          result
        })
      } else {
        macrolet_unbind_macros(icompiler, pipeline, macros, func)
      }
    }
  }
}
