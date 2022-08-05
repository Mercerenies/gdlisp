
pub mod assignment;
pub mod bootstrap;
pub mod local_binding;

use crate::sxp::ast::{AST, ASTF};
use crate::sxp::dotted::DottedExpr;
use super::expr::{ExprF, Expr, FuncRefTarget, AssignTarget, LambdaClass, LocalVarClause, LocalFnClause};
use super::special_ref::SpecialRef;
use super::decl::{self, Decl, DeclF};
use super::arglist::ArgList;
use super::quasiquote::quasiquote_with_depth;
use crate::compile::error::{GDError, GDErrorF};
use crate::compile::args::{Expecting, ExpectedShape};
use crate::compile::frame::MAX_QUOTE_REIFY_DEPTH;
use crate::ir::incremental::IncCompiler;
use crate::ir::identifier::{Id, IdLike, Namespace};
use crate::ir::export::Visibility;
use crate::ir::import::{ImportDecl, ImportDeclParseError};
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::pipeline::error::PError;
use local_binding::{FLetLocalBinding, LabelsLocalBinding, LocalBinding};
use assignment::AssignmentForm;

use std::convert::{TryFrom, TryInto};
use std::borrow::Borrow;

pub fn dispatch_form(icompiler: &mut IncCompiler,
                     pipeline: &mut Pipeline,
                     head: &str,
                     tail: &[&AST],
                     pos: SourceOffset)
                     -> Result<Option<Expr>, PError> {
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
    "unquote" => Err(PError::from(GDError::new(GDErrorF::UnquoteOutsideQuasiquote, pos))),
    "unquote-spliced" => Err(PError::from(GDError::new(GDErrorF::UnquoteSplicedOutsideQuasiquote, pos))),
    "access-slot" => access_slot_form(icompiler, pipeline, tail, pos).map(Some),
    "new" => new_form(icompiler, pipeline, tail, pos).map(Some),
    "yield" => yield_form(icompiler, pipeline, tail, pos).map(Some),
    "assert" => assert_form(icompiler, pipeline, tail, pos).map(Some),
    "return" => return_form(icompiler, pipeline, tail, pos).map(Some),
    "macrolet" => macrolet_form(icompiler, pipeline, tail, pos).map(Some),
    "symbol-macrolet" => symbol_macrolet_form(icompiler, pipeline, tail, pos).map(Some),
    "sys/special-ref" => special_ref_form(icompiler, pipeline, tail, pos).map(Some),
    "sys/context-filename" => context_filename_form(icompiler, pipeline, tail, pos).map(Some),
    "literally" => literally_form(tail, pos).map(Some),
    "sys/split" => split_form(icompiler, pipeline, tail, pos).map(Some),
    "preload" => preload_form(icompiler, pipeline, tail, pos).map(Some),
    _ => Ok(None),
  }
}

pub fn progn_form(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  tail: &[&AST],
                  pos: SourceOffset)
                  -> Result<Expr, PError> {
  let body = tail.iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::progn(body, pos))
}

pub fn cond_form(icompiler: &mut IncCompiler,
                 pipeline: &mut Pipeline,
                 tail: &[&AST],
                 pos: SourceOffset)
                 -> Result<Expr, PError> {
  let body = tail.iter().map(|clause| {
    let vec: Vec<&AST> = DottedExpr::new(clause).try_into().map_err(|x| GDError::from_value(x, pos))?;
    match vec.len() {
      0 => {
        Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("cond"), (*clause).clone(), ExpectedShape::NonemptyList), pos)))
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
                  -> Result<Expr, PError> {
  Expecting::at_least(1).validate("while", pos, tail)?;
  let cond = icompiler.compile_expr(pipeline, tail[0])?;
  let body = tail[1..].iter().map(|x| icompiler.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::while_stmt(cond, Expr::progn(body, pos), pos))
}

pub fn for_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST],
                pos: SourceOffset)
                -> Result<Expr, PError> {
  Expecting::at_least(2).validate("for", pos, tail)?;
  let name = ExpectedShape::extract_symbol("for", tail[0].clone())?;
  let iter = icompiler.compile_expr(pipeline, tail[1])?;
  let body = tail[2..].iter().map(|x| icompiler.compile_expr(pipeline, x)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::for_stmt(name, iter, Expr::progn(body, pos), pos))
}

pub fn let_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST],
                pos: SourceOffset)
                -> Result<Expr, PError> {
  Expecting::at_least(1).validate("let", pos, tail)?;
  let vars: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let var_clauses = vars.into_iter().map(|clause| {
    let var: Vec<_> = match DottedExpr::new(clause) {
      DottedExpr { elements, terminal: AST { value: ASTF::Nil, pos: _ } } if !elements.is_empty() => elements,
      DottedExpr { elements, terminal: tail@AST { value: ASTF::Symbol(_), pos: _ } } if elements.is_empty() => vec!(tail),
      _ => return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("let"), (*clause).clone(), ExpectedShape::VarDecl), pos)))
    };
    let result_value = var[1..].iter().map(|e| icompiler.compile_expr(pipeline, e)).collect::<Result<Vec<_>, _>>()?;
    let name = match &var[0].value {
      ASTF::Symbol(s) => Ok(s.clone()),
      _ => Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("let"), (*clause).clone(), ExpectedShape::VarDecl), pos))),
    }?;
    Ok(LocalVarClause { name, value: Expr::progn(result_value, clause.pos) })
  }).collect::<Result<Vec<_>, _>>()?;
  let var_names: Vec<_> = var_clauses.iter().map(|x| x.name.clone()).collect();
  macrolet_unbind_macros(icompiler, pipeline, &mut var_names.iter().map(|x| (Namespace::Value, &**x)), |icompiler, pipeline| {
    let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::new(ExprF::Let(var_clauses, Box::new(Expr::progn(body, pos))), pos))
  })
}

pub fn lambda_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST],
                   pos: SourceOffset)
                   -> Result<Expr, PError> {
  Expecting::at_least(1).validate("lambda", pos, tail)?;
  let args: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let args = ArgList::parse(args)?;
  let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
  Ok(Expr::new(ExprF::Lambda(args, Box::new(Expr::progn(body, pos))), pos))
}

pub fn function_form(tail: &[&AST],
                     pos: SourceOffset)
                     -> Result<Expr, PError> {
  Expecting::exactly(1).validate("function", pos, tail)?;
  let s = ExpectedShape::extract_symbol("function", tail[0].clone())?;
  Ok(Expr::new(ExprF::FuncRef(FuncRefTarget::SimpleName(s)), pos))
}

pub fn assign_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST],
                   pos: SourceOffset)
                   -> Result<Expr, PError> {
  Expecting::exactly(2).validate("set", pos, tail)?;
  let assign_target = match &tail[0].value {
    ASTF::Symbol(s) => {
      AssignmentForm::Simple(AssignTarget::Variable(tail[0].pos, s.to_owned()))
    }
    _ => {
      let x = tail[0];
      let inner: Vec<_> = DottedExpr::new(x).try_into()?;
      if inner[0].value == ASTF::Symbol(String::from("access-slot")) {
        if let ExprF::FieldAccess(lhs, slot_name) = access_slot_form(icompiler, pipeline, &inner[1..], pos)?.value {
          AssignmentForm::Simple(AssignTarget::InstanceField(inner[0].pos, lhs, slot_name))
        } else {
          return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("set"), x.clone(), ExpectedShape::Symbol), pos))); // TODO Is this possible? I think we can prove that this never happens with a bit of refactoring.
        }
      } else {
        let s = ExpectedShape::extract_symbol("set", inner[0].clone())?;
        let head = AssignmentForm::str_to_setter_prefix(&s);
        let args = inner[1..].iter().map(|x| icompiler.compile_expr(pipeline, x)).collect::<Result<_, _>>()?;
        AssignmentForm::SetterCall(head, args)
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
                 -> Result<Expr, PError> {
  // TODO This function is used for flet and labels, so using "flet"
  // in all of the errors is not strictly correct.
  Expecting::at_least(1).validate("flet", pos, tail)?;
  let fns: Vec<_> = DottedExpr::new(tail[0]).try_into()?;

  // Get all of the names of the declared functions
  let fn_names: Vec<_> = fns.iter().map(|clause| {
    let func: Vec<_> = DottedExpr::new(clause).try_into()?;
    if func.len() < 2 {
      return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("flet"), (*clause).clone(), ExpectedShape::FnDecl), pos)));
    }
    let result = ExpectedShape::extract_symbol("flet", func[0].clone())?;
    Ok(result)
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

  macrolet_unbind_macros(icompiler, pipeline, &mut pre_names.iter().map(|x| (Namespace::Function, &**x)), |icompiler, pipeline| {
    let fn_clauses = fns.into_iter().map(|clause| {
      let func: Vec<_> = DottedExpr::new(clause).try_into()?;
      if func.len() < 2 {
        return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("flet"), clause.clone(), ExpectedShape::FnDecl), pos)));
      }
      let name = match &func[0].value {
        ASTF::Symbol(s) => Ok(s.clone()),
        _ => Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("flet"), (*clause).clone(), ExpectedShape::FnDecl), pos))),
      }?;
      let args: Vec<_> = DottedExpr::new(func[1]).try_into()?;
      let args = ArgList::parse(args)?;
      let body = func[2..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
      Ok(LocalFnClause { name, args, body: Expr::progn(body, clause.pos) })
    }).collect::<Result<Vec<_>, _>>()?;
    macrolet_unbind_macros(icompiler, pipeline, &mut post_names.iter().map(|x| (Namespace::Function, &**x)), |icompiler, pipeline| {
      let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
      Ok(binding_rule.wrap_in_expr(fn_clauses, Box::new(Expr::progn(body, pos)), pos))
    })
  })
}

pub fn quote_form(tail: &[&AST], pos: SourceOffset) -> Result<Expr, PError> {
  Expecting::exactly(1).validate("quote", pos, tail)?;
  Ok(Expr::new(ExprF::Quote(tail[0].clone()), pos))
}

pub fn quasiquote_form(icompiler: &mut IncCompiler,
                       pipeline: &mut Pipeline,
                       tail: &[&AST],
                       pos: SourceOffset)
                       -> Result<Expr, PError> {
  Expecting::exactly(1).validate("quasiquote", pos, tail)?;
  quasiquote_with_depth(icompiler, pipeline, tail[0], MAX_QUOTE_REIFY_DEPTH)
}

pub fn access_slot_form(icompiler: &mut IncCompiler,
                        pipeline: &mut Pipeline,
                        tail: &[&AST],
                        pos: SourceOffset)
                        -> Result<Expr, PError> {
  Expecting::exactly(2).validate("access-slot", pos, tail)?;
  let lhs = icompiler.compile_expr(pipeline, tail[0])?;
  let slot_name = ExpectedShape::extract_symbol("access-slot", tail[1].clone())?;
  Ok(Expr::new(ExprF::FieldAccess(Box::new(lhs), slot_name), pos))
}

pub fn new_form(icompiler: &mut IncCompiler,
                pipeline: &mut Pipeline,
                tail: &[&AST],
                pos: SourceOffset)
                -> Result<Expr, PError> {
  Expecting::at_least(1).validate("new", pos, tail)?;
  let super_call = match &tail[0].value {
    ASTF::Symbol(_) => AST::dotted_list(vec!((*tail[0]).clone()), AST::new(ASTF::Nil, tail[0].pos)),
    _ => tail[0].clone(),
  };
  let super_call = Vec::try_from(DottedExpr::new(&super_call))?;
  if super_call.is_empty() {
    return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("new"), tail[0].clone(), ExpectedShape::SuperclassDecl), pos)));
  }
  let superclass = match &super_call[0].value {
    ASTF::Symbol(superclass_name) => superclass_name.to_owned(),
    _ => return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("new"), tail[0].clone(), ExpectedShape::SuperclassDecl), pos))),
  };
  let super_args = super_call[1..].iter().map(|arg| icompiler.compile_expr(pipeline, arg)).collect::<Result<Vec<_>, _>>()?;
  let mut cls = decl::ClassDecl::new(String::from("(local anonymous class)"), superclass);
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
                  -> Result<Expr, PError> {
  Expecting::between(0, 2).validate("yield", pos, tail)?;
  match tail.len() {
    0 => {
      Ok(Expr::yield_none(pos))
    }
    1 => {
      // TODO yield() is weird in that it's the only thing in GDLisp
      // right now that takes a non-interval number of arguments.
      // Every other function or special form can be broadly described
      // as taking [a, b] or [a, infinity) arguments, where a and b
      // are integers. yield() takes {0, 2}.
      //
      // It would be nice to *define* some semantics for yield(x), for
      // consistency, but I can't think of what that would mean. OTOH
      // how do we correctly report errors for this relatively bizarre
      // special form, since WrongNumberArgs assumes an interval as
      // expected argument count?
      Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("yield"), AST::nil(SourceOffset(0)), ExpectedShape::YieldArg), pos)))
    }
    2 => {
      let lhs = icompiler.compile_expr(pipeline, tail[0])?;
      let rhs = icompiler.compile_expr(pipeline, tail[1])?;
      Ok(Expr::yield_some(lhs, rhs, pos))
    }
    _ => {
      unreachable!()
    }
  }
}

pub fn assert_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST],
                   pos: SourceOffset)
                   -> Result<Expr, PError> {
  Expecting::between(1, 2).validate("assert", pos, tail)?;
  match tail.len() {
    1 => {
      let cond = icompiler.compile_expr(pipeline, tail[0])?;
      Ok(Expr::assert_expr(cond, None, pos))
    }
    2 => {
      let cond = icompiler.compile_expr(pipeline, tail[0])?;
      let message = icompiler.compile_expr(pipeline, tail[1])?;
      Ok(Expr::assert_expr(cond, Some(message), pos))
    }
    _ => {
      unreachable!()
    }
  }
}

pub fn return_form(icompiler: &mut IncCompiler,
                   pipeline: &mut Pipeline,
                   tail: &[&AST],
                   pos: SourceOffset)
                   -> Result<Expr, PError> {
  Expecting::exactly(1).validate("return", pos, tail)?;
  let expr = icompiler.compile_expr(pipeline, tail[0])?;
  Ok(Expr::new(ExprF::Return(Box::new(expr)), pos))
}

pub fn macrolet_form(icompiler: &mut IncCompiler,
                     pipeline: &mut Pipeline,
                     tail: &[&AST],
                     pos: SourceOffset)
                     -> Result<Expr, PError> {
  Expecting::at_least(1).validate("macrolet", pos, tail)?;
  let fns: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let fn_clauses = fns.into_iter().map(|clause| {
    let func: Vec<_> = DottedExpr::new(clause).try_into()?;
    if func.len() < 2 {
      return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("macrolet"), clause.clone(), ExpectedShape::MacroDecl), pos)));
    }
    let name = match &func[0].value {
      ASTF::Symbol(s) => Ok(s.clone()),
      _ => Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("macrolet"), (*clause).clone(), ExpectedShape::MacroDecl), pos))),
    }?;
    let args: Vec<_> = DottedExpr::new(func[1]).try_into()?;
    let args = ArgList::parse(args)?;
    let body = func[2..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(decl::MacroDecl { visibility: Visibility::MACRO, name, args, body: Expr::progn(body, clause.pos) })
  }).collect::<Result<Vec<_>, _>>()?;

  let mut fn_clauses = fn_clauses.into_iter().map(|x| (Namespace::Function, x));
  macrolet_bind_locals(icompiler, pipeline, &mut fn_clauses, pos, |icompiler, pipeline| {
    let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::progn(body, pos))
  })

}

pub fn symbol_macrolet_form(icompiler: &mut IncCompiler,
                            pipeline: &mut Pipeline,
                            tail: &[&AST],
                            pos: SourceOffset)
                            -> Result<Expr, PError> {
  Expecting::at_least(1).validate("symbol-macrolet", pos, tail)?;
  let vars: Vec<_> = DottedExpr::new(tail[0]).try_into()?;
  let var_clauses = vars.into_iter().map(|clause| {
    let var: Vec<_> = DottedExpr::new(clause).try_into()?;
    if var.len() != 2 {
      return Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("symbol-macrolet"), clause.clone(), ExpectedShape::MacroDecl), pos)));
    }
    let name = match &var[0].value {
      ASTF::Symbol(s) => Ok(s.clone()),
      _ => Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("symbol-macrolet"), (*clause).clone(), ExpectedShape::MacroDecl), pos))),
    }?;
    let body = icompiler.compile_expr(pipeline, var[1])?;
    Ok(decl::MacroDecl { visibility: Visibility::MACRO, name, args: ArgList::empty(), body: body })
  }).collect::<Result<Vec<_>, _>>()?;

  let mut var_clauses = var_clauses.into_iter().map(|x| (Namespace::Value, x));
  macrolet_bind_locals(icompiler, pipeline, &mut var_clauses, pos, |icompiler, pipeline| {
    let body = tail[1..].iter().map(|expr| icompiler.compile_expr(pipeline, expr)).collect::<Result<Vec<_>, _>>()?;
    Ok(Expr::progn(body, pos))
  })

}

pub fn special_ref_form(_icompiler: &mut IncCompiler,
                        _pipeline: &mut Pipeline,
                        tail: &[&AST],
                        pos: SourceOffset)
                        -> Result<Expr, PError> {
  Expecting::exactly(1).validate("sys/special-ref", pos, tail)?;
  if let ASTF::Symbol(sym) = &tail[0].value {
    match sym.borrow() {
      "this-file" => Ok(Expr::from_value(SpecialRef::ThisFile, pos)),
      "this-filename" => Ok(Expr::from_value(SpecialRef::ThisFileName, pos)),
      "this-true-filename" => Ok(Expr::from_value(SpecialRef::ThisTrueFileName, pos)),
      _ => Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("sys/special-ref"), tail[0].clone(), ExpectedShape::SpecialRefValue), pos))),
    }
  } else {
    Err(PError::from(GDError::new(GDErrorF::InvalidArg(String::from("sys/special-ref"), tail[0].clone(), ExpectedShape::SpecialRefValue), pos)))
  }
}

// This one requires a bit of explanation. (sys/context-filename path)
// is a compiler hook designed to assist with macro expansion. It's
// much easier to explain *how* it works than to explain when you'd
// need it. For the latter, see the expansion for `deflazy`, which
// uses this trick.
//
// path must be a literal string. (sys/context-filename path) plugs
// path into the *current* preload resolver during the compilation
// stage and produces a new literal string consisting of the result.
// Obviously, path must be a literal string which parses as a resource
// path (i.e. "res://*"). This is basically the same transformation
// (use ...) directives undergo, but I'm exposing it as an
// expression-level special form so that macros can exploit it.
//
// Note that (sys/context-filename ...) is *not* involved in
// dependency resolution, so macros won't detect it as a dependency
// like they will a (use ...) directive. Thus, it's basically only
// safe to use this directive in situations where the relevant file is
// guaranteed to *already* be loaded by a prior (use ...) in the same
// file.
pub fn context_filename_form(_icompiler: &mut IncCompiler,
                             _pipeline: &mut Pipeline,
                             tail: &[&AST],
                             pos: SourceOffset)
                             -> Result<Expr, PError> {
  Expecting::exactly(1).validate("sys/context-filename", pos, tail)?;
  let s = ExpectedShape::extract_string("sys/context-filename", tail[0].clone())?;
  let path = ImportDecl::parse_path_param(&s).ok_or_else(|| {
    let err = ImportDeclParseError::InvalidPath(s.clone());
    PError::from(GDError::from_value(err, pos))
  })?;
  Ok(Expr::new(ExprF::ContextualFilename(path), pos))
}

pub fn literally_form(tail: &[&AST], pos: SourceOffset) -> Result<Expr, PError> {
  Expecting::exactly(1).validate("literally", pos, tail)?;
  let name = ExpectedShape::extract_symbol("literally", tail[0].clone())?;
  Ok(Expr::new(ExprF::AtomicName(name), pos))
}

pub fn split_form(icompiler: &mut IncCompiler,
                  pipeline: &mut Pipeline,
                  tail: &[&AST],
                  pos: SourceOffset)
                  -> Result<Expr, PError> {
  Expecting::exactly(1).validate("sys/split", pos, tail)?;
  let expr = icompiler.compile_expr(pipeline, tail[0])?;
  Ok(expr.split(pos))
}

fn macrolet_bind_locals<B, E, F, I>(icompiler: &mut IncCompiler,
                                    pipeline: &mut Pipeline,
                                    macros: &mut I,
                                    pos: SourceOffset,
                                    func: F)
                                    -> Result<B, E>
where E : From<PError>,
      F : FnOnce(&mut IncCompiler, &mut Pipeline) -> Result<B, E>,
      I : Iterator<Item=(Namespace, decl::MacroDecl)> {
  match macros.next() {
    None => func(icompiler, pipeline),
    #[allow(clippy::redundant_clone)] // Clippy thinks this clone is
                                      // redundant but it doesn't
                                      // compile without it.
    Some((namespace, m)) => icompiler.locally_save_macro(&*Id::build(namespace, &m.name), |icompiler| {
      let name = m.name.to_string();
      icompiler.bind_macro(pipeline, m.to_owned(), pos, true, namespace)?;
      let old_symbol_value = {
        let table = icompiler.declaration_table();
        table.get(&*Id::build(namespace, &name)).cloned()
      };
      match namespace {
        Namespace::Function => {
          icompiler.declaration_table().add(Decl::new(DeclF::MacroDecl(m.clone()), pos));
        }
        Namespace::Value => {
          let symbol_macro = decl::SymbolMacroDecl { visibility: m.visibility, name: m.name.to_owned(), body: m.body.clone() };
          icompiler.declaration_table().add(Decl::new(DeclF::SymbolMacroDecl(symbol_macro), pos));
        }
      };
      let result = macrolet_bind_locals(icompiler, pipeline, macros, pos, func);
      if let Some(old_symbol_value) = old_symbol_value {
        let table = icompiler.declaration_table();
        table.add(old_symbol_value);
      } else {
        let table = icompiler.declaration_table();
        table.del(&*Id::build(namespace, &name));
      }
      icompiler.unbind_macro(&*Id::build(namespace, &name));
      result
    }),
  }
}

fn macrolet_unbind_macros<'a, B, E, F, I>(icompiler: &mut IncCompiler,
                                          pipeline: &mut Pipeline,
                                          macros: &mut I,
                                          func: F)
                                          -> Result<B, E>
where E : From<PError>,
      F : FnOnce(&mut IncCompiler, &mut Pipeline) -> Result<B, E>,
      I : Iterator<Item=(Namespace, &'a str)> {
  match macros.next() {
    None => func(icompiler, pipeline),
    Some(name) => {
      let name: &(dyn IdLike<NS=Namespace> + 'a) = &name;
      if icompiler.has_macro(name) {
        icompiler.locally_save_macro(name, |icompiler| {
          let old_symbol_value = {
            let table = icompiler.declaration_table();
            table.get(name).cloned()
          };
          icompiler.unbind_macro(name);
          let result = macrolet_unbind_macros(icompiler, pipeline, macros, func);
          if let Some(old_symbol_value) = old_symbol_value {
            let table = icompiler.declaration_table();
            table.add(old_symbol_value);
          } else {
            let table = icompiler.declaration_table();
            table.del(name);
          }
          result
        })
      } else {
        macrolet_unbind_macros(icompiler, pipeline, macros, func)
      }
    }
  }
}

pub fn preload_form(_icompiler: &mut IncCompiler,
                    _pipeline: &mut Pipeline,
                    tail: &[&AST],
                    pos: SourceOffset)
                    -> Result<Expr, PError> {
  Expecting::exactly(1).validate("preload", pos, tail)?;
  let s = ExpectedShape::extract_string("preload", tail[0].clone())?;
  Ok(Expr::new(ExprF::Preload(s), pos))
}
