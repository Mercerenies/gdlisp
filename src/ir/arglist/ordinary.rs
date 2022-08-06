
//! Provides the ordinary argument list type [`ArgList`] for
//! module-level functions and for lambda expressions.

use crate::sxp::ast::AST;
use crate::compile::names::{self, NameTrans};
use crate::compile::names::generator::NameGenerator;
use crate::compile::symbol_table::function_call::FnSpecs;
use crate::gdscript::arglist::ArgList as GDArgList;
use crate::pipeline::source::SourceOffset;
use super::error::{ArgListParseError, ArgListParseErrorF};
use super::vararg::VarArg;
use super::general::{GeneralArg, GeneralArgList};
use super::parser;

use std::convert::TryFrom;
use std::borrow::Borrow;

/// An argument list in GDLisp consists of a sequence of zero or more
/// required arguments, followed by zero or more optional arguments,
/// followed by (optionally) a "rest" argument.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgList {
  /// The list of required argument names.
  pub required_args: Vec<String>,
  /// The list of optional argument names. Note that optional
  /// arguments in GDLisp always default to `nil`, so no default value
  /// is explicitly mentioned here.
  pub optional_args: Vec<String>,
  /// The "rest" argument. If present, this indicates the name of the
  /// argument and the type of "rest" argument.
  pub rest_arg: Option<(String, VarArg)>,
}

impl ArgList {

  /// An empty argument list, taking no required or optional arguments
  /// and having no "rest" argument.
  pub fn empty() -> ArgList {
    ArgList {
      required_args: vec!(),
      optional_args: vec!(),
      rest_arg: None,
    }
  }

  /// An argument list consisting only of a single [`VarArg::RestArg`]
  /// argument with a default name.
  #[deprecated(note="Directly construct the arglist with an explicit rest name instead")]
  pub fn rest() -> ArgList {
    ArgList {
      required_args: vec!(),
      optional_args: vec!(),
      rest_arg: Some((String::from("rest-arg"), VarArg::RestArg)),
    }
  }

  /// An argument list consisting only of required arguments with the
  /// given names.
  pub fn required(args: Vec<String>) -> ArgList {
    ArgList {
      required_args: args,
      optional_args: vec!(),
      rest_arg: None,
    }
  }

  /// Converts an [`FnSpecs`] to an [`ArgList`] with dummy names for
  /// the variables.
  ///
  /// The names of the generated arguments may change and should not
  /// be relied upon; only the shape and lengths should be considered
  /// stable.
  pub fn from_specs(specs: FnSpecs) -> ArgList {
    // Uses dummy names for variables.
    let required_args = (0..specs.required).into_iter().map(|i| format!("required_arg{}", i)).collect();
    let optional_args = (0..specs.optional).into_iter().map(|i| format!("optional_arg{}", i)).collect();
    let rest_arg = specs.rest.map(|arg| (String::from("rest_arg"), arg));
    ArgList { required_args, optional_args, rest_arg }
  }

  /// Parse an argument list from an iterator of `AST` values. Returns
  /// either the [`GeneralArgList`] or an appropriate error.
  pub fn parse<'a>(args: impl IntoIterator<Item = &'a AST>, pos: SourceOffset)
                   -> Result<ArgList, ArgListParseError> {
    let general_arglist = parser::parse(args)?;
    ArgList::try_from(general_arglist).map_err(|err| ArgListParseError::new(err, pos))
  }

  /// Converts the argument list into a GDScript argument list, using
  /// the given name generator to produce unique GDScript names.
  ///
  /// * Each required argument will be translated into a GDScript
  /// argument.
  ///
  /// * Each optional argument, likewise, will be translated into a
  /// GDScript argument. On the GDScript side, required and optional
  /// arguments are indistinguishable.
  ///
  /// * If there is a "rest" argument of any kind, it is translated to
  /// a single GDScript argument as well.
  pub fn into_gd_arglist(self, gen: &mut impl NameGenerator) -> (GDArgList, Vec<NameTrans>) {
    let cap = 1 + self.required_args.len() + self.optional_args.len();
    let mut name_translations = Vec::with_capacity(cap);
    let mut args = Vec::with_capacity(cap);
    for arg in self.required_args {
      let gd = gen.generate_with(&names::lisp_to_gd(&arg));
      name_translations.push(NameTrans { lisp_name: arg, gd_name: gd.clone() });
      args.push(gd);
    }
    for arg in self.optional_args {
      let gd = gen.generate_with(&names::lisp_to_gd(&arg));
      name_translations.push(NameTrans { lisp_name: arg, gd_name: gd.clone() });
      args.push(gd);
    }
    if let Some((arg, _)) = self.rest_arg {
      let gd = gen.generate_with(&names::lisp_to_gd(&arg));
      name_translations.push(NameTrans { lisp_name: arg, gd_name: gd.clone() });
      args.push(gd);
    }
    (GDArgList::required(args), name_translations)
  }

  /// An iterator over all variable names mentioned in the argument
  /// list, in order.
  pub fn iter_vars(&self) -> impl Iterator<Item = &str> {
    self.required_args.iter()
      .chain(self.optional_args.iter())
      .chain(self.rest_arg.iter().map(|x| &x.0))
      .map(|x| x.borrow())
  }

}

impl From<ArgList> for FnSpecs {

  /// [`FnSpecs`] is simply an [`ArgList`] without the argument names;
  /// it merely preserves the shape. From an `ArgList` we can always
  /// construct an `FnSpecs` in a canonical way.
  fn from(arglist: ArgList) -> FnSpecs {
    // TODO We need to define an upper limit on argument list length
    // (and check if Godot already has one we need to respect)
    FnSpecs::new(
      arglist.required_args.len(),
      arglist.optional_args.len(),
      arglist.rest_arg.map(|x| x.1),
    )
  }

}

impl From<ArgList> for GeneralArgList {

  fn from(arglist: ArgList) -> GeneralArgList {
    let required_args: Vec<_> = arglist.required_args.into_iter().map(GeneralArg::new).collect();
    let optional_args: Vec<_> = arglist.optional_args.into_iter().map(GeneralArg::new).collect();
    let rest_arg: Option<_> = arglist.rest_arg.map(|(name, vararg)| (GeneralArg::new(name), vararg));
    GeneralArgList {
      required_args,
      optional_args,
      rest_arg,
    }
  }

}

impl TryFrom<GeneralArgList> for ArgList {
  type Error = ArgListParseErrorF;

  // Note: Can't fail right now, but it will be able to once
  // GeneralArgList supports constructor fields.
  fn try_from(arglist: GeneralArgList) -> Result<Self, Self::Error> {
    let required_args: Vec<_> = arglist.required_args.into_iter().map(|x| x.name).collect();
    let optional_args: Vec<_> = arglist.optional_args.into_iter().map(|x| x.name).collect();
    let rest_arg: Option<_> = arglist.rest_arg.map(|(arg, vararg)| (arg.name, vararg));
    Ok(ArgList {
      required_args,
      optional_args,
      rest_arg,
    })
  }

}
