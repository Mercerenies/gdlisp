
//! Argument list types as supported by the IR.
//!
//! See [`crate::gdscript::arglist`] for the companion module on the
//! GDScript side.

pub mod constructor;
pub mod error;
pub mod ordinary;
pub mod simple;
pub mod vararg;

#[cfg(test)]
mod tests {
  use super::*;

  use ordinary::ArgList;
  use error::ArgListParseError;
  use vararg::VarArg;
  use crate::AST_PARSER;
  use crate::sxp::ast::AST;
  use crate::sxp::dotted::DottedExpr;
  use crate::compile::names::fresh::FreshNameGenerator;
  use crate::compile::names::NameTrans;
  use crate::gdscript::arglist::ArgList as GDArgList;

  use std::convert::TryInto;

  fn parse_ast(input: &str) -> AST {
    AST_PARSER.parse(input).unwrap()
  }

  fn parse_arglist(input: &str) -> Result<ArgList, ArgListParseError> {
    let ast = parse_ast(input);
    let dotted: Vec<_> = DottedExpr::new(&ast).try_into().unwrap();
    ArgList::parse(dotted)
  }

  fn arglist(req: Vec<&str>, opt: Vec<&str>, rest: Option<(&str, VarArg)>) -> ArgList {
    ArgList {
      required_args: req.into_iter().map(|x| x.to_owned()).collect(),
      optional_args: opt.into_iter().map(|x| x.to_owned()).collect(),
      rest_arg: rest.map(|(x, y)| (x.to_owned(), y)),
    }
  }

  fn gdarglist(req: Vec<&str>) -> GDArgList {
    GDArgList::required(req.into_iter().map(|x| x.to_owned()).collect())
  }

  fn into_gd(args: ArgList) -> (GDArgList, Vec<NameTrans>) {
    let mut tmp = FreshNameGenerator::new(vec!());
    args.into_gd_arglist(&mut tmp)
  }

  #[test]
  fn test_parsing() {
    assert_eq!(parse_arglist("()").unwrap(), arglist(vec!(), vec!(), None));
    assert_eq!(parse_arglist("(a)").unwrap(), arglist(vec!("a"), vec!(), None));
    assert_eq!(parse_arglist("(a b)").unwrap(), arglist(vec!("a", "b"), vec!(), None));
    assert_eq!(parse_arglist("(a b &opt c)").unwrap(), arglist(vec!("a", "b"), vec!("c"), None));
    assert_eq!(parse_arglist("(a &rest rest)").unwrap(), arglist(vec!("a"), vec!(), Some(("rest", VarArg::RestArg))));
    assert_eq!(parse_arglist("(a b c &opt d &rest e)").unwrap(), arglist(vec!("a", "b", "c"), vec!("d"), Some(("e", VarArg::RestArg))));
    assert_eq!(parse_arglist("(a b c &opt d e &rest f)").unwrap(), arglist(vec!("a", "b", "c"), vec!("d", "e"), Some(("f", VarArg::RestArg))));
    assert_eq!(parse_arglist("(a b c &opt d e &arr f)").unwrap(), arglist(vec!("a", "b", "c"), vec!("d", "e"), Some(("f", VarArg::ArrArg))));
    assert_eq!(parse_arglist("(a b c &opt d e)").unwrap(), arglist(vec!("a", "b", "c"), vec!("d", "e"), None));
  }

  #[test]
  fn test_invalid_parse() {
    assert!(parse_arglist("(&silly-name)").is_err());
    assert!(parse_arglist("(&opt a &opt b)").is_err());
    assert!(parse_arglist("(&rest a &opt b)").is_err());
    assert!(parse_arglist("(&arr a &opt b)").is_err());
    assert!(parse_arglist("(&rest a &rest b)").is_err());
    assert!(parse_arglist("(&rest a b)").is_err());
    assert!(parse_arglist("(&rest a &arr b)").is_err());
    assert!(parse_arglist("(&arr a &rest b)").is_err());
  }

  #[test]
  fn test_arglist_gen() {
    assert_eq!(into_gd(arglist(vec!(), vec!(), None)).0, gdarglist(vec!()));
    assert_eq!(into_gd(arglist(vec!("a"), vec!(), None)).0, gdarglist(vec!("a_0")));
    assert_eq!(into_gd(arglist(vec!("a", "b"), vec!(), None)).0, gdarglist(vec!("a_0", "b_1")));
    assert_eq!(into_gd(arglist(vec!("a"), vec!("b"), None)).0, gdarglist(vec!("a_0", "b_1")));
    assert_eq!(into_gd(arglist(vec!("a"), vec!("b"), Some(("r", VarArg::RestArg)))).0, gdarglist(vec!("a_0", "b_1", "r_2")));
    assert_eq!(into_gd(arglist(vec!("a"), vec!("b"), Some(("r", VarArg::ArrArg)))).0, gdarglist(vec!("a_0", "b_1", "r_2")));
  }

}
