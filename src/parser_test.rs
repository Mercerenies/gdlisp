
// These are the tests for parser.lalrpop

#[cfg(test)]
mod tests {
  use crate::parser::*;
  use crate::sxp::ast::{self, AST};

  #[test]
  fn parser_simple() {
    let p = ASTParser::new();
    assert_eq!(p.parse("12").unwrap(), AST::Int(12));
    assert_eq!(p.parse("12.0").unwrap(), AST::Float((12.0).into()));
    assert_eq!(p.parse("abc").unwrap(), ast::symbol("abc"));
    assert_eq!(p.parse("\"abc\"").unwrap(), ast::string("abc"));
  }

  #[test]
  fn parser_string() {
    let p = ASTParser::new();
    assert_eq!(p.parse("\"abcdef\"").unwrap(), ast::string("abcdef"));
    assert_eq!(p.parse(r#""abc\"def""#).unwrap(), ast::string("abc\"def"));
    assert_eq!(p.parse(r#""abc\\def\\""#).unwrap(), ast::string("abc\\def\\"));
  }

  #[test]
  fn parser_list() {
    let p = ASTParser::new();
    assert_eq!(p.parse("()").unwrap(), AST::Nil);
    assert_eq!(p.parse("(1)").unwrap(), ast::cons(AST::Int(1), AST::Nil));
    assert_eq!(p.parse("(1 . 2)").unwrap(), ast::cons(AST::Int(1), AST::Int(2)));
    assert_eq!(p.parse("(1 2)").unwrap(), ast::cons(AST::Int(1), ast::cons(AST::Int(2), AST::Nil)));
  }

  #[test]
  fn parser_quoting() {
    let p = ASTParser::new();
    assert_eq!(p.parse("'a").unwrap(), ast::list(vec!(ast::symbol("quote"), ast::symbol("a"))));
    assert_eq!(p.parse("`a").unwrap(), ast::list(vec!(ast::symbol("quasiquote"), ast::symbol("a"))));
    assert_eq!(p.parse(",a").unwrap(), ast::list(vec!(ast::symbol("unquote"), ast::symbol("a"))));
  }

  #[test]
  fn parser_failures() {
    let p = ASTParser::new();
    assert!(p.parse("(").is_err());
    assert!(p.parse("\"foo\\\"").is_err());
    assert!(p.parse(")").is_err());
    assert!(p.parse("(()").is_err());
    assert!(p.parse("1.").is_err());
    assert!(p.parse("()(").is_err());
    assert!(p.parse("(1 . )").is_err());
  }

}
