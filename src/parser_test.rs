
// These are the tests for parser.lalrpop

#[cfg(test)]
mod tests {
  use crate::parser::*;
  use crate::sxp::ast::AST;

  #[test]
  fn parser_simple() {
    let p = ASTParser::new();
    assert_eq!(p.parse("12").unwrap(), AST::Int(12));
    assert_eq!(p.parse("12.0").unwrap(), AST::Float((12.0).into()));
    assert_eq!(p.parse("abc").unwrap(), AST::symbol("abc"));
    assert_eq!(p.parse("abc.def").unwrap(), AST::symbol("abc.def"));
    assert_eq!(p.parse("\"abc\"").unwrap(), AST::string("abc"));
  }

  #[test]
  fn parser_string() {
    let p = ASTParser::new();
    assert_eq!(p.parse("\"abcdef\"").unwrap(), AST::string("abcdef"));
    assert_eq!(p.parse(r#""abc\"def""#).unwrap(), AST::string("abc\"def"));
    assert_eq!(p.parse(r#""abc\\def\\""#).unwrap(), AST::string("abc\\def\\"));
  }

  #[test]
  fn parser_list() {
    let p = ASTParser::new();
    assert_eq!(p.parse("()").unwrap(), AST::Nil);
    assert_eq!(p.parse("(1)").unwrap(), AST::cons(AST::Int(1), AST::Nil));
    assert_eq!(p.parse("(1 . 2)").unwrap(), AST::cons(AST::Int(1), AST::Int(2)));
    assert_eq!(p.parse("(1 2)").unwrap(), AST::cons(AST::Int(1), AST::cons(AST::Int(2), AST::Nil)));
  }

  #[test]
  fn parser_quoting() {
    let p = ASTParser::new();
    assert_eq!(p.parse("'a").unwrap(), AST::list(vec!(AST::symbol("quote"), AST::symbol("a"))));
    assert_eq!(p.parse("`a").unwrap(), AST::list(vec!(AST::symbol("quasiquote"), AST::symbol("a"))));
    assert_eq!(p.parse(",a").unwrap(), AST::list(vec!(AST::symbol("unquote"), AST::symbol("a"))));
  }

  #[test]
  fn parser_colon() {
    let p = ASTParser::new();
    assert_eq!(p.parse("a:b").unwrap(), AST::list(vec!(AST::symbol("access-slot"), AST::symbol("a"), AST::symbol("b"))));
    assert_eq!(p.parse("(1 . 2):b").unwrap(), AST::list(vec!(AST::symbol("access-slot"), AST::cons(AST::Int(1), AST::Int(2)), AST::symbol("b"))));
    assert_eq!(p.parse("'a:b").unwrap(), AST::list(vec!(AST::symbol("quote"), AST::list(vec!(AST::symbol("access-slot"), AST::symbol("a"), AST::symbol("b"))))));
    assert_eq!(p.parse("a:b:c").unwrap(), AST::list(vec!(AST::symbol("access-slot"), AST::list(vec!(AST::symbol("access-slot"), AST::symbol("a"), AST::symbol("b"))), AST::symbol("c"))));
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
    assert!(p.parse("a:").is_err());
    assert!(p.parse(":b").is_err());
    assert!(p.parse("a:(1 . 2)").is_err());
    assert!(p.parse("abc.").is_err());
    assert!(p.parse(".def").is_err());
  }

}
