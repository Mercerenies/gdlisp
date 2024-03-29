// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

// These are the tests for parser.lalrpop

#[cfg(test)]
mod tests {
  use crate::AST_PARSER;
  use crate::sxp::ast::{AST, ASTF};
  use crate::pipeline::source::SourceOffset;

  fn so(x: usize) -> SourceOffset {
    // I'm tired of writing it >.<
    SourceOffset(x)
  }

  #[test]
  fn parser_simple() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("12").unwrap(), AST::new(ASTF::int(12), so(0)));
    assert_eq!(p.parse("12.0").unwrap(), AST::new(ASTF::float(12.0), so(0)));
    assert_eq!(p.parse("abc").unwrap(), AST::symbol("abc", so(0)));
    assert_eq!(p.parse("abc.def").unwrap(), AST::symbol("abc.def", so(0)));
    assert_eq!(p.parse("\"abc\"").unwrap(), AST::string("abc", so(0)));
  }

  #[test]
  fn parser_simple_unicode() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("αβγ").unwrap(), AST::symbol("αβγ", so(0))); // Category Ll
    assert_eq!(p.parse("ΓΔ.ῼῼ").unwrap(), AST::symbol("ΓΔ.ῼῼ", so(0))); // Categories Lu and Lt
    assert_eq!(p.parse("aʳ").unwrap(), AST::symbol("aʳ", so(0))); // Category Lm
    assert_eq!(p.parse("aª").unwrap(), AST::symbol("aª", so(0))); // Category Lo
    assert_eq!(p.parse("a͚").unwrap(), AST::symbol("a͚", so(0))); // Category Mn
    assert_eq!(p.parse("Ⅸ³").unwrap(), AST::symbol("Ⅸ³", so(0))); // Categories Nl and No
    assert_eq!(p.parse("£3÷£2").unwrap(), AST::symbol("£3÷£2", so(0))); // Categories Sm and Sc
    assert_eq!(p.parse("©©©˄").unwrap(), AST::symbol("©©©˄", so(0))); // Categories Sk and So
    assert_eq!(p.parse("⁀־־٭").unwrap(), AST::symbol("⁀־־٭", so(0))); // Categories Pc, Pd, and Po
    assert_eq!(p.parse("value༣༣༣").unwrap(), AST::symbol("value༣༣༣", so(0))); // Category Nd (not allowed at beginning)
  }

  #[test]
  fn parser_string() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("\"abcdef\"").unwrap(), AST::string("abcdef", so(0)));
    assert_eq!(p.parse(r#""abc\"def""#).unwrap(), AST::string("abc\"def", so(0)));
    assert_eq!(p.parse(r#""abc\\def\\""#).unwrap(), AST::string("abc\\def\\", so(0)));
  }

  #[test]
  fn parser_list() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("()").unwrap(), AST::nil(so(0)));
    assert_eq!(p.parse("(1)").unwrap(), AST::cons(AST::int(1, so(1)), AST::nil(so(2)), so(0)));
    assert_eq!(p.parse("(1 . 2)").unwrap(), AST::cons(AST::int(1, so(1)), AST::int(2, so(5)), so(0)));
    assert_eq!(p.parse("(1 2)").unwrap(), AST::cons(AST::int(1, so(1)), AST::cons(AST::int(2, so(3)), AST::nil(so(4)), so(3)), so(0)));
  }

  #[test]
  fn parser_quoting() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("'a").unwrap(), AST::list(vec!(AST::symbol("quote", so(0)), AST::symbol("a", so(1))), so(0)));
    assert_eq!(p.parse("`a").unwrap(), AST::list(vec!(AST::symbol("quasiquote", so(0)), AST::symbol("a", so(1))), so(0)));
    assert_eq!(p.parse(",a").unwrap(), AST::list(vec!(AST::symbol("unquote", so(0)), AST::symbol("a", so(1))), so(0)));
  }

  #[test]
  fn parser_colon() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("a:b").unwrap(), AST::list(vec!(AST::symbol("access-slot", so(0)), AST::symbol("a", so(0)), AST::symbol("b", so(2))), so(0)));
    assert_eq!(p.parse("(1 . 2):b").unwrap(), AST::list(vec!(AST::symbol("access-slot", so(0)), AST::cons(AST::int(1, so(1)), AST::int(2, so(5)), so(0)), AST::symbol("b", so(8))), so(0)));
    assert_eq!(p.parse("'a:b").unwrap(), AST::list(vec!(AST::symbol("quote", so(0)), AST::list(vec!(AST::symbol("access-slot", so(1)), AST::symbol("a", so(1)), AST::symbol("b", so(3))), so(1))), so(0)));
    assert_eq!(p.parse("a:b:c").unwrap(), AST::list(vec!(AST::symbol("access-slot", so(0)), AST::list(vec!(AST::symbol("access-slot", so(0)), AST::symbol("a", so(0)), AST::symbol("b", so(2))), so(0)), AST::symbol("c", so(4))), so(0)));
  }

  #[test]
  fn parser_at_self() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("@b").unwrap(), AST::list(vec!(AST::symbol("access-slot", so(0)), AST::symbol("self", so(0)), AST::symbol("b", so(1))), so(0)));
    assert_eq!(p.parse("@b:c").unwrap(), AST::list(vec!(AST::symbol("access-slot", so(0)), AST::list(vec!(AST::symbol("access-slot", so(0)), AST::symbol("self", so(0)), AST::symbol("b", so(1))), so(0)), AST::symbol("c", so(3))), so(0)));
  }

  #[test]
  fn parser_comments() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("\"abcdef\" ;; test comment").unwrap(), AST::string("abcdef", so(0)));
    assert_eq!(p.parse("\"abc ;; def\"").unwrap(), AST::string("abc ;; def", so(0))); // Note: Not a comment
    assert_eq!(p.parse("(a ;; b \n\n\n c)").unwrap(), AST::cons(AST::symbol("a", so(1)), AST::cons(AST::symbol("c", so(12)), AST::nil(so(13)), so(12)), so(0)));
    assert_eq!(p.parse("\"abcdef\" #| test comment |#").unwrap(), AST::string("abcdef", so(0)));
    assert_eq!(p.parse("\"abc #| |# def\"").unwrap(), AST::string("abc #| |# def", so(0))); // Note: Not a comment
    assert_eq!(p.parse("(a #| b |# c)").unwrap(), AST::cons(AST::symbol("a", so(1)), AST::cons(AST::symbol("c", so(11)), AST::nil(so(12)), so(11)), so(0)));
  }

  #[test]
  fn parser_array() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("[]").unwrap(), AST::list(vec!(AST::symbol("array", so(0))), so(1)));
    assert_eq!(p.parse("[1]").unwrap(), AST::list(vec!(AST::symbol("array", so(0)), AST::int(1, so(1))), so(2)));
    assert_eq!(p.parse("[1 2]").unwrap(), AST::list(vec!(AST::symbol("array", so(0)), AST::int(1, so(1)), AST::int(2, so(3))), so(4)));
  }

  #[test]
  fn parser_dict() {
    let p = &AST_PARSER;
    assert_eq!(p.parse("{}").unwrap(), AST::list(vec!(AST::symbol("dict", so(0))), so(1)));
    assert_eq!(p.parse("{1 2}").unwrap(), AST::list(vec!(AST::symbol("dict", so(0)), AST::int(1, so(1)), AST::int(2, so(3))), so(4)));
    assert_eq!(p.parse("{1 2 3 4}").unwrap(), AST::list(vec!(AST::symbol("dict", so(0)), AST::int(1, so(1)), AST::int(2, so(3)), AST::int(3, so(5)), AST::int(4, so(7))), so(8)));
  }

  #[test]
  fn parser_failures() {
    let p = &AST_PARSER;
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
    assert!(p.parse("[a").is_err());
    assert!(p.parse("{a").is_err());
    assert!(p.parse("{a}").is_err()); // Not an even number of entries
    assert!(p.parse("༣value").is_err()); // Category Nd (not allowed at beginning)
    assert!(p.parse("valueऻ").is_err()); // Category Mc
    assert!(p.parse("value⃝value").is_err()); // Category Me
    assert!(p.parse("value❰").is_err()); // Category Ps
    assert!(p.parse("value❱").is_err()); // Category Pe
    assert!(p.parse("value«").is_err()); // Category Pi
    assert!(p.parse("value»").is_err()); // Category Pf
  }

}
