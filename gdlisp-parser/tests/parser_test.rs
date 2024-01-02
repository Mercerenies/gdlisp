// Copyright 2024 Silvio Mayolo
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

use gdlisp_parser::SEXPR_PARSER;
use gdlisp_parser::sexpr::SExpr;
use gdlisp_util::source::SourceOffset;

fn so(x: usize) -> SourceOffset {
  // I'm tired of writing it >.<
  SourceOffset(x)
}

#[test]
fn parser_simple() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("12").unwrap(), SExpr::int(12, so(0)));
  assert_eq!(p.parse("12.0").unwrap(), SExpr::float(12.0, so(0)));
  assert_eq!(p.parse("abc").unwrap(), SExpr::symbol("abc", so(0)));
  assert_eq!(p.parse("abc.def").unwrap(), SExpr::symbol("abc.def", so(0)));
  assert_eq!(p.parse("\"abc\"").unwrap(), SExpr::string("abc", so(0)));
}

#[test]
fn parser_simple_unicode() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("αβγ").unwrap(), SExpr::symbol("αβγ", so(0))); // Category Ll
  assert_eq!(p.parse("ΓΔ.ῼῼ").unwrap(), SExpr::symbol("ΓΔ.ῼῼ", so(0))); // Categories Lu and Lt
  assert_eq!(p.parse("aʳ").unwrap(), SExpr::symbol("aʳ", so(0))); // Category Lm
  assert_eq!(p.parse("aª").unwrap(), SExpr::symbol("aª", so(0))); // Category Lo
  assert_eq!(p.parse("a͚").unwrap(), SExpr::symbol("a͚", so(0))); // Category Mn
  assert_eq!(p.parse("Ⅸ³").unwrap(), SExpr::symbol("Ⅸ³", so(0))); // Categories Nl and No
  assert_eq!(p.parse("£3÷£2").unwrap(), SExpr::symbol("£3÷£2", so(0))); // Categories Sm and Sc
  assert_eq!(p.parse("©©©˄").unwrap(), SExpr::symbol("©©©˄", so(0))); // Categories Sk and So
  assert_eq!(p.parse("⁀־־٭").unwrap(), SExpr::symbol("⁀־־٭", so(0))); // Categories Pc, Pd, and Po
  assert_eq!(p.parse("value༣༣༣").unwrap(), SExpr::symbol("value༣༣༣", so(0))); // Category Nd (not allowed at beginning)
}

#[test]
fn parser_string() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("\"abcdef\"").unwrap(), SExpr::string("abcdef", so(0)));
  assert_eq!(p.parse(r#""abc\"def""#).unwrap(), SExpr::string("abc\"def", so(0)));
  assert_eq!(p.parse(r#""abc\\def\\""#).unwrap(), SExpr::string("abc\\def\\", so(0)));
}

#[test]
fn parser_list() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("()").unwrap(), SExpr::nil(so(0)));
  assert_eq!(p.parse("(1)").unwrap(), SExpr::cons(SExpr::int(1, so(1)), SExpr::nil(so(2)), so(0)));
  assert_eq!(p.parse("(1 . 2)").unwrap(), SExpr::cons(SExpr::int(1, so(1)), SExpr::int(2, so(5)), so(0)));
  assert_eq!(p.parse("(1 2)").unwrap(), SExpr::cons(SExpr::int(1, so(1)), SExpr::cons(SExpr::int(2, so(3)), SExpr::nil(so(4)), so(3)), so(0)));
}

#[test]
fn parser_quoting() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("'a").unwrap(), SExpr::list(vec!(SExpr::symbol("quote", so(0)), SExpr::symbol("a", so(1))), so(0)));
  assert_eq!(p.parse("`a").unwrap(), SExpr::list(vec!(SExpr::symbol("quasiquote", so(0)), SExpr::symbol("a", so(1))), so(0)));
  assert_eq!(p.parse(",a").unwrap(), SExpr::list(vec!(SExpr::symbol("unquote", so(0)), SExpr::symbol("a", so(1))), so(0)));
}

#[test]
#[ignore = "not yet implemented"]
fn parser_colon() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("a:b").unwrap(), SExpr::list(vec!(SExpr::symbol("access-slot", so(0)), SExpr::symbol("a", so(0)), SExpr::symbol("b", so(2))), so(0)));
  assert_eq!(p.parse("(1 . 2):b").unwrap(), SExpr::list(vec!(SExpr::symbol("access-slot", so(0)), SExpr::cons(SExpr::int(1, so(1)), SExpr::int(2, so(5)), so(0)), SExpr::symbol("b", so(8))), so(0)));
  assert_eq!(p.parse("'a:b").unwrap(), SExpr::list(vec!(SExpr::symbol("quote", so(0)), SExpr::list(vec!(SExpr::symbol("access-slot", so(1)), SExpr::symbol("a", so(1)), SExpr::symbol("b", so(3))), so(1))), so(0)));
  assert_eq!(p.parse("a:b:c").unwrap(), SExpr::list(vec!(SExpr::symbol("access-slot", so(0)), SExpr::list(vec!(SExpr::symbol("access-slot", so(0)), SExpr::symbol("a", so(0)), SExpr::symbol("b", so(2))), so(0)), SExpr::symbol("c", so(4))), so(0)));
}

#[test]
#[ignore = "not yet implemented"]
fn parser_at_self() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("@b").unwrap(), SExpr::list(vec!(SExpr::symbol("access-slot", so(0)), SExpr::symbol("self", so(0)), SExpr::symbol("b", so(1))), so(0)));
  assert_eq!(p.parse("@b:c").unwrap(), SExpr::list(vec!(SExpr::symbol("access-slot", so(0)), SExpr::list(vec!(SExpr::symbol("access-slot", so(0)), SExpr::symbol("self", so(0)), SExpr::symbol("b", so(1))), so(0)), SExpr::symbol("c", so(3))), so(0)));
}

#[test]
fn parser_comments() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("\"abcdef\" ;; test comment").unwrap(), SExpr::string("abcdef", so(0)));
  assert_eq!(p.parse("\"abc ;; def\"").unwrap(), SExpr::string("abc ;; def", so(0))); // Note: Not a comment
  assert_eq!(p.parse("(a ;; b \n\n\n c)").unwrap(), SExpr::cons(SExpr::symbol("a", so(1)), SExpr::cons(SExpr::symbol("c", so(12)), SExpr::nil(so(13)), so(12)), so(0)));
  assert_eq!(p.parse("\"abcdef\" #| test comment |#").unwrap(), SExpr::string("abcdef", so(0)));
  assert_eq!(p.parse("\"abc #| |# def\"").unwrap(), SExpr::string("abc #| |# def", so(0))); // Note: Not a comment
  assert_eq!(p.parse("(a #| b |# c)").unwrap(), SExpr::cons(SExpr::symbol("a", so(1)), SExpr::cons(SExpr::symbol("c", so(11)), SExpr::nil(so(12)), so(11)), so(0)));
}

#[test]
fn parser_array() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("[]").unwrap(), SExpr::list(vec!(SExpr::symbol("array", so(0))), so(1)));
  assert_eq!(p.parse("[1]").unwrap(), SExpr::list(vec!(SExpr::symbol("array", so(0)), SExpr::int(1, so(1))), so(2)));
  assert_eq!(p.parse("[1 2]").unwrap(), SExpr::list(vec!(SExpr::symbol("array", so(0)), SExpr::int(1, so(1)), SExpr::int(2, so(3))), so(4)));
}

#[test]
fn parser_dict() {
  let p = &SEXPR_PARSER;
  assert_eq!(p.parse("{}").unwrap(), SExpr::list(vec!(SExpr::symbol("dict", so(0))), so(1)));
  assert_eq!(p.parse("{1 2}").unwrap(), SExpr::list(vec!(SExpr::symbol("dict", so(0)), SExpr::int(1, so(1)), SExpr::int(2, so(3))), so(4)));
  assert_eq!(p.parse("{1 2 3 4}").unwrap(), SExpr::list(vec!(SExpr::symbol("dict", so(0)), SExpr::int(1, so(1)), SExpr::int(2, so(3)), SExpr::int(3, so(5)), SExpr::int(4, so(7))), so(8)));
}

#[test]
fn parser_failures() {
  let p = &SEXPR_PARSER;
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
