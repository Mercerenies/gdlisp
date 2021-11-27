
//! GDScript declarations.
//!
//! This module defines a [datatype](Decl) for representing
//! declarations in the GDScript language, as well as
//! [`Decl::write_gd`] for writing declarations as GDScript syntax to
//! a [`fmt::Write`] instance.

use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::indent;
use crate::gdscript::library;
use crate::gdscript::arglist::ArgList;
use crate::pipeline::source::{SourceOffset, Sourced};

use std::fmt::{self, Write};

// TODO _init has some special syntax that we need to be prepared to handle.

/// The type of GDScript declarations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclF {
  VarDecl(Option<Export>, Onready, String, Option<Expr>),
  ConstDecl(String, Expr),
  ClassDecl(ClassDecl),
  InitFnDecl(InitFnDecl),
  FnDecl(Static, FnDecl),
  EnumDecl(EnumDecl),
  SignalDecl(String, ArgList),
}

/// GDScript declaration with its source offset. See [`Sourced`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decl {
  pub value: DeclF,
  pub pos: SourceOffset,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassDecl {
  pub name: String,
  pub extends: ClassExtends,
  pub body: Vec<Decl>,
}

/// The top-level class is a special kind of class, similar to a
/// [`ClassDecl`].
///
/// The top-level class, however, is not required to have a name and
/// will print using subtly different syntax.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TopLevelClass {
  pub name: Option<String>, // The top-level class is not required to have a name.
  pub extends: ClassExtends,
  pub body: Vec<Decl>,
}

/// A descriptor of what class is being extended.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassExtends {
  /// A qualified name. `Qualified(vec!("foo", "bar", "baz"))`
  /// represents the name `foo.bar.baz`.
  Qualified(Vec<String>),
  // StringLit(String), // TODO Support string literals (once we have them in general)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnDecl {
  pub name: String,
  pub args: ArgList,
  pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InitFnDecl {
  pub args: ArgList,
  pub super_call: Vec<Expr>,
  pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumDecl {
  pub name: Option<String>,
  pub clauses: Vec<(String, Option<Expr>)>,
}

/// A GDScript export declaration attached to a variable.
///
/// Not all expressions are valid in an export declaration, so the
/// user should take care to use this type only with valid
/// expressions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Export {
  pub args: Vec<Expr>,
}

/// A Boolean-isomorphic type which indicates whether or not a
/// function is static.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Static {
  NonStatic, IsStatic,
}

/// A Boolean-isomorphic type which indicates whether or not a
/// variable has the `onready` modifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Onready {
  No, Yes,
}

fn empty_class_body(pos: SourceOffset) -> Decl {
  Decl::new(
    DeclF::InitFnDecl(InitFnDecl {
      args: ArgList::empty(),
      super_call: vec!(),
      body: vec!(),
    }),
    pos
  )
}

impl Decl {

  /// A new `Decl` with its source offset.
  pub fn new(value: DeclF, pos: SourceOffset) -> Decl {
    Decl { value, pos }
  }

  /// The name of the identifier referenced by this declaration, if
  /// one exists. Some declarations, such as enums, may not always
  /// have names.
  pub fn name(&self) -> Option<&str> {
    match &self.value {
      DeclF::VarDecl(_, _, n, _) => Some(&n),
      DeclF::ConstDecl(n, _) => Some(&n),
      DeclF::ClassDecl(cdecl) => Some(&cdecl.name),
      DeclF::InitFnDecl(_) => Some(&library::CONSTRUCTOR_NAME),
      DeclF::FnDecl(_, fdecl) => Some(&fdecl.name),
      DeclF::EnumDecl(edecl) => edecl.name.as_ref().map(|x| x.as_ref()),
      DeclF::SignalDecl(n, _) => Some(&n)
    }
  }

  /// Write the declaration, as GDScript code, to the [`fmt::Write`]
  /// instance `w`.
  ///
  /// We are assumed to be at the indentation level `ind`, so that all
  /// lines in the result will be indented to that level.
  ///
  /// The writer `w` should currently be either empty or immediately
  /// after a newline. The declaration will always end by printing a
  /// newline, making it suitable for writing a subsequent declaration
  /// immediately after.
  pub fn write_gd<W : fmt::Write>(&self, w: &mut W, ind: u32) -> Result<(), fmt::Error> {
    indent(w, ind)?;
    match &self.value {
      DeclF::VarDecl(export, onready, name, value) => {
        if let Some(export) = export {
          if export.args.is_empty() {
            write!(w, "export ")?;
          } else {
            write!(w, "export({}) ", export.args.iter().map(|x| x.to_gd()).collect::<Vec<_>>().join(", "))?;
          }
        }
        if bool::from(*onready) {
          write!(w, "onready ")?;
        }
        write!(w, "var {}", name)?;
        match value {
          None => writeln!(w),
          Some(value) => writeln!(w, " = {}", value.to_gd()),
        }
      }
      DeclF::ConstDecl(name, value) => {
        writeln!(w, "const {} = {}", name, value.to_gd())
      }
      DeclF::ClassDecl(ClassDecl { name, extends, body }) => {
        writeln!(w, "class {} extends {}:", name, extends.to_gd())?;
        Decl::write_gd_decls(body, &empty_class_body(self.pos), w, ind + 4)
      }
      DeclF::FnDecl(stat, FnDecl { name, args, body }) => {
        if *stat == Static::IsStatic {
          write!(w, "static ")?;
        }
        writeln!(w, "func {}({}):", name, args.to_gd())?;
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
      DeclF::InitFnDecl(InitFnDecl { args, super_call, body }) => {
        let super_args: Vec<_> = super_call.iter().map(Expr::to_gd).collect();
        if super_args.is_empty() {
          writeln!(w, "func _init({}):", args.to_gd())?;
        } else {
          writeln!(w, "func _init({}).({}):", args.to_gd(), super_args.join(", "))?;
        }
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
      DeclF::SignalDecl(name, args) => {
        if args.is_empty() {
          writeln!(w, "signal {}", name)
        } else {
          writeln!(w, "signal {}({})", name, args.to_gd())
        }
      }
      DeclF::EnumDecl(EnumDecl { name, clauses }) => {
        write!(w, "enum ")?;
        if let Some(name) = name {
          write!(w, "{} ", name)?;
        }
        writeln!(w, "{{")?;
        for (const_name, const_value) in clauses {
          indent(w, ind + 4)?;
          write!(w, "{}", const_name)?;
          if let Some(const_value) = const_value {
            write!(w, " = {}", const_value.to_gd())?;
          }
          writeln!(w, ",")?;
        }
        indent(w, ind)?;
        writeln!(w, "}}")
      }
    }
  }

  /// Write several declarations in sequence, using [`Decl::write_gd`].
  ///
  /// If `iter` is empty, then ``default` will be written instead.
  pub fn write_gd_decls<'a, W, I>(iter: I, default: &Decl, w: &mut W, ind: u32) -> Result<(), fmt::Error>
  where W : fmt::Write,
        I : IntoIterator<Item = &'a Decl> {
    let mut empty = true;
    for decl in iter {
      decl.write_gd(w, ind)?;
      empty = false
    }
    if empty {
      default.write_gd(w, ind)?;
    }
    Ok(())
  }

  /// Write the declaration to a string, using [`Decl::write_gd`].
  ///
  /// # Panics
  ///
  /// This function panics if there is a write error to the string. If
  /// you wish to handle that case yourself, use [`Stmt::write_gd`]
  /// explicitly.
  pub fn to_gd(&self, ind: u32) -> String {
    let mut string = String::new();
    self.write_gd(&mut string, ind).expect("Could not write to string in Decl::to_gd");
    string
  }

}

impl Sourced for Decl {
  type Item = DeclF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &DeclF {
    &self.value
  }

}

impl ClassExtends {

  /// A simple unqualified name.
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::gdscript::decl::ClassExtends;
  /// let named = ClassExtends::named(String::from("Foobar"));
  /// assert_eq!(named, ClassExtends::Qualified(vec!(String::from("Foobar"))));
  /// ```
  pub fn named(name: String) -> ClassExtends {
    ClassExtends::Qualified(vec!(name))
  }

  /// Convert `self` to a string suitable as the tail end of a
  /// `extends` clause in GDScript.
  pub fn to_gd(&self) -> String {
    match self {
      ClassExtends::Qualified(names) => names.join("."),
    }
  }

}

impl TopLevelClass {

  /// Convert `self` to a string suitable for writing to a `.gd` file.
  pub fn to_gd(&self) -> String {
    let mut string = String::new();
    if let Some(name) = &self.name {
      writeln!(string, "class_name {}", name).expect("Could not write to string in TopLevelClass::to_gd");
    }
    writeln!(string, "extends {}", self.extends.to_gd()).expect("Could not write to string in TopLevelClass::to_gd");
    Decl::write_gd_decls(self.body.iter(), &empty_class_body(SourceOffset(0)), &mut string, 0)
      .expect("Could not write to string in TopLevelClass::to_gd");
    string
  }

}

impl From<Static> for bool {
  fn from(s: Static) -> bool {
    s == Static::IsStatic
  }
}

impl From<Onready> for bool {
  fn from(r: Onready) -> bool {
    r == Onready::Yes
  }
}

impl From<bool> for Static {
  fn from(b: bool) -> Static {
    if b { Static::IsStatic } else { Static::NonStatic }
  }
}

impl From<bool> for Onready {
  fn from(b: bool) -> Onready {
    if b { Onready::Yes } else { Onready::No }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::expr::{Expr, ExprF};
  use crate::pipeline::source::SourceOffset;

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  fn d(decl: DeclF) -> Decl {
    Decl::new(decl, SourceOffset::default())
  }

  #[test]
  fn var_and_const() {
    let expr = e(ExprF::from(10));
    assert_eq!(d(DeclF::VarDecl(None, Onready::No, String::from("foo"), None)).to_gd(0), "var foo\n");
    assert_eq!(d(DeclF::VarDecl(None, Onready::No, String::from("foo"), Some(expr.clone()))).to_gd(0), "var foo = 10\n");
    assert_eq!(d(DeclF::VarDecl(None, Onready::Yes, String::from("foo"), None)).to_gd(0), "onready var foo\n");
    assert_eq!(d(DeclF::VarDecl(None, Onready::Yes, String::from("foo"), Some(expr.clone()))).to_gd(0), "onready var foo = 10\n");
    assert_eq!(d(DeclF::ConstDecl(String::from("FOO"), expr.clone())).to_gd(0), "const FOO = 10\n");
  }

  #[test]
  fn exported_var() {
    let expr = e(ExprF::from(10));

    let export1 = Export { args: vec!(Expr::var("int", SourceOffset::default())) };
    assert_eq!(d(DeclF::VarDecl(Some(export1), Onready::No, String::from("foo"), None)).to_gd(0), "export(int) var foo\n");

    let export2 = Export { args: vec!() };
    assert_eq!(d(DeclF::VarDecl(Some(export2), Onready::No, String::from("foo"), None)).to_gd(0), "export var foo\n");

    let export3 = Export { args: vec!() };
    assert_eq!(d(DeclF::VarDecl(Some(export3), Onready::Yes, String::from("foo"), None)).to_gd(0), "export onready var foo\n");

    let export4 = Export { args: vec!(Expr::var("int", SourceOffset::default()), Expr::from_value(1, SourceOffset::default()), Expr::from_value(10, SourceOffset::default())) };
    assert_eq!(d(DeclF::VarDecl(Some(export4), Onready::No, String::from("foo"), Some(expr.clone()))).to_gd(0), "export(int, 1, 10) var foo = 10\n");
  }

  #[test]
  fn signal() {
    assert_eq!(d(DeclF::SignalDecl(String::from("signal_name"), ArgList::empty())).to_gd(0), "signal signal_name\n");
    assert_eq!(d(DeclF::SignalDecl(String::from("signal_name"), ArgList::required(vec!(String::from("a"), String::from("b"))))).to_gd(0), "signal signal_name(a, b)\n");
  }

  #[test]
  fn functions() {

    let decl1 = d(DeclF::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!()),
      body: vec!()
    }));
    assert_eq!(decl1.to_gd(0), "func foobar():\n    pass\n");

    let decl2 = d(DeclF::FnDecl(Static::IsStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!()),
      body: vec!()
    }));
    assert_eq!(decl2.to_gd(0), "static func foobar():\n    pass\n");

    let decl3 = d(DeclF::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!(String::from("arg1"))),
      body: vec!()
    }));
    assert_eq!(decl3.to_gd(0), "func foobar(arg1):\n    pass\n");

    let decl4 = d(DeclF::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!(String::from("arg1"), String::from("arg2"))),
      body: vec!()
    }));
    assert_eq!(decl4.to_gd(0), "func foobar(arg1, arg2):\n    pass\n");

    let decl5 = d(DeclF::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!(String::from("arg1"), String::from("arg2"))),
      body: vec!(Stmt::expr(e(ExprF::Var(String::from("function_body")))))
    }));
    assert_eq!(decl5.to_gd(0), "func foobar(arg1, arg2):\n    function_body\n");

  }

  #[test]
  fn init_functions() {

    let decl1 = d(DeclF::InitFnDecl(InitFnDecl {
      args: ArgList::required(vec!()),
      super_call: vec!(),
      body: vec!()
    }));
    assert_eq!(decl1.to_gd(0), "func _init():\n    pass\n");

    let decl2 = d(DeclF::InitFnDecl(InitFnDecl {
      args: ArgList::required(vec!(String::from("arg1"))),
      super_call: vec!(e(ExprF::Var(String::from("arg1"))), e(ExprF::from(8))),
      body: vec!()
    }));
    assert_eq!(decl2.to_gd(0), "func _init(arg1).(arg1, 8):\n    pass\n");

    let decl3 = d(DeclF::InitFnDecl(InitFnDecl {
      args: ArgList::required(vec!(String::from("arg1"), String::from("arg2"))),
      super_call: vec!(e(ExprF::Var(String::from("arg1"))), e(ExprF::from(8))),
      body: vec!()
    }));
    assert_eq!(decl3.to_gd(0), "func _init(arg1, arg2).(arg1, 8):\n    pass\n");

    let decl4 = d(DeclF::InitFnDecl(InitFnDecl {
      args: ArgList::required(vec!(String::from("arg1"), String::from("arg2"))),
      super_call: vec!(e(ExprF::Var(String::from("arg1"))), e(ExprF::from(8))),
      body: vec!(Stmt::expr(e(ExprF::Var(String::from("function_body")))))
    }));
    assert_eq!(decl4.to_gd(0), "func _init(arg1, arg2).(arg1, 8):\n    function_body\n");

  }

  #[test]
  fn classes() {

    let sample_function = d(DeclF::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("sample"),
      args: ArgList::required(vec!()),
      body: vec!()
    }));

    let decl1 = d(DeclF::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::named(String::from("ParentClass")),
      body: vec!(),
    }));
    assert_eq!(decl1.to_gd(0), "class MyClass extends ParentClass:\n    func _init():\n        pass\n");

    let decl2 = d(DeclF::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::named(String::from("ParentClass")),
      body: vec!(
        d(DeclF::VarDecl(None, Onready::No, String::from("variable"), None)),
      ),
    }));
    assert_eq!(decl2.to_gd(0), "class MyClass extends ParentClass:\n    var variable\n");

    let decl3 = d(DeclF::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::named(String::from("ParentClass")),
      body: vec!(
        d(DeclF::VarDecl(None, Onready::No, String::from("variable"), None)),
        sample_function.clone(),
      ),
    }));
    assert_eq!(decl3.to_gd(0), "class MyClass extends ParentClass:\n    var variable\n    func sample():\n        pass\n");

    let decl4 = d(DeclF::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::named(String::from("ParentClass")),
      body: vec!(
        d(DeclF::VarDecl(None, Onready::Yes, String::from("variable"), None)),
        sample_function.clone(),
      ),
    }));
    assert_eq!(decl4.to_gd(0), "class MyClass extends ParentClass:\n    onready var variable\n    func sample():\n        pass\n");

  }

  #[test]
  fn enums() {
    let decl1 = d(DeclF::EnumDecl(EnumDecl {
      name: None,
      clauses: vec!(),
    }));
    assert_eq!(decl1.to_gd(0), "enum {\n}\n");

    let decl2 = d(DeclF::EnumDecl(EnumDecl {
      name: None,
      clauses: vec!((String::from("Value1"), None), (String::from("Value2"), None)),
    }));
    assert_eq!(decl2.to_gd(0), "enum {\n    Value1,\n    Value2,\n}\n");

    let decl3 = d(DeclF::EnumDecl(EnumDecl {
      name: None,
      clauses: vec!((String::from("Value1"), Some(e(ExprF::from(99)))), (String::from("Value2"), None)),
    }));
    assert_eq!(decl3.to_gd(0), "enum {\n    Value1 = 99,\n    Value2,\n}\n");

    let decl4 = d(DeclF::EnumDecl(EnumDecl {
      name: Some(String::from("EnumName")),
      clauses: vec!((String::from("Value1"), Some(e(ExprF::from(99)))), (String::from("Value2"), None)),
    }));
    assert_eq!(decl4.to_gd(0), "enum EnumName {\n    Value1 = 99,\n    Value2,\n}\n");
  }

  #[test]
  fn decl_names() {
    let decl1 = d(DeclF::VarDecl(None, Onready::No, String::from("abc"), None));
    assert_eq!(decl1.name(), Some("abc"));

    let decl2 = d(DeclF::ConstDecl(String::from("MY_CONST"), e(ExprF::from(10))));
    assert_eq!(decl2.name(), Some("MY_CONST"));

    let decl3 = d(DeclF::ClassDecl(ClassDecl { name: String::from("MyClass"),
                                               extends: ClassExtends::Qualified(vec!(String::from("Node"))),
                                               body: vec!() }));
    assert_eq!(decl3.name(), Some("MyClass"));

    let decl4 = d(DeclF::InitFnDecl(InitFnDecl { args: ArgList::empty(), super_call: vec!(), body: vec!() }));
    assert_eq!(decl4.name(), Some("_init"));

    let decl5 = d(DeclF::FnDecl(Static::NonStatic, FnDecl { name: String::from("foobar"), args: ArgList::empty(), body: vec!() }));
    assert_eq!(decl5.name(), Some("foobar"));

    let decl6 = d(DeclF::EnumDecl(EnumDecl { name: Some(String::from("MyEnum")), clauses: vec!() }));
    assert_eq!(decl6.name(), Some("MyEnum"));

    let decl7 = d(DeclF::EnumDecl(EnumDecl { name: None, clauses: vec!() }));
    assert_eq!(decl7.name(), None);

    let decl8 = d(DeclF::SignalDecl(String::from("signal_emitted"), ArgList::empty()));
    assert_eq!(decl8.name(), Some("signal_emitted"));
  }

}
