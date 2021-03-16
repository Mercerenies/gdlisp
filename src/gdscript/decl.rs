
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::indent;
use crate::gdscript::arglist::ArgList;

use std::fmt::{self, Write};

// TODO _init has some special syntax that we need to be prepared to handle.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decl {
  VarDecl(Option<Export>, String, Option<Expr>),
  ConstDecl(String, Expr),
  ClassDecl(ClassDecl),
  FnDecl(Static, FnDecl),
  SignalDecl(String, ArgList),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassDecl {
  pub name: String,
  pub extends: ClassExtends,
  pub body: Vec<Decl>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TopLevelClass {
  pub name: Option<String>, // The top-level class is not required to have a name.
  pub extends: ClassExtends,
  pub body: Vec<Decl>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassExtends {
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
pub struct Export {
  pub args: Vec<Expr>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Static {
  NonStatic, IsStatic,
}

fn empty_class_body() -> Decl {
  Decl::FnDecl(Static::NonStatic, FnDecl {
    name: String::from("_init"),
    args: ArgList::empty(),
    body: vec!(),
  })
}

impl Decl {

  pub fn write_gd<W : fmt::Write>(&self, w: &mut W, ind: u32) -> Result<(), fmt::Error> {
    indent(w, ind)?;
    match self {
      Decl::VarDecl(export, name, value) => {
        if let Some(export) = export {
          if export.args.is_empty() {
            write!(w, "export ")?;
          } else {
            write!(w, "export({}) ", export.args.iter().map(|x| x.to_gd()).collect::<Vec<_>>().join(", "))?;
          }
        }
        write!(w, "var {}", name)?;
        match value {
          None => writeln!(w),
          Some(value) => writeln!(w, " = {}", value.to_gd()),
        }
      }
      Decl::ConstDecl(name, value) => {
        writeln!(w, "const {} = {}", name, value.to_gd())
      }
      Decl::ClassDecl(ClassDecl { name, extends, body }) => {
        writeln!(w, "class {} extends {}:", name, extends.to_gd())?;
        Decl::write_gd_decls(body, &empty_class_body(), w, ind + 4)
      }
      Decl::FnDecl(stat, FnDecl { name, args, body }) => {
        if *stat == Static::IsStatic {
          write!(w, "static ")?;
        }
        writeln!(w, "func {}({}):", name, args.to_gd())?;
        Stmt::write_gd_stmts(body, w, ind + 4)
      }
      Decl::SignalDecl(name, args) => {
        if args.is_empty() {
          writeln!(w, "signal {}", name)
        } else {
          writeln!(w, "signal {}({})", name, args.to_gd())
        }
      }
    }
  }

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

  pub fn to_gd(&self, ind: u32) -> String {
    let mut string = String::new();
    self.write_gd(&mut string, ind).expect("Could not write to string in Decl::to_gd");
    string
  }

}

impl ClassExtends {

  pub fn named(name: String) -> ClassExtends {
    ClassExtends::Qualified(vec!(name))
  }

  pub fn to_gd(&self) -> String {
    match self {
      ClassExtends::Qualified(names) => names.join("."),
    }
  }

}

impl TopLevelClass {

  pub fn to_gd(&self) -> String {
    let mut string = String::new();
    if let Some(name) = &self.name {
      writeln!(string, "class_name {}", name).expect("Could not write to string in TopLevelClass::to_gd");
    }
    writeln!(string, "extends {}", self.extends.to_gd()).expect("Could not write to string in TopLevelClass::to_gd");
    Decl::write_gd_decls(self.body.iter(), &empty_class_body(), &mut string, 0)
      .expect("Could not write to string in TopLevelClass::to_gd");
    string
  }

}

impl From<Static> for bool {
  fn from(s: Static) -> bool {
    s == Static::IsStatic
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn var_and_const() {
    let expr = Expr::from(10);
    assert_eq!(Decl::VarDecl(None, String::from("foo"), None).to_gd(0), "var foo\n");
    assert_eq!(Decl::VarDecl(None, String::from("foo"), Some(expr.clone())).to_gd(0), "var foo = 10\n");
    assert_eq!(Decl::ConstDecl(String::from("FOO"), expr.clone()).to_gd(0), "const FOO = 10\n");
  }

  #[test]
  fn exported_var() {
    let expr = Expr::from(10);

    let export1 = Export { args: vec!(Expr::var("int")) };
    assert_eq!(Decl::VarDecl(Some(export1), String::from("foo"), None).to_gd(0), "export(int) var foo\n");

    let export2 = Export { args: vec!() };
    assert_eq!(Decl::VarDecl(Some(export2), String::from("foo"), None).to_gd(0), "export var foo\n");

    let export3 = Export { args: vec!(Expr::var("int"), Expr::from(1), Expr::from(10)) };
    assert_eq!(Decl::VarDecl(Some(export3), String::from("foo"), Some(expr.clone())).to_gd(0), "export(int, 1, 10) var foo = 10\n");
  }

  #[test]
  fn signal() {
    assert_eq!(Decl::SignalDecl(String::from("signal_name"), ArgList::empty()).to_gd(0), "signal signal_name\n");
    assert_eq!(Decl::SignalDecl(String::from("signal_name"), ArgList::required(vec!(String::from("a"), String::from("b")))).to_gd(0), "signal signal_name(a, b)\n");
  }

  #[test]
  fn functions() {

    let decl1 = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!()),
      body: vec!()
    });
    assert_eq!(decl1.to_gd(0), "func foobar():\n    pass\n");

    let decl2 = Decl::FnDecl(Static::IsStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!()),
      body: vec!()
    });
    assert_eq!(decl2.to_gd(0), "static func foobar():\n    pass\n");

    let decl3 = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!(String::from("arg1"))),
      body: vec!()
    });
    assert_eq!(decl3.to_gd(0), "func foobar(arg1):\n    pass\n");

    let decl4 = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!(String::from("arg1"), String::from("arg2"))),
      body: vec!()
    });
    assert_eq!(decl4.to_gd(0), "func foobar(arg1, arg2):\n    pass\n");

    let decl5 = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: ArgList::required(vec!(String::from("arg1"), String::from("arg2"))),
      body: vec!(Stmt::Expr(Expr::Var(String::from("function_body"))))
    });
    assert_eq!(decl5.to_gd(0), "func foobar(arg1, arg2):\n    function_body\n");

  }

  #[test]
  fn classes() {

    let sample_function = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("sample"),
      args: ArgList::required(vec!()),
      body: vec!()
    });

    let decl1 = Decl::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::named(String::from("ParentClass")),
      body: vec!(),
    });
    assert_eq!(decl1.to_gd(0), "class MyClass extends ParentClass:\n    func _init():\n        pass\n");

    let decl2 = Decl::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::named(String::from("ParentClass")),
      body: vec!(
        Decl::VarDecl(None, String::from("variable"), None),
      ),
    });
    assert_eq!(decl2.to_gd(0), "class MyClass extends ParentClass:\n    var variable\n");

    let decl3 = Decl::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::named(String::from("ParentClass")),
      body: vec!(
        Decl::VarDecl(None, String::from("variable"), None),
        sample_function.clone(),
      ),
    });
    assert_eq!(decl3.to_gd(0), "class MyClass extends ParentClass:\n    var variable\n    func sample():\n        pass\n");

  }

}
