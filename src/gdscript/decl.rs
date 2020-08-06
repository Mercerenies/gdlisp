
use crate::gdscript::expr::Expr;
use crate::gdscript::stmt::Stmt;
use crate::gdscript::indent;

use std::fmt;

#[derive(Clone, Debug)]
pub enum Decl {
  VarDecl(String, Option<Expr>),
  ConstDecl(String, Expr),
  ClassDecl(ClassDecl),
  FnDecl(Static, FnDecl),
}

#[derive(Clone, Debug)]
pub struct ClassDecl {
  name: String,
  extends: ClassExtends,
  body: Vec<Decl>,
}

#[derive(Clone, Debug)]
pub struct TopLevelClass {
  name: Option<String>, // The top-level class is not required to have a name.
  extends: ClassExtends,
  body: Vec<Decl>,
}

#[derive(Clone, Debug)]
pub enum ClassExtends {
  Named(String), // StringLit(String), // TODO Support string literals (once we have them in general)
}

// TODO Support default arguments
#[derive(Clone, Debug)]
pub struct FnDecl {
  name: String,
  args: Vec<String>,
  body: Vec<Stmt>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Static {
  NonStatic, IsStatic,
}

fn empty_class_body() -> Decl {
  Decl::FnDecl(Static::NonStatic, FnDecl {
    name: String::from("_init"),
    args: vec!(),
    body: vec!(),
  })
}

impl Decl {

  pub fn write_gd<W : fmt::Write>(&self, w: &mut W, ind: u32) -> Result<(), fmt::Error> {
    indent(w, ind)?;
    match self {
      Decl::VarDecl(name, value) => {
        write!(w, "var {}", name)?;
        match value {
          None => write!(w, "\n"),
          Some(value) => write!(w, " = {}\n", value.to_gd()),
        }
      }
      Decl::ConstDecl(name, value) => {
        write!(w, "const {} = {}\n", name, value.to_gd())
      }
      Decl::ClassDecl(ClassDecl { name, extends, body }) => {
        write!(w, "class {} extends {}:\n", name, extends.to_gd())?;
        Decl::write_gd_decls(body, &empty_class_body(), w, ind + 4)
      }
      Decl::FnDecl(stat, FnDecl { name, args, body }) => {
        if *stat == Static::IsStatic {
          write!(w, "static ")?;
        }
        write!(w, "func {}({}):\n", name, args.join(", "))?;
        Stmt::write_gd_stmts(body, w, ind + 4)
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

  fn to_gd(&self) -> String {
    match self {
      ClassExtends::Named(name) => name.clone()
    }
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::literal::Literal;

  #[test]
  fn var_and_const() {
    let expr = Expr::Literal(Literal::Int(10));
    assert_eq!(Decl::VarDecl(String::from("foo"), None).to_gd(0), "var foo\n");
    assert_eq!(Decl::VarDecl(String::from("foo"), Some(expr.clone())).to_gd(0), "var foo = 10\n");
    assert_eq!(Decl::ConstDecl(String::from("FOO"), expr.clone()).to_gd(0), "const FOO = 10\n");
  }

  #[test]
  fn functions() {

    let decl1 = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: vec!(),
      body: vec!()
    });
    assert_eq!(decl1.to_gd(0), "func foobar():\n    pass\n");

    let decl2 = Decl::FnDecl(Static::IsStatic, FnDecl {
      name: String::from("foobar"),
      args: vec!(),
      body: vec!()
    });
    assert_eq!(decl2.to_gd(0), "static func foobar():\n    pass\n");

    let decl3 = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: vec!(String::from("arg1")),
      body: vec!()
    });
    assert_eq!(decl3.to_gd(0), "func foobar(arg1):\n    pass\n");

    let decl4 = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: vec!(String::from("arg1"), String::from("arg2")),
      body: vec!()
    });
    assert_eq!(decl4.to_gd(0), "func foobar(arg1, arg2):\n    pass\n");

    let decl5 = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("foobar"),
      args: vec!(String::from("arg1"), String::from("arg2")),
      body: vec!(Stmt::Expr(Expr::Var(String::from("function_body"))))
    });
    assert_eq!(decl5.to_gd(0), "func foobar(arg1, arg2):\n    function_body\n");

  }

  #[test]
  fn classes() {

    let sample_function = Decl::FnDecl(Static::NonStatic, FnDecl {
      name: String::from("sample"),
      args: vec!(),
      body: vec!()
    });

    let decl1 = Decl::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::Named(String::from("ParentClass")),
      body: vec!(),
    });
    assert_eq!(decl1.to_gd(0), "class MyClass extends ParentClass:\n    func _init():\n        pass\n");

    let decl2 = Decl::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::Named(String::from("ParentClass")),
      body: vec!(
        Decl::VarDecl(String::from("variable"), None),
      ),
    });
    assert_eq!(decl2.to_gd(0), "class MyClass extends ParentClass:\n    var variable\n");

    let decl3 = Decl::ClassDecl(ClassDecl {
      name: String::from("MyClass"),
      extends: ClassExtends::Named(String::from("ParentClass")),
      body: vec!(
        Decl::VarDecl(String::from("variable"), None),
        sample_function.clone(),
      ),
    });
    assert_eq!(decl3.to_gd(0), "class MyClass extends ParentClass:\n    var variable\n    func sample():\n        pass\n");

  }

}
