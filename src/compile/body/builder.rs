
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::stmt::Stmt;

#[derive(Clone)]
pub struct CodeBuilder {
  toplevel: decl::TopLevelClass,
}

#[derive(Clone, Default)]
pub struct StmtBuilder {
  body: Vec<Stmt>,
  helpers: Vec<Decl>,
}

pub trait HasDecls {
  fn add_decl(&mut self, decl: Decl);
}

impl CodeBuilder {

  pub fn new(extends: decl::ClassExtends) -> CodeBuilder {
    CodeBuilder {
      toplevel: decl::TopLevelClass {
        name: None,
        extends: extends,
        body: vec!(),
      }
    }
  }

  pub fn named(&mut self, name: String) {
    self.toplevel.name = Some(name);
  }

  pub fn build(self) -> decl::TopLevelClass {
    self.toplevel
  }

}

impl HasDecls for CodeBuilder {
  fn add_decl(&mut self, decl: Decl) {
    self.toplevel.body.push(decl);
  }
}

impl StmtBuilder {

  pub fn new() -> StmtBuilder {
    StmtBuilder::default()
  }

  pub fn append(&mut self, stmt: Stmt) {
    self.body.push(stmt);
  }

  pub fn append_all(&mut self, stmts: &mut dyn Iterator<Item=Stmt>) {
    for stmt in stmts {
      self.append(stmt)
    }
  }

  pub fn add_helper(&mut self, decl: Decl) {
    self.helpers.push(decl);
  }

  pub fn build(self) -> (Vec<Stmt>, Vec<Decl>) {
    (self.body, self.helpers)
  }

  // Builds the StmtBuilder, passing any helper declarations onto the
  // subsequent builder.
  pub fn build_into(self, other: &mut impl HasDecls) -> Vec<Stmt> {
    let (body, helpers) = self.build();
    for h in helpers {
      other.add_decl(h);
    }
    body
  }

}

impl HasDecls for StmtBuilder {
  fn add_decl(&mut self, decl: Decl) {
    self.add_helper(decl);
  }
}
