
use crate::gdscript::decl::{self, Decl};
use crate::gdscript::stmt::Stmt;

#[derive(Clone)]
pub struct CodeBuilder {
  toplevel: decl::TopLevelClass,
}

#[derive(Clone)]
pub struct StmtBuilder {
  body: Vec<Stmt>,
  helpers: Vec<Decl>,
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

impl StmtBuilder {

  pub fn new() -> StmtBuilder {
    StmtBuilder {
      body: vec!(),
      helpers: vec!(),
    }
  }

  pub fn append(&mut self, stmt: Stmt) {
    self.body.push(stmt);
  }

  pub fn add_helper(&mut self, decl: Decl) {
    self.helpers.push(decl);
  }

  pub fn build(self) -> (Vec<Stmt>, Vec<Decl>) {
    (self.body, self.helpers)
  }

  // Builds the StmtBuilder, passing any helper declarations onto the
  // subsequent builder.
  pub fn build_into(self, other: &mut StmtBuilder) -> Vec<Stmt> {
    let (body, helpers) = self.build();
    for h in helpers {
      other.add_helper(h);
    }
    body
  }

}
