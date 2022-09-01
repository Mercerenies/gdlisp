
//! Builders for GDScript code.

use crate::gdscript::decl::{self, Decl};
use crate::gdscript::stmt::Stmt;
use crate::gdscript::class_extends::ClassExtends;

/// A builder for an entire GDScript source file.
#[derive(Clone)]
pub struct CodeBuilder {
  toplevel: decl::TopLevelClass,
}

/// A builder for a sequence of GDScript statements.
///
/// This structure also retains a list of "helper" declarations
/// necessary for the statements to run correctly. These helper
/// declarations should eventually be hoisted to the top-level scope
/// containing the statements. These declarations include things like
/// inner classes for implementing lambdas and other advanced GDLisp
/// features.
#[derive(Clone, Default)]
pub struct StmtBuilder {
  body: Vec<Stmt>,
  helpers: Vec<Decl>,
}

/// Trait for builder structures which can have declarations added to
/// them in some meaningful way.
///
/// This trait allows [`StmtBuilder::build_into`] to fold neatly into
/// another [`StmtBuilder`] or into any other kind of builder (such as
/// [`CodeBuilder`]) in a nice, uniform way.
pub trait HasDecls {

  /// Adds a declaration to the builder.
  fn add_decl(&mut self, decl: Decl);

  /// Adds several declarations to the builder.
  fn add_decls(&mut self, decls: impl IntoIterator<Item=Decl>) {
    for decl in decls {
      self.add_decl(decl);
    }
  }

}

impl CodeBuilder {

  /// Construct a new builder for a class which extends the given
  /// class.
  ///
  /// The builder begins representing a class with no declarations and
  /// no name.
  pub fn new(extends: ClassExtends) -> CodeBuilder {
    CodeBuilder {
      toplevel: decl::TopLevelClass {
        name: None,
        extends: extends,
        body: vec!(),
      }
    }
  }

  /// Give the class referenced by this builder a name, i.e. a
  /// `class_name` header in GDScript.
  ///
  /// If the class already has a name, it is overwritten.
  pub fn named(&mut self, name: String) {
    self.toplevel.name = Some(name);
  }

  /// Change what the class extends.
  pub fn extends(&mut self, extends: ClassExtends) {
    self.toplevel.extends = extends;
  }

  /// Consume this builder and produce a top-level class declaration.
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

  /// A new builder containing no statements.
  pub fn new() -> StmtBuilder {
    StmtBuilder::default()
  }

  /// Append a single statement to the builder.
  pub fn append(&mut self, stmt: Stmt) {
    self.body.push(stmt);
  }

  /// Append a collection of statements to the builder in order.
  pub fn append_all(&mut self, stmts: &mut dyn Iterator<Item=Stmt>) {
    for stmt in stmts {
      self.append(stmt)
    }
  }

  /// Append a declaration to the builder's collection of helper
  /// declarations.
  pub fn add_helper(&mut self, decl: Decl) {
    self.helpers.push(decl);
  }

  /// Consume the builder and produce its statements and necessary
  /// helper declarations.
  ///
  /// The caller is responsible for ensuring that the helper
  /// declarations are safely transmitted to the enclosing scope.
  /// Often, [`StmtBuilder::build_into`] can be used to do this automatically.
  pub fn build(self) -> (Vec<Stmt>, Vec<Decl>) {
    (self.body, self.helpers)
  }

  /// Consume the builder, passing any helper declarations onto the
  /// subsequent builder.
  ///
  /// This is more useful than [`StmtBuilder::build`] if you have access to the
  /// builder (often, but not necessary, a `StmtBuilder`) representing
  /// the enclosing scope. The helper declarations from `self` are
  /// added (via [`HasDecls::add_decl`]) to `other`, and the
  /// statements from `self` are returned.
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
