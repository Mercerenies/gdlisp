
//! Builders for class initializers.

use crate::gdscript::decl::{Decl, DeclF, ClassDecl, InitFnDecl};
use crate::gdscript::stmt::Stmt;
use crate::gdscript::arglist::ArgList;
use crate::util::find_or_else_mut;
use crate::pipeline::source::SourceOffset;
use super::builder::{StmtBuilder, HasDecls};

/// A builder for a class' `_init` method. This builder allows the
/// caller to build up `_init` over the course of compilation and
/// eventually build it into the resulting class at the end.
#[derive(Default, Clone)]
pub struct ClassInitBuilder {
  /// The builder for `_init`.
  pub init_builder: StmtBuilder,
  /// Any declarations which have been absorbed by another builder.
  /// This field exists for compatibility with [`HasDecls`] so that
  /// this builder can be composed with others easily.
  pub other_helpers: Vec<Decl>,
}

/// This is the eventual result of a [`ClassInitBuilder`]. It contains
/// information that can be used to modify a [`ClassDecl`].
#[derive(Default, Clone)]
pub struct ClassInit {
  /// Statements to be prepended to the class' `_init` method.
  pub init: Vec<Stmt>,
}

impl ClassInitBuilder {

  /// A new, empty initializer builder. Equivalent to
  /// `ClassInitBuilder::default()`.
  pub fn new() -> ClassInitBuilder {
    ClassInitBuilder::default()
  }

  /// Builds the builder into a [`ClassInit`].
  pub fn build(self) -> (ClassInit, Vec<Decl>) {
    let mut helpers = self.other_helpers;

    // Initializer
    let (init, mut init_helpers) = self.init_builder.build();
    let initializer = ClassInit {
      init,
    };
    helpers.append(&mut init_helpers);

    (initializer, helpers)
  }

  /// Builds the builder into a [`ClassInit`], passing any helper
  /// declarations onto the enclosing builder. See
  /// [`StmtBuilder::build_into`] for a summary of why this method
  /// might be preferred over [`ClassInitBuilder::build`].
  pub fn build_into(self, other: &mut impl HasDecls) -> ClassInit {
    let (body, helpers) = self.build();
    for h in helpers {
      other.add_decl(h);
    }
    body
  }

}

impl HasDecls for ClassInitBuilder {

  fn add_decl(&mut self, decl: Decl) {
    self.other_helpers.push(decl);
  }

}

impl ClassInit {

  // TODO Protect any variables which might be shadowed by the initializer (////)
  pub fn apply(mut self, class: &mut ClassDecl) {
    // Initializer
    {
      let initializer = ClassInit::find_initializer(&mut class.body);
      self.init.append(&mut initializer.body);
      initializer.body = self.init;
    }
  }

  /// Find the initializer function defined on the current class. If
  /// no initializer is defined, then an empty one is created,
  /// appended to the class, and returned.
  pub fn find_initializer(decls: &mut Vec<Decl>) -> &mut InitFnDecl {
    let init = find_or_else_mut(decls, ClassInit::empty_initializer, |d| matches!(d.value, DeclF::InitFnDecl(_)));
    if let DeclF::InitFnDecl(decl) = &mut init.value {
      decl
    } else {
      panic!("Internal error in ClassInit::find_initializer")
    }
  }

  fn empty_initializer() -> Decl {
    Decl::new(DeclF::InitFnDecl(InitFnDecl {
      args: ArgList::empty(),
      super_call: vec!(),
      body: vec!(),
    }), SourceOffset(0))
  }

}
