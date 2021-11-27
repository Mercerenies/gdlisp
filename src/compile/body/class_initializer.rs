
//! Builders for class initializers.

use crate::gdscript::decl::{Decl, DeclF, ClassDecl, InitFnDecl, FnDecl, Static};
use crate::gdscript::stmt::Stmt;
use crate::gdscript::arglist::ArgList;
use crate::gdscript::library::READY_NAME;
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
  /// The builder for `_ready`.
  pub ready_builder: StmtBuilder,
  /// Any declarations which have been absorbed by another builder.
  /// This field exists for compatibility with [`HasDecls`] so that
  /// this builder can be composed with others easily.
  pub other_helpers: Vec<Decl>,
}

/// This is the eventual result of a [`ClassInitBuilder`]. It contains
/// information that can be used to modify a [`ClassDecl`].
#[derive(Default, Clone, Debug)]
pub struct ClassInit {
  /// Statements to be prepended to the class' `_init` method.
  pub init: Vec<Stmt>,
  /// Statements to be prepended to the class' `_ready` method.
  pub ready: Vec<Stmt>,
}

/// The time that an instance variable should be initialized.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum InitTime {
  /// The variable is initialized during `_init()`, i.e. when the
  /// instance itself is first constructed.
  Init,
  /// The variable is initialized during `_ready()`, i.e. when the
  /// instance is added to the scene tree.
  Ready,
}

impl ClassInitBuilder {

  /// A new, empty initializer builder. Equivalent to
  /// `ClassInitBuilder::default()`.
  pub fn new() -> ClassInitBuilder {
    ClassInitBuilder::default()
  }

  pub fn builder_for(&mut self, init_time: InitTime) -> &mut StmtBuilder {
    match init_time {
      InitTime::Init => &mut self.init_builder,
      InitTime::Ready => &mut self.ready_builder,
    }
  }

  /// Builds the builder into a [`ClassInit`].
  pub fn build(self) -> (ClassInit, Vec<Decl>) {
    let mut helpers = self.other_helpers;

    // Initializer
    let (init, mut init_helpers) = self.init_builder.build();
    helpers.append(&mut init_helpers);

    // Ready
    let (ready, mut ready_helpers) = self.ready_builder.build();
    helpers.append(&mut ready_helpers);

    let initializer = ClassInit {
      init,
      ready,
    };

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

impl Default for InitTime {

  /// [`InitTime::Init`] is the "default" initialization time, if no
  /// modifiers are applied.
  fn default() -> InitTime {
    InitTime::Init
  }

}

impl ClassInit {

  // TODO Is it possible for variables to be shadowed by the
  // initializer arguments here? Should we throw an error in that
  // case?
  pub fn apply(mut self, class: &mut ClassDecl) {

    // Initializer
    if !self.init.is_empty() {
      let initializer = ClassInit::find_initializer(&mut class.body);
      self.init.append(&mut initializer.body);
      initializer.body = self.init;
    }

    // Ready
    if !self.ready.is_empty() {
      let ready = ClassInit::find_ready(&mut class.body);
      self.ready.append(&mut ready.body);
      ready.body = self.ready;
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

  /// Find the _ready function defined on the current class. If no
  /// _ready function is defined, then an empty one is created,
  /// appended to the class, and returned.
  pub fn find_ready(decls: &mut Vec<Decl>) -> &mut FnDecl {
    let decl = find_or_else_mut(decls, ClassInit::empty_ready, |d| matches!(&d.value, DeclF::FnDecl(_, f) if f.name == READY_NAME));
    if let DeclF::FnDecl(_, fdecl) = &mut decl.value {
      fdecl
    } else {
      panic!("Internal error in ClassInit::find_ready")
    }
  }

  fn empty_ready() -> Decl {
    Decl::new(DeclF::FnDecl(Static::NonStatic, FnDecl {
      name: READY_NAME.to_string(),
      args: ArgList::empty(),
      body: vec!(),
    }), SourceOffset(0))
  }

}
