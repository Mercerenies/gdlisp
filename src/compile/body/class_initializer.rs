
//! Builders for class initializers.

use crate::gdscript::decl::{Decl, DeclF, ClassDecl, InitFnDecl, FnDecl, Static};
use crate::gdscript::stmt::Stmt;
use crate::gdscript::arglist::ArgList;
use crate::gdscript::library::READY_NAME;
use crate::compile::error::{Error, ErrorF};
use crate::compile::names::fresh::FreshNameGenerator;
use crate::util::find_or_else_mut;
use crate::pipeline::source::SourceOffset;
use super::builder::{StmtBuilder, HasDecls};
use super::synthetic_field::{SyntheticField, Getter, Setter};
use super::super_proxy::SuperProxy;

use std::mem;
use std::collections::HashMap;

/// A builder for a GDScript class.
///
/// This builder allows certain pieces of functionality for the class,
/// such as `_init` and `_ready`, to build up over the course of
/// compilation and eventually build it into the resulting class at
/// the end.
#[derive(Default, Clone)]
pub struct ClassBuilder {
  /// The builder for `_init`.
  init_builder: StmtBuilder,
  /// The builder for `_ready`.
  ready_builder: StmtBuilder,
  /// The builder for any synthetic fields generated as a result of
  /// `get` and `set` declarations.
  synthetic_fields: Vec<SyntheticField>,
  /// The builder for any supermethod proxies generated as a result of
  /// `super` method calls. Note that a `super` constructor call is an
  /// entirely different mechanism which is parsed into the syntax
  /// earlier than other `super` method calls and is not included
  /// here.
  super_proxies: Vec<SuperProxy>,
  /// Any declarations which have been absorbed by another builder.
  /// This field exists for compatibility with [`HasDecls`] so that
  /// this builder can be composed with others easily.
  other_helpers: Vec<Decl>,
}

/// This is the eventual result of a [`ClassBuilder`]. It contains
/// information that can be used to modify a [`ClassDecl`].
#[derive(Default, Clone, Debug)]
pub struct ClassInit {
  /// Statements to be prepended to the class' `_init` method.
  init: Vec<Stmt>,
  /// Statements to be prepended to the class' `_ready` method.
  ready: Vec<Stmt>,
  /// Proxy fields to be generated with appropriate `setget`
  /// declarations.
  synthetic_fields: Vec<SyntheticField>,
  /// Proxy methods to be generated for supermethod calls.
  super_proxies: Vec<SuperProxy>,
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

impl ClassBuilder {

  /// A new, empty initializer builder. Equivalent to
  /// `ClassBuilder::default()`.
  pub fn new() -> ClassBuilder {
    ClassBuilder::default()
  }

  /// Returns the builder object for either `_init` or `_ready`,
  /// depending on the initialization time argument.
  pub fn builder_for(&mut self, init_time: InitTime) -> &mut StmtBuilder {
    match init_time {
      InitTime::Init => &mut self.init_builder,
      InitTime::Ready => &mut self.ready_builder,
    }
  }

  /// Declares a getter for the given proxy field. If a getter already
  /// exists, this method overwrites it and returns the old one.
  ///
  /// Note: This method takes the field name as argument, not the
  /// getter name. The field name will be prefixed appropriately (as
  /// per [`Getter::method_name`]) to determine the field name.
  pub fn declare_getter_for(&mut self, field_name: String) -> Option<String> {
    let method_name = Getter::method_name(&field_name);
    for field in &mut self.synthetic_fields {
      if field.name == field_name {
        return mem::replace(&mut field.getter, Some(method_name));
      }
    }
    // Name wasn't found at all, so add a new one to the end.
    self.synthetic_fields.push(SyntheticField {
      name: field_name,
      getter: Some(method_name),
      setter: None,
    });
    None
  }

  /// Declares a setter for the given proxy field. If a setter already
  /// exists, this method overwrites it and returns the old one.
  ///
  /// Note: This method takes the field name as argument, not the
  /// setter name. The field name will be prefixed appropriately (as
  /// per [`Setter::method_name`]) to determine the field name.
  pub fn declare_setter_for(&mut self, field_name: String) -> Option<String> {
    let method_name = Setter::method_name(&field_name);
    for field in &mut self.synthetic_fields {
      if field.name == field_name {
        return mem::replace(&mut field.setter, Some(method_name));
      }
    }
    // Name wasn't found at all, so add a new one to the end.
    self.synthetic_fields.push(SyntheticField {
      name: field_name,
      getter: None,
      setter: Some(method_name),
    });
    None
  }

  /// Declares a supermethod proxy which will delegate to the
  /// supermethod with the given name. Returns the name of the new
  /// proxy method we've generated.
  pub fn declare_super_proxy(&mut self, gen: &mut FreshNameGenerator, super_name: String, args: usize, pos: SourceOffset) -> String {
    let proxy = SuperProxy::generate(gen, super_name, args, pos);
    let name = proxy.name.clone();
    self.super_proxies.push(proxy);
    name
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
      synthetic_fields: self.synthetic_fields,
      super_proxies: self.super_proxies,
    };

    (initializer, helpers)
  }

  /// Builds the builder into a [`ClassInit`], passing any helper
  /// declarations onto the enclosing builder. See
  /// [`StmtBuilder::build_into`] for a summary of why this method
  /// might be preferred over [`ClassBuilder::build`].
  pub fn build_into(self, other: &mut impl HasDecls) -> ClassInit {
    let (body, helpers) = self.build();
    for h in helpers {
      other.add_decl(h);
    }
    body
  }

}

impl HasDecls for ClassBuilder {

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

  /// Applies the initializer information to the given class
  /// declaration. The class declaration is mutated in-place. If an
  /// error occurs, then the class declaration is left in a valid but
  /// unspecified state.
  pub fn apply(mut self, class: &mut ClassDecl, pos: SourceOffset) -> Result<(), Error> {

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

    // Proxy fields for getters and setters
    ClassInit::apply_proxy_fields(self.synthetic_fields, class, pos)?;

    // Super-call proxy methods
    for method in self.super_proxies {
      let fn_decl = FnDecl::from(method);
      class.body.push(Decl::new(DeclF::FnDecl(Static::NonStatic, fn_decl), pos));
    }

    Ok(())
  }

  /// Generates all of the proxy fields associated with the synthetic
  /// field collection. In case of a conflict with an existing (non-proxy)
  /// field, an error is raised. In that case, the class declaration
  /// is left in a valid but unspecified state.
  fn apply_proxy_fields(fields: Vec<SyntheticField>, class: &mut ClassDecl, pos: SourceOffset) -> Result<(), Error> {
    let existing_fields = ClassInit::all_var_decls(class);

    for field in fields {
      let var_decl = field.into_field();
      if let Some(pos) = existing_fields.get(&*var_decl.name) {
        return Err(Error::new(ErrorF::FieldAccessorConflict(var_decl.name), *pos));
      }
      class.body.push(Decl::new(DeclF::VarDecl(var_decl), pos));
    }

    Ok(())
  }

  /// Returns a collection of all of the variable declaration names in
  /// the given class.
  fn all_var_decls(class: &ClassDecl) -> HashMap<String, SourceOffset> {
    let mut acc = HashMap::new();
    for decl in &class.body {
      if let DeclF::VarDecl(var_decl) = &decl.value {
        acc.insert(var_decl.name.to_owned(), decl.pos);
      }
    }
    acc
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
