// Copyright 2023 Silvio Mayolo
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

//! Provides the [`SyntheticField`] type, for generating proxy fields
//! on GDScript classes.

use crate::gdscript::decl::{FnDecl, VarDecl, Setget};
use crate::gdscript::arglist::ArgList;
use crate::gdscript::stmt::Stmt;

/// A synthetic field will be generated on the resulting class with
/// the given getter and setter. Both parts of the synthetic field are
/// optional.
#[derive(Default, Clone, Debug)]
pub struct SyntheticField {
  /// The name of the field.
  pub name: String,
  /// The name of the getter method for the field, if present.
  pub getter: Option<String>,
  /// The name of the setter method for the field, if present.
  pub setter: Option<String>,
}

/// A getter method is a zero-argument GDScript function which will
/// return the value of the proxy field.
#[derive(Clone, Debug)]
pub struct Getter {
  name: String,
  body: Vec<Stmt>,
}

/// A setter method is a one-argument GDScript function which will set
/// the value of the proxy field.
#[derive(Clone, Debug)]
pub struct Setter {
  name: String,
  argument_name: String,
  body: Vec<Stmt>,
}

impl SyntheticField {

  pub fn new() -> SyntheticField {
    SyntheticField::default()
  }

  /// Converts the synthetic field into a variable declaration. The
  /// resulting variable has no default value and no modifiers other
  /// than the `setget` modifier.
  pub fn into_field(self) -> VarDecl {
    VarDecl::simple(self.name).setget(Setget {
      setter: self.setter,
      getter: self.getter,
    })
  }

}

impl Getter {

  /// Construct a getter from a method name and a method body.
  pub fn new(name: String, body: Vec<Stmt>) -> Getter {
    Getter { name, body }
  }

  /// By convention, the name of a GDLisp getter is produced using a
  /// set prefix to distinguish it. Given a proxy field name, this
  /// function returns the conventional getter name for that field.
  pub fn method_name(field_name: &str) -> String {
    format!("__gdlisp_get_{}", field_name)
  }

}

impl Setter {

  /// Construct a setter from a method name, an argument name, and a method body.
  pub fn new(name: String, argument_name: String, body: Vec<Stmt>) -> Setter {
    Setter { name, argument_name, body }
  }

  /// By convention, the name of a GDLisp setter is produced using a
  /// set prefix to distinguish it. Given a proxy field name, this
  /// function returns the conventional setter name for that field.
  pub fn method_name(field_name: &str) -> String {
    format!("__gdlisp_set_{}", field_name)
  }

}

impl From<Getter> for FnDecl {

  fn from(getter: Getter) -> FnDecl {
    FnDecl {
      name: getter.name,
      args: ArgList::empty(),
      body: getter.body,
    }
  }

}

impl From<Setter> for FnDecl {

  fn from(setter: Setter) -> FnDecl {
    FnDecl {
      name: setter.name,
      args: ArgList::required(vec!(setter.argument_name)),
      body: setter.body,
    }
  }

}
