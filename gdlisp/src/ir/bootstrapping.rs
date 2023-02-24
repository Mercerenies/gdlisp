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

use super::incremental::IncCompiler;
use super::decl::{Decl, DeclF, DeclareDecl, DeclareType, EnumDecl, ClassDecl};
use super::expr::Expr;
use super::export::Visibility;
use crate::compile::error::{GDError, GDErrorF};
use crate::gdscript::library::gdnative::NativeClasses;
use crate::gdscript::library::{constant_loader, class_loader};
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;
use crate::util::one::One;

pub fn compile_bootstrapping_decl(
  _icompiler: &mut IncCompiler,
  pipeline: &mut Pipeline,
  acc: &mut impl Extend<Decl>,
  directive: &str,
  pos: SourceOffset,
) -> Result<(), GDError> {
  let native = pipeline.get_native_classes();
  match directive {
    "constants" => {
      bootstrap_constant_names(native, acc, pos);
    }
    "constant-enums" => {
      bootstrap_constant_enums(native, acc, pos);
    }
    "non-singleton-types" => {
      bootstrap_non_singletons(native, acc, pos);
    }
    "singleton-types" => {
      bootstrap_singletons(native, acc, pos);
    }
    _ => {
      return Err(GDError::new(GDErrorF::BadBootstrappingDirective(directive.to_owned()), pos));
    }
  }
  Ok(())
}

pub fn compile_bootstrapping_class_inner_decl(
  _icompiler: &mut IncCompiler,
  pipeline: &mut Pipeline,
  acc: &mut ClassDecl,
  directive: &str,
  pos: SourceOffset,
) -> Result<(), GDError> {
  let native = pipeline.get_native_classes();
  match directive {
    "singleton-backing-types" => {
      bootstrap_singleton_backing_types(native, acc, pos);
    }
    _ => {
      return Err(GDError::new(GDErrorF::BadBootstrappingDirective(directive.to_owned()), pos));
    }
  }
  Ok(())
}

pub fn compile_bootstrapping_expr(
  _icompiler: &mut IncCompiler,
  pipeline: &mut Pipeline,
  directive: &str,
  pos: SourceOffset,
) -> Result<Expr, GDError> {
  let native = pipeline.get_native_classes();
  match directive {
    "native-types-table" => {
      Ok(class_loader::native_types_dictionary_initializer(native, pos))
    }
    _ => {
      Err(GDError::new(GDErrorF::BadBootstrappingDirective(directive.to_owned()), pos))
    }
  }
}

fn bootstrap_constant_names(
  native: &NativeClasses,
  acc: &mut impl Extend<Decl>,
  pos: SourceOffset,
) {
  let all_constant_names = constant_loader::get_all_constants(native);
  acc.extend(all_constant_names.map(|name| declare_superglobal(name, pos)));
}

fn declare_superglobal(name: &str, pos: SourceOffset) -> Decl {
  Decl::new(
    DeclF::DeclareDecl(
      DeclareDecl {
        visibility: Visibility::Public,
        declare_type: DeclareType::Superglobal,
        name: name.to_owned(),
        target_name: Some(name.to_owned()),
      }
    ),
    pos,
  )
}

fn bootstrap_constant_enums(
  native: &NativeClasses,
  acc: &mut impl Extend<Decl>,
  pos: SourceOffset,
) {
  let all_enums = constant_loader::get_all_constant_enums(native, pos);
  for constant_enum in all_enums {
    let decl = EnumDecl::from(constant_enum);
    acc.extend(One(Decl::new(DeclF::EnumDecl(decl), pos)));
  }
}

fn bootstrap_non_singletons(
  native: &NativeClasses,
  acc: &mut impl Extend<Decl>,
  pos: SourceOffset,
) {
  let all_non_singleton_classes = class_loader::get_non_singleton_declarations(native);
  acc.extend(all_non_singleton_classes.map(|decl| Decl::new(DeclF::DeclareDecl(decl), pos)));
}

fn bootstrap_singletons(
  native: &NativeClasses,
  acc: &mut impl Extend<Decl>,
  pos: SourceOffset,
) {
  let all_singleton_classes = class_loader::get_singleton_declarations(native);
  acc.extend(all_singleton_classes.into_iter().map(|decl| Decl::new(DeclF::DeclareDecl(decl), pos)));
}

fn bootstrap_singleton_backing_types(
  native: &NativeClasses,
  acc: &mut ClassDecl,
  pos: SourceOffset,
) {
  let all_singleton_classes = class_loader::get_singleton_class_var_declarations(native, pos);
  acc.decls.extend(all_singleton_classes);
}
