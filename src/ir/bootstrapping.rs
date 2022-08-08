
use super::incremental::IncCompiler;
use super::decl::{Decl, DeclF, DeclareDecl, DeclareType, EnumDecl};
use super::export::Visibility;
use crate::compile::error::{GDError, GDErrorF};
use crate::gdscript::library::gdnative::NativeClasses;
use crate::gdscript::library::constant_loader;
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
    _ => {
      return Err(GDError::new(GDErrorF::BadBootstrappingDirective(directive.to_owned()), pos));
    }
  }
  Ok(())
}

fn bootstrap_constant_names(
  native: &NativeClasses,
  acc: &mut impl Extend<Decl>,
  pos: SourceOffset,
) {
  let all_constant_names = constant_loader::get_all_constants(native);
  for constant_name in all_constant_names {
    acc.extend(One(declare_superglobal(constant_name, pos)));
  }
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
