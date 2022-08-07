
use super::incremental::IncCompiler;
use super::decl::Decl;
use super::declaration_table::DeclarationTable;
use crate::compile::error::{GDError, GDErrorF};
use crate::gdscript::library::gdnative::NativeClasses;
use crate::pipeline::Pipeline;
use crate::pipeline::source::SourceOffset;

pub fn compile_bootstrapping_decl(
  icompiler: &mut IncCompiler,
  pipeline: &mut Pipeline,
  acc: &mut impl Extend<Decl>,
  directive: &str,
  pos: SourceOffset,
) -> Result<(), GDError> {
  let native = pipeline.get_native_classes();
  match directive {
    "constants" => {
      bootstrap_constant_names(icompiler.declaration_table(), &native, acc, pos);
    }
    _ => {
      return Err(GDError::new(GDErrorF::BadBootstrappingDirective(directive.to_owned()), pos));
    }
  }
  Ok(())
}

fn bootstrap_constant_names(
  table: &mut DeclarationTable,
  native: &NativeClasses,
  acc: &mut impl Extend<Decl>,
  pos: SourceOffset,
) {
  todo!()
}
