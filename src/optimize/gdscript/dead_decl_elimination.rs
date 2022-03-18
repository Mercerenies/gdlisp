
use crate::gdscript::decl::{self, DeclF};
use crate::compile::error::Error;
use super::{FileOptimization, on_each_decl};

/// `DeadDeclElimination` simply removes all "pass" declarations.
/// These can be produced as a compilation artifact of other
/// optimization passes and of certain types of declarations in the
/// language. They're unnecessary in the resulting output and can
/// always be safely removed.
pub struct DeadDeclElimination;

// TODO In the future, this will expand to remove private declarations
// that are unused (such as lambda classes which have been optimized
// out by other passes). For now, it just runs on "pass" decls.
impl FileOptimization for DeadDeclElimination {

  fn run_on_file(&self, file: &mut decl::TopLevelClass) -> Result<(), Error> {
    file.body = on_each_decl(&file.body, |d| {
      if let DeclF::PassDecl = &d.value {
        Ok(vec!())
      } else {
        Ok(vec!(d.clone()))
      }
    })?;
    Ok(())
  }

}


#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::decl::{self, Decl, DeclF};
  use crate::pipeline::source::SourceOffset;

  fn d(decl: DeclF) -> Decl {
    Decl::new(decl, SourceOffset::default())
  }

  #[test]
  fn eliminate_pass() {
    /* (Eliminate the pass decl)
     * pass
     */
    let decls = vec!(
      d(DeclF::PassDecl),
    );
    let mut toplevel = decl::TopLevelClass {
      name: None,
      extends: decl::ClassExtends::Qualified(vec!(String::from("Reference"))),
      body: decls,
    };
    DeadDeclElimination.run_on_file(&mut toplevel).unwrap();
    assert_eq!(toplevel.name, None);
    assert_eq!(toplevel.extends, decl::ClassExtends::Qualified(vec!(String::from("Reference"))));
    assert_eq!(toplevel.body, vec!());
  }

}
