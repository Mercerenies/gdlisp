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

use crate::gdscript::decl::{self, DeclF};
use crate::compile::error::GDError;
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

  fn run_on_file(&self, file: &mut decl::TopLevelClass) -> Result<(), GDError> {
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
  use crate::gdscript::class_extends::ClassExtends;
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
      extends: ClassExtends::SimpleIdentifier(String::from("Reference")),
      body: decls,
    };
    DeadDeclElimination.run_on_file(&mut toplevel).unwrap();
    assert_eq!(toplevel.name, None);
    assert_eq!(toplevel.extends, ClassExtends::SimpleIdentifier(String::from("Reference")));
    assert_eq!(toplevel.body, vec!());
  }

}
