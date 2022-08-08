
use super::gdnative::NativeClasses;
use super::gdnative::class::Class;
use crate::ir::decl::{Decl, DeclF, DeclareDecl, DeclareType};
use crate::ir::export::Visibility;
use crate::ir::expr::{Expr, ExprF};
use crate::pipeline::source::SourceOffset;
use crate::util::prefix_matcher::PrefixMatcher;

use phf::{phf_map, phf_set};

use std::borrow::Borrow;
use std::collections::HashMap;

/// Classes which, for one reason or another, we do not want the
/// bootstrapping engine to touch. This includes some "fake" classes
/// like `GlobalConstants` which do not actually exist at runtime, as
/// well as very primitive things like `Object` that we handle
/// manually in `GDLisp.lisp`.
const CLASS_NAME_BLACKLIST: phf::Set<&'static str> = phf_set! {
  "GlobalConstants", "Object",
};

pub fn get_all_non_singleton_classes(native: &NativeClasses) -> Vec<&Class> {
  let mut classes: Vec<_> =
    native.values()
    .filter(|x| !x.singleton && !CLASS_NAME_BLACKLIST.contains(&*x.name))
    .collect();
  classes.sort_unstable_by(|a, b| a.name.cmp(&b.name));
  classes
}

pub fn get_all_singleton_classes(native: &NativeClasses) -> Vec<&Class> {
  let mut classes: Vec<_> =
    native.values()
    .filter(|x| x.singleton && !CLASS_NAME_BLACKLIST.contains(&*x.name))
    .collect();
  classes.sort_unstable_by(|a, b| a.name.cmp(&b.name));
  classes
}

pub fn get_non_singleton_declarations<'a>(native: &'a NativeClasses) -> impl Iterator<Item=DeclareDecl> + 'a {
  get_all_non_singleton_classes(native)
    .into_iter()
    .map(|cls| type_declaration_for_class(cls, DeclareType::Superglobal))
}

pub fn get_singleton_declarations<'a>(native: &'a NativeClasses) -> Vec<DeclareDecl> {
  let classes = get_all_singleton_classes(native);
  let mut result: Vec<DeclareDecl> = Vec::with_capacity(classes.len() * 2);
  for class in classes {
    result.push(type_declaration_for_class(class, DeclareType::Value));
    result.push(value_declaration_for_singleton(class, DeclareType::Superglobal));
  }
  result
}

fn type_declaration_for_class(class: &Class, declare_type: DeclareType) -> DeclareDecl {
  DeclareDecl {
    visibility: Visibility::Public,
    declare_type: declare_type,
    name: class.name.clone(),
    target_name: Some(class.name.clone()),
  }
}

fn value_declaration_for_singleton(class: &Class, declare_type: DeclareType) -> DeclareDecl {
  let singleton_name = singleton_name_of(class);
  assert!(!singleton_name.is_empty());
  DeclareDecl {
    visibility: Visibility::Public,
    declare_type: declare_type,
    name: singleton_name.clone(),
    target_name: Some(singleton_name),
  }
}

fn singleton_name_of(class: &Class) -> String {
  // Some singletons in Godot have a class name and a distinct
  // singleton name. For instance, `_Engine` is the class name for
  // `Engine`. For these, it's fine to just use what Godot has. But
  // some, like `ARVRServer`, have the *same* name for the type and
  // object. In that case, we keep the name for the object and force
  // an underscore at the beginning of the class name.
  if class.singleton_name == class.name {
    format!("_{}", class.singleton_name)
  } else {
    class.singleton_name.to_owned()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use std::collections::HashSet;

  #[test]
  fn test_non_singleton_classes() {
    let native = NativeClasses::get_api_from_godot().unwrap();
    let classes: HashSet<&str> = get_all_non_singleton_classes(&native)
      .into_iter()
      .map(|x| &*x.name)
      .collect();
    assert!(classes.contains("Node"));
    assert!(classes.contains("Node2D"));
    assert!(classes.contains("Spatial"));
    assert!(classes.contains("Texture"));
    assert!(!classes.contains("Object"));
    assert!(!classes.contains("GlobalConstants"));
  }

}
