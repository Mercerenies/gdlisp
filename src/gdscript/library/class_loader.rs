
use super::gdnative::NativeClasses;
use super::gdnative::class::Class;
use crate::ir::decl::{Decl, DeclF, DeclareDecl, DeclareType, ClassInnerDecl, ClassInnerDeclF, ClassVarDecl};
use crate::ir::export::Visibility;
use crate::ir::expr::{Expr, ExprF};
use crate::compile::body::class_initializer::InitTime;
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

pub fn get_singleton_class_var_declarations<'a>(native: &'a NativeClasses, pos: SourceOffset) -> impl Iterator<Item=ClassInnerDecl> + 'a {
  get_all_singleton_classes(native).into_iter().map(move |class| {
    let name = backing_class_name_of(class);
    let expr = Expr::new(
      ExprF::MethodCall(
        Box::new(Expr::new(ExprF::LocalVar(String::from("NamedSyntheticType")), pos)),
        String::from("new"),
        vec!(Expr::from_value(class.name.clone(), pos)), // Note: *Original* class name, not the one we made in backing_class_name_of
      ),
      pos,
    );
    ClassInnerDecl::new(
      ClassInnerDeclF::ClassVarDecl(ClassVarDecl {
        export: None,
        name: name,
        value: Some(expr),
        init_time: InitTime::Init,
      }),
      pos,
    )
  })
}

fn type_declaration_for_class(class: &Class, declare_type: DeclareType) -> DeclareDecl {
  let name = backing_class_name_of(class);
  DeclareDecl {
    visibility: Visibility::Public,
    declare_type: declare_type,
    name: name.clone(),
    target_name: Some(name.clone()),
  }
}

fn value_declaration_for_singleton(class: &Class, declare_type: DeclareType) -> DeclareDecl {
  let singleton_name = class.singleton_name.to_owned();
  DeclareDecl {
    visibility: Visibility::Public,
    declare_type: declare_type,
    name: singleton_name.clone(),
    target_name: Some(singleton_name),
  }
}

fn backing_class_name_of(class: &Class) -> String {
  // Some singletons in Godot have a class name and a distinct
  // singleton name. For instance, `_Engine` is the class name for
  // `Engine`. For these, it's fine to just use what Godot has. But
  // some, like `ARVRServer`, have the *same* name for the type and
  // object. In that case, we keep the name for the object and force
  // an underscore at the beginning of the class name.
  if class.singleton_name == class.name {
    format!("_{}", class.name)
  } else {
    class.name.to_owned()
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
