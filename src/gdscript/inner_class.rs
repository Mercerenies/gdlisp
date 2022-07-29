
//! Helpers for construction of GDLisp inner classes.
//!
//! Helpers to make sure GDLisp inner classes (and any feature of
//! GDLisp that compiles to inner classes, like lambdas and lambda
//! classes) can access statics from the enclosing scope.
//!
//! There are two key pieces of functionality in this module. The
//! first is [`NeedsOuterClassRef`], a trait whose sole method
//! determines whether a given type of declaration needs to retain a
//! reference to the enclosing class. By default, inner classes in
//! GDScript cannot access the outer class, so in the cases where we
//! require this behavior, we have to go to special effort to get it.
//! In the cases where we *do* need this behavior, the second core
//! functionality of this module, [`add_outer_class_ref_named`] adds a
//! reference to the outer class to the given scope.

use super::decl::{self, Decl, DeclF, VarDecl};
use crate::pipeline::can_load::CanLoad;
use crate::pipeline::source::SourceOffset;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::VarName;
use crate::compile::symbol_table::function_call::FnName;
use crate::compile::preload_resolver::PreloadResolver;
use crate::ir;
use crate::ir::identifier::{Id, Namespace};
use crate::util::lattice::Lattice;

/// By convention, this name is used as the basis for outer class
/// reference names. Note that this name should not be used *directly*
/// unless you first check that there are no conflicts. Normally, this
/// would be passed to
/// [`generate_with`](crate::compile::names::generator::NameGenerator::generate_with).
pub const OUTER_REFERENCE_NAME: &str = "__gdlisp_outer_class";

/// Add a declaration to the outer class with the given name to
/// `inner_class`.
///
/// When the class referenced by `inner_class` is constructed, the
/// variable with name `var_name` will be initialized on the instance
/// to be equal to the enclosing class resource. This resource will be
/// loaded using `load`. The path to load is determined by `resolver`.
pub fn add_outer_class_ref_named(inner_class: &mut decl::ClassDecl, resolver: &dyn PreloadResolver, current_file: &impl CanLoad, var_name: String, pos: SourceOffset) {
  let current_filename = get_current_filename(current_file, resolver)
    .expect("Error identifying current file");
  let load_expr = VarName::load_expr(current_filename, pos);
  let var_decl = Decl::new(DeclF::VarDecl(VarDecl::new(var_name, Some(load_expr))), pos);
  inner_class.body.push(var_decl);
}

/// Gets the filename of the currently loading object from the
/// pipeline, as per [`CanLoad::current_filename`], and then resolves
/// that filename using `resolver`. Returns `None` if either step
/// fails.
pub fn get_current_filename<L, R>(current_file: &L, resolver: &R) -> Option<String>
where L : CanLoad + ?Sized,
      R : PreloadResolver + ?Sized {
  resolver.resolve_preload(&current_file.current_filename())
}

/// Trait for objects, such as declarations, which may need an outer
/// class reference.
///
/// Before blindly throwing unnecessary references on every inner
/// class (which, in addition to being hilariously inefficient, would
/// cause significant memory leaks since GDScript resources are
/// reference counted), the compiler should use this trait to check
/// whether an outer reference is actually warranted.
pub trait NeedsOuterClassRef {
  /// Given the names that are in scope at the current point in the
  /// code, determine whether this declaration requires an outer class
  /// reference.
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool;
}

fn check_dependencies_for_outer_class_ref(deps: impl Iterator<Item=Id>, table: &SymbolTable) -> bool {
  for dep in deps {
    // If the dependency is in the function namespace and refers to
    // a function defined in the top-level of the current file, then
    // we need an outer class reference.
    if dep.namespace == Namespace::Function {
      if let Some((func, _)) = table.get_fn(&dep.name) {
        // TODO Abstract this check alongside the inner static update pattern into one place
        if func.object == FnName::FileConstant {
          return true;
        }
      }
    }
  }
  false
}

impl NeedsOuterClassRef for ir::decl::ClassInnerDecl {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    if self.is_static() {
      // Static functions never use the outer class reference, since
      // the reference itself is an instance variable and is
      // inaccessible from static scope.
      return false;
    }
    check_dependencies_for_outer_class_ref(self.dependencies().into_iter().map(|(k, _)| k), table)
  }
}

impl NeedsOuterClassRef for ir::decl::ConstructorDecl {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    check_dependencies_for_outer_class_ref(self.dependencies().into_iter().map(|(k, _)| k), table)
  }
}

impl NeedsOuterClassRef for ir::decl::ClassDecl {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    let ir::decl::ClassDecl { visibility: _, name: _, extends: _, main_class: _, constructor, decls } = self;
    constructor.as_ref().map_or(false, |x| x.needs_outer_class_ref(table)) ||
      decls.iter().any(|x| x.needs_outer_class_ref(table))
  }
}

impl NeedsOuterClassRef for ir::expr::LambdaClass {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    let ir::expr::LambdaClass { extends: _, args: _, constructor, decls } = self;
    constructor.as_ref().map_or(false, |x| x.needs_outer_class_ref(table)) ||
      decls.iter().any(|x| x.needs_outer_class_ref(table))
  }
}

impl<T : Lattice> NeedsOuterClassRef for ir::closure_names::ClosureNames<T> {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    let deps = self.names().map(|x| Id::new(Namespace::Function, x.to_owned()));
    check_dependencies_for_outer_class_ref(deps, table)
  }
}
