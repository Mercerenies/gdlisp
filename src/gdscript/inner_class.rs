
use super::decl::{self, Decl};
use crate::pipeline::can_load::CanLoad;
use crate::compile::symbol_table::SymbolTable;
use crate::compile::symbol_table::local_var::VarName;
use crate::compile::symbol_table::function_call::FnName;
use crate::compile::preload_resolver::PreloadResolver;
use crate::ir;
use crate::ir::identifier::{Id, Namespace};

// Helpers to make sure GDLisp inner classes (and any feature of
// GDLisp that compiles to inner classes, like lambdas and lambda
// classes) can access statics from the enclosing scope.

pub const OUTER_REFERENCE_NAME: &str = "__gdlisp_outer_class";

pub fn add_outer_class_ref_named(inner_class: &mut decl::ClassDecl, resolver: &dyn PreloadResolver, current_file: &impl CanLoad, var_name: String) {
  let current_filename = current_file.current_filename()
    .and_then(|fname| resolver.resolve_preload(&fname))
    .expect("Error identifying current file"); // TODO Expect
  let load_expr = VarName::load_expr(current_filename);
  let var_decl = Decl::VarDecl(None, var_name, Some(load_expr));
  inner_class.body.push(var_decl);
}

pub trait NeedsOuterClassRef {
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
    check_dependencies_for_outer_class_ref(self.dependencies().into_iter(), table)
  }
}

impl NeedsOuterClassRef for ir::decl::ConstructorDecl {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    check_dependencies_for_outer_class_ref(self.dependencies().into_iter(), table)
  }
}

impl NeedsOuterClassRef for ir::decl::ClassDecl {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    let ir::decl::ClassDecl { visibility: _, name: _, extends: _, main_class: _, constructor, decls } = self;
    constructor.needs_outer_class_ref(table) || decls.iter().any(|x| x.needs_outer_class_ref(table))
  }
}

impl NeedsOuterClassRef for ir::expr::LambdaClass {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    let ir::expr::LambdaClass { extends: _, args: _, constructor, decls } = self;
    constructor.needs_outer_class_ref(table) || decls.iter().any(|x| x.needs_outer_class_ref(table))
  }
}

impl NeedsOuterClassRef for ir::functions::Functions {
  fn needs_outer_class_ref(&self, table: &SymbolTable) -> bool {
    let deps = self.names().map(|x| Id::new(Namespace::Function, x.to_owned()));
    check_dependencies_for_outer_class_ref(deps, table)
  }
}
