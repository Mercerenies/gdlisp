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

//! [`FnCall`] and various helper structures for storing information
//! about functions.

use crate::gdscript::expr::Expr;
use crate::gdscript::inner_class;
use crate::ir::arglist::vararg::VarArg;
use crate::compile::Compiler;
use crate::compile::error::GDError;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::stateful::StExpr;
use crate::compile::preload_resolver::PreloadResolver;
use crate::compile::args::Expecting;
use crate::compile::constant::CONSTANT_GDSCRIPT_FUNCTIONS;
use crate::pipeline::can_load::CanLoad;
use crate::pipeline::source::SourceOffset;
use super::call_magic::{CallMagic};
use super::local_var::VarName;
use super::SymbolTable;

use serde::{Serialize, Deserialize};

/// All of the relevant information needed to make a call to a
/// function is stored in `FnCall`.
/// [`CallMagic`](super::call_magic::CallMagic) requires an `FnCall` to
/// identify the information about a function.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FnCall {
  /// The function's scope.
  pub scope: FnScope,
  /// The expression for the object on which the function is being
  /// called.
  pub object: FnName,
  /// The name of the function, as a GDScript identifier.
  pub function: String,
  /// The shape of the function.
  pub specs: FnSpecs,
  /// Whether or not the function being called is in fact a macro.
  /// This can affect the way in which a function is imported in
  /// scope.
  pub is_macro: bool,
}

/// The type of scope in which a function declaration appears. This
/// affects certain compiled behaviors of the function, such as
/// whether it needs to be included in closures and how it gets
/// imported into new scopes.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FnScope {
  /// A superglobal function is available in all scopes, such as
  /// built-in GDScript functions like `abs()` or `min()`.
  /// Superglobals never need to be closed around or qualified, even
  /// if imported.
  Superglobal,
  /// A global function is defined at the global scope and will be
  /// compiled to a globally-scoped function. When imported, globals
  /// will have their name qualified. Global functions do not require
  /// closures, as they do not close around any data.
  Global,
  /// A semiglobal function is still local in scope from the GDLisp
  /// perspective, but it doesn't require any variable closures, so it
  /// will be promoted to a global function on the GDScript side, for
  /// efficiency reasons.
  SemiGlobal,
  /// A local function is a closure which exists as a local variable
  /// on the GDScript side, very similar to a lambda but with
  /// different name resolution rules. The string parameter is the
  /// name of the (GDScript) local variable which contains the
  /// function. The variable should refer to an object with
  /// `call_func` and `call_funcv` methods. If the variable has
  /// nonstandard method names, then [`FnScope::SpecialLocal`] should
  /// be used instead, as the object is not suitable as a funcref in
  /// this case.
  Local(String),
  /// A special local function is like a local function in that it
  /// needs a closure. But a special local function is potentially
  /// constructed with several other functions like it, so it will
  /// still need an explicit closure if referred to via a funcref.
  /// This is the worst case scenario, as we can make no assumptions
  /// about the scoping of this function. As with [`FnScope::Local`],
  /// the argument to `FnScope::SpecialLocal` should be the name of a
  /// local variable on which the function is defined. No assumptions
  /// are made about the name of the function on this object, only
  /// that it exists.
  SpecialLocal(String),
}

/// Like [`VarName`](super::local_var::VarName), this will eventually
/// translate into an [`Expr`] (or possibly a lack thereof) and
/// consists of all of the expressions which denote valid function
/// "name" translations.
///
/// `FnName` should be thought of as a more restricted form of `Expr`.
/// The ultimate goal of this type is to eventually be converted to an
/// `Expr` via [`From::from`].
#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub enum FnName {
  /// A static function local to the file and defined at the
  /// top-level.
  FileConstant,
  /// A superglobal name, such as built-in GDScript functions.
  Superglobal,
  /// A file-level function defined in another file and imported.
  ImportedConstant(Box<VarName>),
  /// A local function referenced using a local variable.
  OnLocalVar(Box<VarName>),
  /// A local function referenced by the current local scope.
  OnLocalScope,
}

/// A specification of the parameters a function takes. `FnSpecs` is
/// similar to [`ArgList`](crate::ir::arglist::ordinary::ArgList)
/// except that the latter specifies names for its arguments, whereas
/// this structure simply designates the shape of the function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct FnSpecs {
  /// The number of required parameters to the function. If the
  /// function is provided with fewer than this many arguments, an
  /// error is issued.
  pub required: usize,
  /// The number of optional parameters to the function. After
  /// required arguments are bound, the next arguments are bound to
  /// optional parameters until the argument list is exhausted or
  /// optional parameters have all been bound.
  pub optional: usize,
  /// The "rest" argument. After required and optional arguments have
  /// been handled, if there are excess arguments, they are bound to
  /// the "rest" parameter. If there is no "rest" parameter and there
  /// are excess arguments, then an error is issued.
  pub rest: Option<VarArg>,
}

/// When referencing enclosing static data from an inner class scope,
/// there are two possible types of inner class scopes we could be
/// within.
///
/// If we're in a static inner scope and need to reference a static
/// outer scope, then the inner scope will need to load the current
/// file, using [`FnName::inner_static_load`]. On the other hand, if
/// we're in a non-static inner scope and need to reference a static
/// outer scope, then we're expected to do a single `load` call and
/// store the result in an instance variable.
///
/// This enum encapsulates these two behaviors. An outer static
/// reference is either referred to by a live static load or by an
/// instance variable with a given name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OuterStaticRef<'a> {
  /// An outer static reference from an inner static scope uses a live
  /// `load` call on-site.
  InnerStatic,
  /// An outer static reference from an inner non-static scope
  /// references an instance variable on the current (inner) class
  /// with the given name.
  InnerInstanceVar(&'a str),
}

impl FnCall {

  /// A top-level macro. A macro always has `object` of
  /// [`FnName::FileConstant`] and `is_macro` of true.
  pub fn file_macro(specs: FnSpecs, scope: FnScope, function: String) -> FnCall {
    FnCall { specs, scope, object: FnName::FileConstant, function, is_macro: true }
  }

  /// A top-level function, with [`FnName::FileConstant`].
  pub fn file_constant(specs: FnSpecs, scope: FnScope, function: String) -> FnCall {
    FnCall { specs, scope, object: FnName::FileConstant, function, is_macro: false }
  }

  /// A superglobal function, with [`FnName::Superglobal`].
  pub fn superglobal(specs: FnSpecs, scope: FnScope, function: String) -> FnCall {
    FnCall { specs, scope, object: FnName::Superglobal, function, is_macro: false }
  }

  /// Returns whether the function indicated by this function call
  /// object can be called in a `const` context, given sufficiently
  /// constant arguments.
  pub fn can_be_called_as_const(&self) -> bool {
    match self.object {
      FnName::FileConstant => false,
      FnName::Superglobal => {
        // Very specific superglobals can be called.
        CONSTANT_GDSCRIPT_FUNCTIONS.contains(&*self.function)
      }
      FnName::ImportedConstant(_) => false,
      FnName::OnLocalVar(_) => false,
      FnName::OnLocalScope => false,
    }
  }

  /// As [`FnCall::into_expr_with_magic`] with [`CallMagic::DefaultCall`] as the
  /// call magic type.
  pub fn into_expr(self,
                   compiler: &mut Compiler,
                   builder: &mut StmtBuilder,
                   table: &mut SymbolTable,
                   args: Vec<StExpr>,
                   pos: SourceOffset)
                   -> Result<Expr, GDError> {
    self.into_expr_with_magic(&CallMagic::DefaultCall, compiler, builder, table, args, pos)
  }

  /// Compile, via [`CallMagic::compile`], the function call `self`
  /// into an [`Expr`].
  pub fn into_expr_with_magic(self,
                              magic: &CallMagic,
                              compiler: &mut Compiler,
                              builder: &mut StmtBuilder,
                              table: &mut SymbolTable,
                              args: Vec<StExpr>,
                              pos: SourceOffset)
                              -> Result<Expr, GDError> {
    magic.compile(self, compiler, builder, table, args, pos)
  }

}

impl FnSpecs {

  /// An `FnSpecs` object representing a function of no arguments.
  pub const EMPTY: FnSpecs = FnSpecs { required: 0, optional: 0, rest: None };

  /// Convenience constructor for a `FnSpecs`.
  pub fn new(required: usize, optional: usize, rest: Option<VarArg>) -> FnSpecs {
    FnSpecs { required, optional, rest }
  }

  /// Returns whether `self` has a "rest" argument of any kind (list
  /// or array). Equivalent to `self.rest.is_some()`.
  pub fn has_rest(&self) -> bool {
    self.rest.is_some()
  }

  /// In GDLisp, a function can have a "rich" argument list (as per
  /// [`ArgList`](crate::ir::arglist::ordinary::ArgList)), consisting
  /// of required arguments, optional arguments, and a rest argument.
  /// However, on the GDScript side, we always compile down to a set
  /// number of required arguments. Given an `FnSpecs`, this method
  /// returns the number of arguments that will be present in that
  /// resulting compiled function.
  ///
  /// Required arguments in GDLisp compile one-to-one to required
  /// arguments in GDScript. Optional arguments in GDLisp are
  /// converted into required arguments and padded (at compile-time)
  /// with `null` as needed. Finally, if a rest argument (of either
  /// type) is present, it is converted into a single additional
  /// argument.
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::compile::symbol_table::function_call::FnSpecs;
  /// # use gdlisp::ir::arglist::vararg::VarArg;
  /// assert_eq!(FnSpecs::new(0, 0, None).runtime_arity(), 0);
  /// assert_eq!(FnSpecs::new(5, 0, None).runtime_arity(), 5);
  /// assert_eq!(FnSpecs::new(0, 5, None).runtime_arity(), 5);
  /// assert_eq!(FnSpecs::new(2, 5, None).runtime_arity(), 7);
  /// assert_eq!(FnSpecs::new(0, 0, Some(VarArg::RestArg)).runtime_arity(), 1);
  /// assert_eq!(FnSpecs::new(0, 0, Some(VarArg::ArrArg)).runtime_arity(), 1);
  /// assert_eq!(FnSpecs::new(2, 1, Some(VarArg::RestArg)).runtime_arity(), 4);
  /// assert_eq!(FnSpecs::new(1, 2, Some(VarArg::ArrArg)).runtime_arity(), 4);
  /// ```
  pub fn runtime_arity(&self) -> usize {
    self.required + self.optional + usize::from(self.has_rest())
  }

  /// The minimum number of arguments necessary to correctly call a
  /// function with this shape. Equivalent to `self.required`.
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::compile::symbol_table::function_call::FnSpecs;
  /// # use gdlisp::ir::arglist::vararg::VarArg;
  /// assert_eq!(FnSpecs::new(0, 0, None).min_arity(), 0);
  /// assert_eq!(FnSpecs::new(5, 0, None).min_arity(), 5);
  /// assert_eq!(FnSpecs::new(0, 5, None).min_arity(), 0);
  /// assert_eq!(FnSpecs::new(2, 5, None).min_arity(), 2);
  /// assert_eq!(FnSpecs::new(0, 0, Some(VarArg::RestArg)).min_arity(), 0);
  /// assert_eq!(FnSpecs::new(0, 0, Some(VarArg::ArrArg)).min_arity(), 0);
  /// assert_eq!(FnSpecs::new(2, 1, Some(VarArg::RestArg)).min_arity(), 2);
  /// assert_eq!(FnSpecs::new(1, 2, Some(VarArg::ArrArg)).min_arity(), 1);
  /// ```
  pub fn min_arity(&self) -> usize {
    self.required
  }

  /// The maximum number of arguments that can be correctly supplied
  /// to a function with this shape.
  ///
  /// If the function takes a rest argument, then this returns
  /// [`usize::MAX`].
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::compile::symbol_table::function_call::FnSpecs;
  /// # use gdlisp::ir::arglist::vararg::VarArg;
  /// assert_eq!(FnSpecs::new(0, 0, None).max_arity(), 0);
  /// assert_eq!(FnSpecs::new(5, 0, None).max_arity(), 5);
  /// assert_eq!(FnSpecs::new(0, 5, None).max_arity(), 5);
  /// assert_eq!(FnSpecs::new(2, 5, None).max_arity(), 7);
  /// assert_eq!(FnSpecs::new(0, 0, Some(VarArg::RestArg)).max_arity(), usize::MAX);
  /// assert_eq!(FnSpecs::new(0, 0, Some(VarArg::ArrArg)).max_arity(), usize::MAX);
  /// assert_eq!(FnSpecs::new(2, 1, Some(VarArg::RestArg)).max_arity(), usize::MAX);
  /// assert_eq!(FnSpecs::new(1, 2, Some(VarArg::ArrArg)).max_arity(), usize::MAX);
  /// ```
  pub fn max_arity(&self) -> usize {
    // TODO Is usize.MAX correct here? If we put an upper limit on
    // function arity, use that instead.
    if self.has_rest() { usize::MAX } else { self.required + self.optional }
  }

}

impl FnScope {

  /// Whether or not the scope is local.
  ///
  /// [`FnScope::Local`] and [`FnScope::SpecialLocal`] are local
  /// scopes. All other scopes are considered global.
  pub fn is_local(&self) -> bool {
    self.local_name().is_some()
  }

  /// If `self` refers to a local scope (i.e.
  /// [`is_local`](FnScope::is_local) returns true), then this method
  /// returns the name of the GDScript local variable which contains
  /// the enclosing object for the function. If `self` refers to a
  /// global scope, this method returns [`None`].
  pub fn local_name(&self) -> Option<&str> {
    match self {
      FnScope::Local(name) | FnScope::SpecialLocal(name) => Some(name),
      FnScope::Superglobal | FnScope::Global | FnScope::SemiGlobal => None,
    }
  }

}

impl FnName {

  /// A value imported from another scope.
  pub fn imported_constant(orig_name: VarName) -> FnName {
    FnName::ImportedConstant(Box::new(orig_name))
  }

  /// A call made on a local variable with the given name.
  pub fn on_local_var(local_name: VarName) -> FnName {
    FnName::OnLocalVar(Box::new(local_name))
  }

  /// Converts the `FnName` into an appropriate value to be called
  /// from another module.
  ///
  /// If a name `foo` is available at top-level scope `A.gd` and some
  /// file `B.gd` imports `A.gd` and calls the top-level preload
  /// constant `AConst`, then calling `foo.as_imported("AConst")` will
  /// convert the name to how it should be referenced from `B.gd`.
  pub fn into_imported(self, import_name: String) -> FnName {
    self.into_imported_var(VarName::FileConstant(import_name))
  }

  pub fn into_imported_var(self, import: VarName) -> FnName {
    match self {
      FnName::FileConstant => {
        FnName::imported_constant(import)
      }
      FnName::Superglobal => {
        FnName::Superglobal
      }
      FnName::ImportedConstant(v) => {
        FnName::imported_constant(v.into_imported_var(import))
      }
      FnName::OnLocalVar(v) => {
        // This case probably shouldn't happen, but oh well. Delegate to VarName.
        FnName::on_local_var(v.into_imported_var(import))
      }
      FnName::OnLocalScope => {
        // This case definitely shouldn't happen. Leave it alone I guess.
        FnName::OnLocalScope
      }
    }
  }

  /// Generate a `load` expression for the current file (as per
  /// `loader`) using `resolver` as the current preload resolver. See
  /// [`FnName::update_for_inner_scope`] for details on why this is
  /// necessary. Note that this function is usually called *through*
  /// that one and should seldom be called directly.
  ///
  /// # Panics
  ///
  /// If the current filename cannot be detected from `loader`, or if
  /// `resolver` fails to resolve the name given by `loader`, then
  /// this function will panic.
  pub fn inner_static_load(resolver: &(impl PreloadResolver + ?Sized), loader: &impl CanLoad) -> FnName {
    let fname = inner_class::get_current_filename(loader, resolver)
      .expect("Cannot identify currently-loading filename");
    FnName::on_local_var(VarName::CurrentFile(fname))
  }

  /// In Godot, inner classes do not retain a reference to the
  /// enclosing scope, not even to be able to call static functions in
  /// an enclosing scope. GDLisp makes extensive use of static
  /// file-level functions, so this poses an issue for any nontrivial
  /// inner class. `inner_static_load` provides a viable workaround
  /// for this.
  ///
  /// If we are in a static context in an inner class and we need to
  /// refer to an enclosing static function, we need to `load` the
  /// current file again. In most cases, this `load` will be a cache
  /// hit and will simply see an existing resource (though it's not
  /// impossible for a cache miss to occur, and in this case the
  /// `load` will still work as intended, albeit a bit slower).
  /// `inner_static_load` constructs an `FnName` representing a `load`
  /// expression for the appropriate file. `resolver` shall be the
  /// current preload resolver, and `loader`, naturally, provides the
  /// current filename.
  ///
  /// If we're in a non-static inner scope and need to refer to an
  /// enclosing static scope, then for efficiency reasons we don't
  /// want to load the outer script every time. Instead, we simply
  /// require that an instance variable (whose name is given by
  /// `outer_ref_name`) on the class be loaded (via `load`) to point
  /// to the current file at construction time.
  ///
  /// See [Issue #30](https://github.com/Mercerenies/gdlisp/issues/30)
  /// for a further discussion.
  pub fn update_for_inner_scope(&mut self,
                                binding_type: &OuterStaticRef<'_>,
                                resolver: &(impl PreloadResolver + ?Sized),
                                loader: &impl CanLoad) {
    if self.needs_inner_scope_reference() {
      match binding_type {
        OuterStaticRef::InnerStatic => {
          *self = FnName::inner_static_load(resolver, loader);
        }
        OuterStaticRef::InnerInstanceVar(outer_ref_name) => {
          *self = FnName::OnLocalVar(Box::new(VarName::outer_class_ref(outer_ref_name)));
        }
      }
    }
  }

  /**
   * Returns true if this function name type requires an outer class
   * reference in order to be called. Functions require an outer class
   * reference if they are either (1) a file-level constant, or (2)
   * already an outer class reference. The latter case occurs if we're
   * in a nested lambda inside of a lambda scope that already has an
   * outer class reference.
   */
  pub fn needs_inner_scope_reference(&self) -> bool {
    match self {
      FnName::FileConstant => {
        true
      }
      FnName::OnLocalVar(var_name) => {
        matches!(&**var_name, VarName::OuterClassRef(_))
      }
      _ => {
        false
      }
    }
  }

  /// As [`update_for_inner_scope`](FnName::update_for_inner_scope),
  /// but takes ownership of `self` and returns the modified `FnName`.
  pub fn into_inner_scope(mut self,
                          binding_type: &OuterStaticRef<'_>,
                          resolver: &(impl PreloadResolver + ?Sized),
                          loader: &impl CanLoad)
                          -> Self {
    self.update_for_inner_scope(binding_type, resolver, loader);
    self
  }

  /// **Note:** An `Option` here does NOT denote failure to convert.
  /// `FnName` can be converted to an `Option<Expr>`, in the sense
  /// that "there is no expression here" is a completely valid result
  /// of conversion and indicates a function call which is not
  /// subscripted on a name.
  pub fn into_expr(self, pos: SourceOffset) -> Option<Expr> {
    match self {
      FnName::FileConstant => None,
      FnName::Superglobal => None,
      FnName::ImportedConstant(var_name) => Some(var_name.into_expr(pos)),
      FnName::OnLocalVar(var_name) => Some(var_name.into_expr(pos)),
      FnName::OnLocalScope => None,
    }
  }

}

impl<'a> OuterStaticRef<'a> {

  /// If `static_binding` is true, then this method returns
  /// [`OuterStaticRef::InnerStatic`]. Otherwise, this method returns
  /// [`OuterStaticRef::InnerInstanceVar`] with `outer_ref_name` as
  /// the reference name. This is useful in situations where we
  /// already have an outer reference name and simply need to know
  /// whether we can use it from the given scope.
  pub fn from_ref_type(static_binding: bool, outer_ref_name: &'a str) -> Self {
    if static_binding {
      OuterStaticRef::InnerStatic
    } else {
      OuterStaticRef::InnerInstanceVar(outer_ref_name)
    }
  }

  /// Converts `None` to [`OuterStaticRef::InnerStatic`] and `Some` to
  /// [`OuterStaticRef::InnerInstanceVar`].
  pub fn from_option_ref(outer_ref_name: Option<&'a str>) -> Self {
    match outer_ref_name {
      None => OuterStaticRef::InnerStatic,
      Some(ref_name) => OuterStaticRef::InnerInstanceVar(ref_name),
    }
  }

}

impl From<VarName> for FnName {
  fn from(var_name: VarName) -> FnName {
    FnName::imported_constant(var_name)
  }
}

impl From<FnSpecs> for Expecting {
  fn from(specs: FnSpecs) -> Expecting {
    Expecting::new(specs.min_arity(), specs.max_arity())
  }
}
