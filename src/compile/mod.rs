
pub mod names;
pub mod body;
pub mod error;
pub mod stmt_wrapper;
pub mod symbol_table;
pub mod special_form;
pub mod stateful;
pub mod preload_resolver;
pub mod resource_type;
pub mod constant;
pub mod args;
pub mod factory;
pub mod frame;
pub mod import;

use frame::CompilerFrame;
use body::builder::{CodeBuilder, StmtBuilder, HasDecls};
use body::class_initializer::{ClassBuilder, InitTime};
use body::class_scope::{ClassScope, OutsideOfClass, DirectClassScope};
use names::fresh::FreshNameGenerator;
use names::generator::NameGenerator;
use preload_resolver::PreloadResolver;
use constant::MaybeConstant;
use crate::gdscript::literal::Literal;
use crate::gdscript::expr::{Expr, ExprF};
use crate::gdscript::decl::{self, Decl, DeclF, Onready, Setget};
use crate::gdscript::class_extends::ClassExtends;
use crate::gdscript::library;
use crate::gdscript::arglist::ArgList;
use error::{GDError, GDErrorF};
use symbol_table::{SymbolTable, ClassTablePair};
use symbol_table::local_var::{LocalVar, ValueHint, VarName, VarScope, VarNameIntoExtendsError};
use symbol_table::function_call;
use symbol_table::call_magic::CallMagic;
use symbol_table::call_magic::table::MagicTable;
use crate::ir;
use crate::ir::import::{ImportName, ImportDecl, ImportDetails};
use crate::ir::identifier::{Namespace, ClassNamespace};
use crate::ir::access_type::AccessType;
use crate::ir::decl::InstanceFunctionName;
use crate::runner::path::RPathBuf;
use crate::pipeline::error::PError;
use crate::pipeline::Pipeline;
use crate::pipeline::can_load::CanLoad;
use crate::pipeline::source::SourceOffset;
use stateful::{StExpr, NeedsResult, SideEffects};
use resource_type::ResourceType;
use import::ImportTable;

use std::ffi::OsStr;
use std::convert::TryFrom;
use std::borrow::Borrow;

type IRDecl = ir::decl::Decl;
type IRDeclF = ir::decl::DeclF;
type IRExpr = ir::expr::Expr;
type IRExprF = ir::expr::ExprF;
type IRArgList = ir::arglist::ordinary::ArgList;

pub struct Compiler {
  gen: FreshNameGenerator,
  resolver: Box<dyn PreloadResolver>,
  magic_table: MagicTable,
  import_table: ImportTable,
  minimalist: bool,
}

impl Compiler {

  /// Constructs a new compiler associated with the given name
  /// generator and preload resolver.
  pub fn new(gen: FreshNameGenerator, resolver: Box<dyn PreloadResolver>, minimalist: bool) -> Compiler {
    let magic_table = library::magic::standard_magic_table();
    let import_table = ImportTable::default();
    Compiler { gen, resolver, magic_table, import_table, minimalist }
  }

  pub fn nil_expr(pos: SourceOffset) -> StExpr {
    StExpr { expr: Expr::null(pos), side_effects: SideEffects::None }
  }

  pub fn name_generator(&mut self) -> &mut FreshNameGenerator {
    &mut self.gen
  }

  pub fn preload_resolver(&self) -> &dyn PreloadResolver {
    &*self.resolver
  }

  pub fn import_path_table(&self) -> &ImportTable {
    &self.import_table
  }

  pub fn is_minimalist(&self) -> bool {
    self.minimalist
  }

  pub fn frame<'a, 'b, 'c, 'd, 'e, B>(&'a mut self,
                                      pipeline: &'b mut Pipeline,
                                      builder: &'c mut B,
                                      table: &'d mut SymbolTable,
                                      class_scope: &'e mut dyn ClassScope)
                                      -> CompilerFrame<'a, 'b, 'c, 'd, 'e, B> {
    CompilerFrame::new(self, pipeline, builder, table, class_scope)
  }

  fn compile_export(&mut self,
                    pipeline: &mut Pipeline,
                    table: &mut SymbolTable,
                    expr: &IRExpr) -> Result<Expr, GDError> {
    // Any expression valid as a const is valid here, but then so are
    // Expr::LocalVar since we need to allow type names.
    //
    // TODO Validate that the local vars appearing here make sense.
    match &expr.value {
      IRExprF::LocalVar(s) => Ok(Expr::new(ExprF::Var(s.to_owned()), expr.pos)),
      _ => {
        let mut class_scope = OutsideOfClass;
        let expr = self.frame(pipeline, &mut (), table, &mut class_scope).compile_simple_expr("export", expr, NeedsResult::Yes)?;
        expr.validate_const_expr("export", table)?;
        Ok(expr)
      }
    }
  }

  // TODO To CompilerFrame (???)
  pub fn compile_class_inner_decl(&mut self,
                                  pipeline: &mut Pipeline,
                                  builder: &mut ClassBuilder,
                                  tables: ClassTablePair<'_, '_>,
                                  class_scope: &mut DirectClassScope,
                                  decl: &ir::decl::ClassInnerDecl)
                                  -> Result<Decl, GDError> {
    let table = tables.into_table(decl.is_static());
    match &decl.value {
      ir::decl::ClassInnerDeclF::ClassSignalDecl(s) => {
        let name = names::lisp_to_gd(&s.name);
        let args = s.args.args.iter().map(|x| names::lisp_to_gd(x)).collect();
        Ok(Decl::new(DeclF::SignalDecl(name, ArgList::required(args)), decl.pos))
      }
      ir::decl::ClassInnerDeclF::ClassConstDecl(c) => {
        // TODO Merge this with IRDecl::ConstDecl above
        let gd_name = names::lisp_to_gd(&c.name);
        let value = self.frame(pipeline, &mut (), table, class_scope).compile_simple_expr(&c.name, &c.value, NeedsResult::Yes)?;
        value.validate_const_expr(&c.name, table)?;
        Ok(Decl::new(DeclF::ConstDecl(gd_name, value), decl.pos))
      }
      ir::decl::ClassInnerDeclF::ClassVarDecl(v) => {
        let export = v.export.as_ref().map(|export| {
          export.args.iter().map(|expr| self.compile_export(pipeline, table, expr)).collect::<Result<Vec<_>, _>>()
        }).transpose()?;
        let export = export.map(|args| decl::Export { args });
        let name = names::lisp_to_gd(&v.name);
        // Note: immediate_value is *only* values which will be
        // compiled directly into the declaration. Initializers added
        // to _init or _ready are not included in this return value
        // (they are added to the mutable builder frame instead).
        let immediate_value = {
          let mut local_frame = self.frame(pipeline, builder.builder_for(v.init_time), table, class_scope);
          Compiler::compile_inner_var_value(&name, v.value.as_ref(), &mut local_frame, decl.pos)?
        };
        let onready = Onready::from(immediate_value.is_some() && v.init_time == InitTime::Ready);
        Ok(Decl::new(DeclF::VarDecl(decl::VarDecl {
          export,
          onready,
          name,
          value: immediate_value,
          setget: Setget::default(),
        }), decl.pos))
      }
      ir::decl::ClassInnerDeclF::ClassFnDecl(f) => {

        // If we're dealing with a setter or a getter, inform the
        // builder that we will need a synthetic field and validate
        // the argument list and modifiers.
        match &f.name {
          InstanceFunctionName::Ordinary(_) => {
            // No action required; there is no proxy field for
            // ordinary methods.
          }
          InstanceFunctionName::Setter(field_name) => {

            if f.is_static == decl::Static::IsStatic || f.args.len() != 1 {
              return Err(GDError::new(
                GDErrorF::BadSetterArguments(field_name.to_owned()),
                decl.pos,
              ));
            }

            let field_name = names::lisp_to_gd(field_name);

            let conflicting_name = builder.declare_setter_for(field_name.to_owned());
            if conflicting_name.is_some() {
              return Err(GDError::new(
                GDErrorF::DuplicateName(ClassNamespace::Value, field_name),
                decl.pos,
              ));
            }
          }
          InstanceFunctionName::Getter(field_name) => {

            if f.is_static == decl::Static::IsStatic || !f.args.is_empty() {
              return Err(GDError::new(
                GDErrorF::BadGetterArguments(field_name.to_owned()),
                decl.pos,
              ));
            }

            let field_name = names::lisp_to_gd(field_name);

            let conflicting_name = builder.declare_getter_for(field_name.to_owned());
            if conflicting_name.is_some() {
              return Err(GDError::new(
                GDErrorF::DuplicateName(ClassNamespace::Value, field_name),
                decl.pos,
              ))
            }
          }
        }

        let gd_name = names::lisp_to_gd(f.name.method_name().borrow());
        let func = factory::declare_function(&mut self.frame(pipeline, builder, table, class_scope),
                                             gd_name,
                                             IRArgList::from(f.args.clone()),
                                             &f.body,
                                             &stmt_wrapper::Return)?;
        Ok(Decl::new(DeclF::FnDecl(f.is_static, func), decl.pos))
      }
    }
  }

  fn compile_inner_var_value(name: &str,
                             value: Option<&IRExpr>,
                             frame: &mut CompilerFrame<StmtBuilder>,
                             pos: SourceOffset)
                             -> Result<Option<Expr>, GDError> {
    match value {
      None => Ok(None),
      Some(value) => {

        // Try to compile as a constant first
        if let Ok(simple_expr) = frame.compile_simple_expr(name, value, NeedsResult::Yes) {
          if simple_expr.is_allowable_const(frame.table) {
            return Ok(Some(simple_expr));
          }
        }

        // Otherwise, compile as an ordinary expression into the builder.
        let destination = stmt_wrapper::AssignToExpr(
          Expr::self_var(pos).attribute(name.to_owned(), pos),
        );
        frame.compile_stmt(&destination, value)?;
        Ok(None)

      }
    }
  }

  /// Given the (simple) name of an `extends` clause for a class or
  /// singleton object, resolve that name (using the symbol table)
  /// into a [`ClassExtends`].
  ///
  /// The name `extends` is looked up in the symbol table `table`.
  /// Then the resulting variable's GDScript [`LocalVar`] is converted
  /// into a [`ClassExtends`] via [`TryFrom::try_from`]. If either the
  /// lookup or the conversion fails, then the appropriate error is
  /// returned.
  ///
  /// Additionally, if the scope of the [`LocalVar`] is *not*
  /// [`VarScope::GlobalVar`](crate::compile::symbol_table::local_var::VarScope::GlobalVar),
  /// then [`GDErrorF::CannotExtend`] is returned. This situation should
  /// never happen for top-level class or object declarations, but it
  /// can occur in lambda classes.
  pub fn resolve_extends(table: &SymbolTable, extends: &str, pos: SourceOffset) -> Result<ClassExtends, GDError> {
    let var = table.get_var(extends).ok_or_else(|| GDError::new(GDErrorF::NoSuchVar(extends.to_owned()), pos))?;
    if var.scope != VarScope::GlobalVar {
      return Err(GDError::new(GDErrorF::CannotExtend(VarNameIntoExtendsError::CannotExtendLocal(extends.to_owned())), pos));
    }
    let var_name = var.name.clone();
    ClassExtends::try_from(var_name).map_err(|x| GDError::from_value(x, pos))
  }

  fn bind_decl(magic_table: &MagicTable,
               pipeline: &mut Pipeline,
               table: &mut SymbolTable,
               decl: &IRDecl)
               -> Result<(), GDError> {
    match &decl.value {
      IRDeclF::FnDecl(ir::decl::FnDecl { visibility: _, call_magic, name, args, body: _ }) => {
        let func = function_call::FnCall::file_constant(
          function_call::FnSpecs::from(args.to_owned()),
          function_call::FnScope::Global,
          names::lisp_to_gd(name),
        );
        let call_magic: CallMagic = match call_magic {
          None => CallMagic::DefaultCall,
          Some(m) => {
            // If a call magic declaration was specified, it MUST
            // exist or it's a compile error.
            match magic_table.get(m) {
              None => return Err(GDError::new(GDErrorF::NoSuchMagic(m.to_owned()), decl.pos)),
              Some(magic) => magic.clone(),
            }
          }
        };
        table.set_fn(name.clone(), func, call_magic);
      }
      IRDeclF::MacroDecl(ir::decl::MacroDecl { visibility: _, name, args, body: _ }) => {
        // As above, macros compile basically the same as functions in
        // terms of call semantics and should be resolved during the
        // IR stage.
        let func = function_call::FnCall::file_macro(
          function_call::FnSpecs::from(args.to_owned()),
          function_call::FnScope::Global,
          names::lisp_to_gd(name),
        );
        table.set_fn(name.clone(), func, CallMagic::DefaultCall);
      }
      IRDeclF::SymbolMacroDecl(ir::decl::SymbolMacroDecl { name, .. }) => {
        // No action; symbol macros have no runtime binding presence.
        table.set_var(name.clone(), LocalVar {
          name: VarName::Null,
          access_type: AccessType::Read,
          scope: VarScope::GlobalVar,
          assignable: false,
          value_hint: Some(ValueHint::SymbolMacro),
        });
      }
      IRDeclF::ConstDecl(ir::decl::ConstDecl { visibility: _, name, value }) => {
        let mut var = LocalVar::file_constant(names::lisp_to_gd(name)); // Can't assign to constants
        if let IRExprF::Literal(value) = &value.value {
          if let Ok(value) = Literal::try_from(value.clone()) {
            var = var.with_hint(ValueHint::Literal(value));
          }
        }
        table.set_var(name.clone(), var);
      }
      IRDeclF::ClassDecl(ir::decl::ClassDecl { name, main_class, .. }) => {
        if *main_class {
          let var = LocalVar::current_file(pipeline.current_filename().to_string()).with_hint(ValueHint::ClassName);
          table.set_var(name.clone(), var);
        } else {
          let var = LocalVar::file_constant(names::lisp_to_gd(name))
            .no_assign() // Can't assign to class names
            .with_hint(ValueHint::ClassName);
          table.set_var(name.clone(), var);
        }
      }
      IRDeclF::EnumDecl(edecl) => {
        let name = edecl.name.clone();
        let var = LocalVar::file_constant(names::lisp_to_gd(&name))
          .no_assign() // Can't assign to constants
          .with_hint(ValueHint::enumeration(edecl.value_names()));
        table.set_var(name, var);
      }
      IRDeclF::DeclareDecl(ddecl) => {
        let ir::decl::DeclareDecl { visibility: _, declare_type, name, target_name } = ddecl;
        let target_name: String = match target_name {
          // Default behavior: Target name is `lisp_to_gd` on the
          // declared name. Overriding built-ins is forbidden in this
          // case.
          None => names::lisp_to_gd(name),
          // Custom behavior: Target name is `lisp_to_gd_bare` on the
          // specified name. Overriding built-ins is allowed in this
          // case. This essentially acts as though the target name was
          // placed inside a `(literally ...)` block.
          Some(target_name) => names::lisp_to_gd_bare(target_name),
        };
        match declare_type {
          ir::decl::DeclareType::Value => {
            let var = LocalVar::file_constant(target_name);
            table.set_var(name.clone(), var);
          }
          ir::decl::DeclareType::Constant => {
            let var = LocalVar::file_constant(target_name)
              .with_hint(ValueHint::GlobalConstant);
            table.set_var(name.clone(), var);
          }
          ir::decl::DeclareType::Superglobal => {
            let var = LocalVar::superglobal(target_name)
              .with_hint(ValueHint::Superglobal);
            table.set_var(name.clone(), var);
          }
          ir::decl::DeclareType::Function(args) => {
            let func = function_call::FnCall::file_constant(
              function_call::FnSpecs::from(args.to_owned()),
              function_call::FnScope::Global,
              target_name,
            );
            table.set_fn(name.clone(), func, CallMagic::DefaultCall);
          }
          ir::decl::DeclareType::SuperglobalFn(args) => {
            let func = function_call::FnCall::superglobal(
              function_call::FnSpecs::from(args.to_owned()),
              function_call::FnScope::Superglobal,
              target_name,
            );
            table.set_fn(name.clone(), func, CallMagic::DefaultCall);
          }
        }
      }
    };
    Ok(())
  }

  fn make_preload_line(&self, var: String, path: &RPathBuf, pos: SourceOffset) -> Result<Decl, GDError> {
    let expr = self.make_preload_expr(path, pos)?;
    Ok(Decl::new(
      DeclF::ConstDecl(var, expr),
      pos,
    ))
  }

  fn get_preload_resolved_path(&self, path: &RPathBuf, pos: SourceOffset) -> Result<String, GDError> {
      let mut path = path.clone();
      if path.path().extension() == Some(OsStr::new("lisp")) {
        path.path_mut().set_extension("gd");
      }
      self.resolver.resolve_preload(&path)
        .ok_or_else(|| GDError::new(GDErrorF::NoSuchFile(path.to_string()), pos))
  }

  /// Compile a `preload` call, using the current preload resolver on
  /// `self` to resolve the path.
  pub fn make_preload_expr(&self, path: &RPathBuf, pos: SourceOffset) -> Result<Expr, GDError> {
    if self.resolver.include_resource(ResourceType::from(path.path())) {
      let path = self.get_preload_resolved_path(path, pos)?;
      Ok(Expr::simple_call("preload", vec!(Expr::from_value(path, pos)), pos))
    } else {
      // We null out any resources we don't understand. This means
      // that GDScript source files (those NOT written in GDLisp) and
      // other resources like PackedScene instances cannot be used in
      // macros, as they'll just be seen as "null" during macro
      // resolution. I do not verify that you follow this rule; you
      // are expected to be responsible with your macro resource
      // usage.
      Ok(Expr::null(pos))
    }
  }

  fn import_name(&mut self, import: &ImportDecl) -> String {
    let prefix = match &import.details {
      ImportDetails::Named(s) => names::lisp_to_gd(s),
      ImportDetails::Restricted(_) | ImportDetails::Open => String::from("_Import"),
    };
    self.gen.generate_with(&prefix)
  }

  pub fn translate_call(import_name: String, mut call: function_call::FnCall) -> function_call::FnCall {
    if call.scope != function_call::FnScope::Superglobal {
      call.object = call.object.into_imported(import_name);
    }
    call
  }

  pub fn resolve_import(&mut self,
                        pipeline: &mut Pipeline,
                        builder: &mut CodeBuilder,
                        table: &mut SymbolTable,
                        import: &ImportDecl)
                        -> Result<(), PError> {
    let preload_name = self.import_name(import);
    builder.add_decl(self.make_preload_line(preload_name.clone(), &import.filename, import.pos)?);
    let res_type = ResourceType::from(import);

    // If the path should be included and isn't being `null`ed out
    // right now, then we add it to the import table.
    if self.resolver.include_resource(ResourceType::from(import.filename.path())) {
      let target_path = self.get_preload_resolved_path(&import.filename, import.pos)?;
      self.import_table.set(preload_name.clone(), target_path);
    }

    ResourceType::check_import(pipeline, import)?;

    if res_type == ResourceType::GDLispSource {
      // Now add the pertinent symbols to the symbol table
      let unit = pipeline.load_file(&import.filename.path(), import.pos)?;
      let unit_table = &unit.table;
      let exports = &unit.exports;
      let names = import.names(&unit.exports);
      for imp in names {
        // TODO Can these situations happen? It seems like
        // ImportNameResolutionError should've already caught any
        // nonexistent names. Review this and see if we can turn these
        // into panic! calls.
        let ImportName { namespace: namespace, in_name: import_name, out_name: export_name } = imp;
        match namespace {
          Namespace::Function => {
            let (call, _) = unit_table.get_fn(&export_name).ok_or_else(|| GDError::new(GDErrorF::NoSuchFn(export_name), import.pos))?;
            let call = Compiler::translate_call(preload_name.clone(), call.clone());
            table.set_fn(import_name.clone(), call, CallMagic::DefaultCall);
          }
          Namespace::Value => {
            let mut var = unit_table.get_var(&export_name).ok_or_else(|| GDError::new(GDErrorF::NoSuchVar(export_name), import.pos))?.clone();
            var.name = var.name.into_imported(preload_name.clone());
            table.set_var(import_name.clone(), var);
          }
        }
      }

      // If it was a restricted import list, validate the import names
      if let ImportDetails::Restricted(vec) = &import.details {
        for imp in vec {
          imp.refine(exports).map_err(|x| GDError::from_value(x, import.pos))?;
        }
      }
    } else {
      // Simple resource import
      let name = match &import.details {
        ImportDetails::Named(s) => s.to_owned(),
        _ => return Err(PError::from(GDError::new(GDErrorF::InvalidImportOnResource(import.filename.to_string()), import.pos))),
      };
      // TODO Check that the file exists?
      let var = LocalVar::file_constant(preload_name);
      // TODO Value hint? It would have to be based on the file extension / what resource type it is.
      // (e.g. *.gd would be ClassHint, but *.tres or *.png would not be)
      table.set_var(name, var);
    }

    Ok(())
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::gdscript::decl::Decl;
  use crate::gdscript::stmt::{Stmt, StmtF};
  use crate::sxp::ast::{AST, ASTF};
  use crate::compile::symbol_table::function_call::{FnCall, FnScope, FnSpecs};
  use crate::compile::preload_resolver::DefaultPreloadResolver;
  use crate::compile::body::builder::StmtBuilder;
  use crate::pipeline::config::ProjectConfig;
  use crate::pipeline::source::SourceOffset;
  use crate::ir::incremental::IncCompiler;
  use crate::runner::version::VersionInfo;

  use std::path::PathBuf;

  // TODO A lot more of this

  fn int(n: i32) -> AST {
    AST::new(ASTF::Int(n), SourceOffset::default())
  }

  fn nil() -> AST {
    AST::nil(SourceOffset::default())
  }

  #[allow(dead_code)]
  fn cons(a: AST, b: AST) -> AST {
    AST::new(ASTF::cons(a, b), SourceOffset::default())
  }

  fn list(data: Vec<AST>) -> AST {
    AST::dotted_list(data, nil())
  }

  fn e(expr: ExprF) -> Expr {
    Expr::new(expr, SourceOffset::default())
  }

  fn s(stmt: StmtF) -> Stmt {
    Stmt::new(stmt, SourceOffset::default())
  }

  fn bind_helper_symbols(table: &mut SymbolTable) {
    // Binds a few helper names to the symbol table for the sake of
    // debugging.
    table.set_fn(String::from("foo1"), FnCall::file_constant(FnSpecs::new(1, 0, None), FnScope::Global, String::from("foo1")), CallMagic::DefaultCall);
    table.set_fn(String::from("foo"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("foo")), CallMagic::DefaultCall);
    table.set_fn(String::from("bar"), FnCall::file_constant(FnSpecs::new(0, 0, None), FnScope::Global, String::from("bar")), CallMagic::DefaultCall);
    table.set_var(String::from("foobar"), LocalVar::read(String::from("foobar")));
  }

  fn compile_stmt(ast: &AST) -> Result<(Vec<Stmt>, Vec<Decl>), PError> {

    let mut pipeline = Pipeline::new(ProjectConfig { root_directory: PathBuf::from("."), optimizations: false, godot_version: VersionInfo::default() });

    let used_names = ast.all_symbols();
    let mut compiler = Compiler::new(FreshNameGenerator::new(used_names), Box::new(DefaultPreloadResolver), false);
    let mut table = SymbolTable::new();
    bind_helper_symbols(&mut table);
    library::bind_builtins(&mut table, true);
    let mut builder = StmtBuilder::new();
    let expr = {
      let mut icompiler = IncCompiler::new(ast.all_symbols());
      icompiler.bind_builtin_macros(&mut pipeline);
      icompiler.compile_expr(&mut pipeline, ast)
    }?;
    {
      let mut class_scope = OutsideOfClass;
      let mut frame = compiler.frame(&mut pipeline, &mut builder, &mut table, &mut class_scope);
      let () = frame.compile_stmt(&mut stmt_wrapper::Return, &expr)?;
    }
    Ok(builder.build())
  }

  #[test]
  fn compile_var() {
    let ast = AST::symbol("foobar", SourceOffset::default());
    let expected = s(StmtF::ReturnStmt(e(ExprF::Var(String::from("foobar")))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_call() {
    let ast = list(vec!(AST::symbol("foo1", SourceOffset::default()), int(10)));
    let expected = s(StmtF::ReturnStmt(e(ExprF::Call(None, String::from("foo1"), vec!(e(ExprF::from(10)))))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_int() {
    let ast = int(99);
    let expected = s(StmtF::ReturnStmt(e(ExprF::from(99))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_bool_t() {
    let ast = AST::new(ASTF::Bool(true), SourceOffset::default());
    let expected = s(StmtF::ReturnStmt(e(ExprF::from(true))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_bool_f() {
    let ast = AST::new(ASTF::Bool(false), SourceOffset::default());
    let expected = s(StmtF::ReturnStmt(e(ExprF::from(false))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_string() {
    let ast = AST::string("foobar", SourceOffset::default());
    let expected = s(StmtF::ReturnStmt(e(ExprF::from("foobar".to_owned()))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, vec!(expected));
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_vacuous() {
    let ast = list(vec!(AST::symbol("progn", SourceOffset::default()), int(1), int(2)));
    let expected = vec!(s(StmtF::ReturnStmt(e(ExprF::from(2)))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_progn_stateful() {
    let ast = list(vec!(AST::symbol("progn", SourceOffset::default()),
                        list(vec!(AST::symbol("foo", SourceOffset::default()))),
                        list(vec!(AST::symbol("bar", SourceOffset::default())))));
    let expected = vec!(s(StmtF::Expr(e(ExprF::Call(None, String::from("foo"), vec!())))),
                        s(StmtF::ReturnStmt(e(ExprF::Call(None, String::from("bar"), vec!())))));
    let actual = compile_stmt(&ast).unwrap();
    assert_eq!(actual.0, expected);
    assert_eq!(actual.1, vec!());
  }

  #[test]
  fn compile_nil() {
    let result1 = compile_stmt(&nil()).unwrap();
    assert_eq!(result1, (vec!(s(StmtF::ReturnStmt(Compiler::nil_expr(SourceOffset::default()).expr))), vec!()));

    let result2 = compile_stmt(&list(vec!(AST::symbol("progn", SourceOffset::default())))).unwrap();
    assert_eq!(result2, (vec!(s(StmtF::ReturnStmt(Compiler::nil_expr(SourceOffset::default()).expr))), vec!()));
  }

}
