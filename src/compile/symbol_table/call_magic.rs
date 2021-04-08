
// Function calls, by default, compile to function calls in GDScript,
// naturally. However, there are many situations where the function on
// the GDScript side is simply a trivial wrapper around some operation
// that should be inlined. This is such a trivial inline step that's
// so ubiquitously useful that we do it here as a rule. If a built-in
// function is called directly (i.e. not through a funcref) then it
// can trigger a special CallMagic which effectively inlines it.

// For a good example, look at the + GDLisp function. In general, it
// compiles to GDLisp.plus, which iterates over its arguments, adds
// them, and returns the result. But, of course, (+ a b) shouldn't
// require a for loop, so any time we call + with an arity known at
// compile time (i.e. without invoking funcrefs or anything like
// that), we can compile directly to the + operator in GDScript, which
// is much more efficient.

// Note that this is *not* general-purpose inlining, which I'll
// implement later as a general pass over the IR. This is for the very
// specific case of certain GDScript functions written in GDLisp.gd
// which I know how to inline effectively by hand.

use dyn_clone::{self, DynClone};

use crate::gdscript::expr::Expr;
use crate::gdscript::op;
use crate::gdscript::library;
use crate::gdscript::expr_wrapper;
use crate::compile::Compiler;
use crate::compile::error::Error;
use crate::compile::body::builder::StmtBuilder;
use crate::compile::stateful::StExpr;
use crate::compile::stmt_wrapper::{self, StmtWrapper};
use crate::ir::arglist::VarArg;
use crate::util;
use super::function_call::FnCall;
use super::SymbolTable;

pub trait CallMagic : DynClone {
  fn compile<'a>(&self,
                 call: FnCall,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error>;
}

dyn_clone::clone_trait_object!(CallMagic);

#[derive(Clone)]
pub struct DefaultCall;
#[derive(Clone)]
pub struct MinusOperation;
#[derive(Clone)]
pub struct DivOperation;
#[derive(Clone)]
pub struct IntDivOperation;
#[derive(Clone)]
pub struct ModOperation;
#[derive(Clone)]
pub struct NEqOperation { pub fallback: Box<dyn CallMagic> }
#[derive(Clone)]
pub struct BooleanNotOperation;
#[derive(Clone)]
pub struct ListOperation;
#[derive(Clone)]
pub struct VectorOperation;
#[derive(Clone)]
pub struct ArraySubscript;
#[derive(Clone)]
pub struct ElementOf;
#[derive(Clone)]
pub struct InstanceOf;

// Covers addition and multiplication, for instance
#[derive(Clone)]
pub struct CompileToBinOp {
  pub zero: Expr,
  pub bin: op::BinaryOp,
  pub assoc: Assoc,
}

// Covers most comparison operators like = and < (excludes /=, as that
// operator's behavior is different in complex ways not captured by
// this CallMagic)
#[derive(Clone)]
pub struct CompileToTransCmp {
  pub bin: op::BinaryOp,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Assoc { Left, Right }

// For most of these (but *not* all of them), we need Vec<Expr>, not
// Vec<StExpr>. The latter gives more information than we usually
// need. So this helper just strips off the excess.
fn strip_st(x: Vec<StExpr>) -> Vec<Expr> {
  x.into_iter().map(|x| x.0).collect()
}

// This function is useful independent of the CallMagic interface (for
// instance, in the incremental compiler when calling macros), and
// DefaultCall::compile doesn't actually use all of the arguments
// supplied in the CallMagic contract, so this is the DefaultCall case
// of CallMagic, refined down only to the arguments it actually uses.
pub fn compile_default_call(call: FnCall, mut args: Vec<Expr>) -> Result<Expr, Error> {
  let FnCall { scope: _, object, function, specs } = call;
  // First, check arity
  if args.len() < specs.min_arity() as usize {
    return Err(Error::TooFewArgs(function, args.len()));
  }
  if args.len() > specs.max_arity() as usize {
    return Err(Error::TooManyArgs(function, args.len()));
  }
  let rest = if args.len() < (specs.required + specs.optional) as usize {
    vec!()
  } else {
    args.split_off((specs.required + specs.optional) as usize)
  };
  // Extend with nil
  while args.len() < (specs.required + specs.optional) as usize {
    args.push(library::nil());
  }
  match specs.rest {
    None => {
      assert!(rest.is_empty());
    }
    Some(VarArg::RestArg) => {
      args.push(library::construct_list(rest));
    }
    Some(VarArg::ArrArg) => {
      args.push(Expr::ArrayLit(rest));
    }
  }
  let object: Option<Expr> = object.into();
  Ok(Expr::Call(object.map(Box::new), function, args))
}

impl CallMagic for DefaultCall {
  // TODO Currently, this uses the GD name in error messages, which is
  // super wonky, especially for stdlib calls. Store the Lisp name and
  // use it for this.
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let args = strip_st(args);
    compile_default_call(call, args)
  }
}

impl CallMagic for CompileToBinOp {
  fn compile<'a>(&self,
                 _call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let args = strip_st(args);
    if args.is_empty() {
      Ok(self.zero.clone())
    } else {
      Ok(match self.assoc {
        Assoc::Left => {
          util::fold1(args.into_iter(), |x, y| Expr::Binary(Box::new(x), self.bin, Box::new(y)))
        }
        Assoc::Right => {
          util::fold1(args.into_iter().rev(), |x, y| Expr::Binary(Box::new(y), self.bin, Box::new(x)))
        }
      }.unwrap())
    }
  }
}

impl CallMagic for CompileToTransCmp {
  fn compile<'a>(&self,
                 call: FnCall,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 mut args: Vec<StExpr>) -> Result<Expr, Error> {
    match args.len() {
      0 => {
        Err(Error::TooFewArgs(call.function, args.len()))
      }
      1 => {
        // Dump to the builder as a simple statement if it's stateful.
        (&stmt_wrapper::Vacuous).wrap_to_builder(builder, args[0].clone());
        Ok(Expr::from(true))
      }
      2 => {
        let a = args.remove(0).0;
        let b = args.remove(0).0;
        Ok(Expr::Binary(Box::new(a), self.bin, Box::new(b)))
      }
      _ => {
        // We need to use several of the arguments twice, so any
        // arguments (such as function calls) which are potentially
        // stateful need to be stored in temporaries. Note that simply
        // accessing variables, even variables which may change, is
        // fine, since we're doing it twice in a row, and nothing
        // happens in between.
        let args = args.into_iter().map(|x| {
          let StExpr(expr, stateful) = x;
          if stateful.modifies_state() {
            let var_name = compiler.declare_var(builder, "_cmp", Some(expr));
            Expr::Var(var_name)
          } else {
            expr
          }
        });
        let comparisons = util::each_pair(args).map(|(x, y)| {
          Expr::Binary(Box::new(x), self.bin, Box::new(y))
        });
        Ok(
          util::fold1(comparisons, |x, y| Expr::Binary(Box::new(x), op::BinaryOp::And, Box::new(y))).unwrap()
        )
      }
    }
  }
}

impl CallMagic for MinusOperation {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let args = strip_st(args);
    match args.len() {
      0 => Err(Error::TooFewArgs(call.function, args.len())),
      1 => Ok(Expr::Unary(op::UnaryOp::Negate, Box::new(args[0].clone()))),
      _ => {
        Ok(
          util::fold1(args.into_iter(), |x, y| Expr::Binary(Box::new(x), op::BinaryOp::Sub, Box::new(y))).unwrap()
        )
      }
    }
  }
}

impl CallMagic for DivOperation {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let mut args = strip_st(args);
    match args.len() {
      0 => Err(Error::TooFewArgs(call.function, args.len())),
      1 => Ok(Expr::Binary(Box::new(Expr::from(1)),
                           op::BinaryOp::Div,
                           Box::new(expr_wrapper::float(args[0].clone())))),
      _ => {
        let first = Box::new(args.remove(0));
        let result = args.into_iter().fold(first, |x, y| {
          Box::new(Expr::Binary(x, op::BinaryOp::Div, Box::new(expr_wrapper::float(y))))
        });
        Ok(*result)
      }
    }
  }
}

// TODO Integer division (both the regular function and the call
// magic) still does floating-point division if the first argument is
// a vector. I want to do integer division to each component of the
// vector, which GDScript has no built-in way to do.
impl CallMagic for IntDivOperation {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let mut args = strip_st(args);
    match args.len() {
      0 => Err(Error::TooFewArgs(call.function, args.len())),
      1 => Ok(Expr::Binary(Box::new(Expr::from(1)),
                           op::BinaryOp::Div,
                           Box::new(expr_wrapper::int(args[0].clone())))),
      _ => {
        let first = Box::new(args.remove(0));
        let result = args.into_iter().fold(first, |x, y| {
          Box::new(Expr::Binary(x, op::BinaryOp::Div, Box::new(expr_wrapper::int(y))))
        });
        Ok(*result)
      }
    }
  }
}

impl CallMagic for ModOperation {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let mut args = strip_st(args);
    if args.len() < 2 {
      return Err(Error::TooFewArgs(call.function, args.len()));
    }
    if args.len() > 2 {
      return Err(Error::TooManyArgs(call.function, args.len()));
    }
    let y = args.pop().expect("Internal error in VectorOperation");
    let x = args.pop().expect("Internal error in VectorOperation");
    Ok(Expr::Binary(Box::new(x), op::BinaryOp::Mod, Box::new(y)))
  }
}

impl CallMagic for NEqOperation {
  fn compile<'a>(&self,
                 call: FnCall,
                 compiler: &mut Compiler<'a>,
                 builder: &mut StmtBuilder,
                 table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    // We only optimize for the 0, 1, and 2 argument cases. Any more
    // arguments than that and the resulting expression would just be
    // long and annoying, and it's simply easier to call the built-in
    // anyway.
    match args.len() {
      0 => {
        Err(Error::TooFewArgs(call.function, args.len()))
      }
      1 => {
        // Dump to the builder as a simple statement if it's stateful.
        (&stmt_wrapper::Vacuous).wrap_to_builder(builder, args[0].clone());
        Ok(Expr::from(true))
      }
      2 => {
        Ok(Expr::Binary(Box::new(args[0].0.clone()), op::BinaryOp::NE, Box::new(args[1].0.clone())))
      }
      _ => {
        self.fallback.compile(call, compiler, builder, table, args)
      }
    }
  }
}

impl CallMagic for BooleanNotOperation {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let args = strip_st(args);
    match args.len() {
      0 => Err(Error::TooFewArgs(call.function, args.len())),
      1 => Ok(Expr::Unary(op::UnaryOp::Not,
                          Box::new(args[0].clone()))),
      _ => Err(Error::TooManyArgs(call.function, args.len())),
    }
  }
}

impl CallMagic for ListOperation {
  fn compile<'a>(&self,
                 _call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let args = strip_st(args);
    Ok(library::construct_list(args))
  }
}

impl CallMagic for VectorOperation {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let mut args = strip_st(args);
    match args.len() {
      0 | 1 => {
        Err(Error::TooFewArgs(call.function, args.len()))
      }
      2 => {
        let y = args.pop().expect("Internal error in VectorOperation");
        let x = args.pop().expect("Internal error in VectorOperation");
        Ok(Expr::Call(None, String::from("Vector2"), vec!(x, y)))
      }
      3 => {
        let z = args.pop().expect("Internal error in VectorOperation");
        let y = args.pop().expect("Internal error in VectorOperation");
        let x = args.pop().expect("Internal error in VectorOperation");
        Ok(Expr::Call(None, String::from("Vector3"), vec!(x, y, z)))
      }
      _ => {
        Err(Error::TooManyArgs(call.function, args.len()))
      }
    }
  }
}

impl CallMagic for ArraySubscript {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let mut args = strip_st(args);
    match args.len() {
      0 | 1 => {
        Err(Error::TooFewArgs(call.function, args.len()))
      }
      2 => {
        let n = args.pop().expect("Internal error in ArraySubscript");
        let arr = args.pop().expect("Internal error in ArraySubscript");
        Ok(Expr::Subscript(Box::new(arr), Box::new(n)))
      }
      _ => {
        Err(Error::TooManyArgs(call.function, args.len()))
      }
    }
  }
}

impl CallMagic for ElementOf {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let mut args = strip_st(args);
    match args.len() {
      0 | 1 => {
        Err(Error::TooFewArgs(call.function, args.len()))
      }
      2 => {
        let arr = args.pop().expect("Internal error in ElementOf");
        let value = args.pop().expect("Internal error in ElementOf");
        Ok(Expr::Binary(Box::new(value), op::BinaryOp::In, Box::new(arr)))
      }
      _ => {
        Err(Error::TooManyArgs(call.function, args.len()))
      }
    }
  }
}

impl CallMagic for InstanceOf {
  fn compile<'a>(&self,
                 call: FnCall,
                 _compiler: &mut Compiler<'a>,
                 _builder: &mut StmtBuilder,
                 _table: &mut SymbolTable,
                 args: Vec<StExpr>) -> Result<Expr, Error> {
    let mut args = strip_st(args);
    match args.len() {
      0 | 1 => {
        Err(Error::TooFewArgs(call.function, args.len()))
      }
      2 => {
        let type_ = args.pop().expect("Internal error in InstanceOf");
        let value = args.pop().expect("Internal error in InstanceOf");
        Ok(Expr::Binary(Box::new(value), op::BinaryOp::Is, Box::new(type_)))
      }
      _ => {
        Err(Error::TooManyArgs(call.function, args.len()))
      }
    }
  }
}
