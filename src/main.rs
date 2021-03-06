
extern crate lalrpop_util;
extern crate walkdir;
extern crate gdlisp;

/*
pub mod sxp;
pub mod compile;
pub mod gdscript;
mod parser_test;

lalrpop_mod!(pub parser);
*/

use gdlisp::gdscript::decl;
use gdlisp::command_line::{parse_args, show_help_message};
use gdlisp::pipeline::Pipeline;
use gdlisp::pipeline::config::ProjectConfig;
use gdlisp::runner::version::get_godot_version;

use walkdir::WalkDir;

use std::io::{self, BufRead};
use std::env;
use std::path::{PathBuf, Path};
use std::str::FromStr;

fn run_pseudo_repl() {
  let stdin = io::stdin();
  let mut pipeline = Pipeline::new(ProjectConfig { root_directory: PathBuf::from_str(".").unwrap(), optimizations: true }); // Infallible

  for line in stdin.lock().lines() {
    match pipeline.compile_code("./REPL.lisp", &line.unwrap()) {
      Err(err) => {
        println!("Error: {}", err);
      }
      Ok(trans) => {
        let cls = decl::TopLevelClass::from(trans);
        print!("{}", cls.to_gd());
      }
    }
  }

}

fn compile_file<P : AsRef<Path> + ?Sized>(input: &P) {
  let input = input.as_ref();
  let mut pipeline = Pipeline::new(ProjectConfig { root_directory: input.parent().unwrap_or(input).to_owned(), optimizations: true });
  match pipeline.load_file(input.file_name().unwrap()) {
    Err(err) => {
      println!("Error: {}", err);
    }
    Ok(_unit) => {}
  }
}

fn compile_all_files<P : AsRef<Path> + ?Sized>(input: &P) {
  let input = input.as_ref();
  let mut pipeline = Pipeline::new(ProjectConfig { root_directory: input.to_owned(), optimizations: true });
  for entry in WalkDir::new(input).into_iter().filter_map(|e| e.ok()) {
    if entry.path().is_file() && entry.path().extension() == Some("lisp".as_ref()) {
      println!("Compiling {} ...", entry.path().to_string_lossy());
      let path = entry.path().strip_prefix(&pipeline.config().root_directory).expect("Non-local file load detected");
      match pipeline.load_file(path) {
        Err(err) => {
          println!("Error in {}: {}", entry.path().to_string_lossy(), err);
        }
        Ok(_unit) => {}
      }
    }
  }
}

fn main() {
  let args: Vec<_> = env::args().collect();
  let program_name = &args[0];
  let parsed_args = parse_args(&args[1..]);
  if parsed_args.help_message {
    show_help_message(&program_name);
  } else {

    let version = get_godot_version();
    match version {
      Ok(version) => {
        println!("GDLisp (development version)");
        println!("Running under Godot {}", version);
      }
      Err(err) => {
        eprintln!("Warning: `godot` is not on your path. A significant portion of the GDLisp compiler depends on the Godot engine. It is strongly recommended that you add `godot` to your path before continuing.");
        eprintln!("Warning: While looking for Godot, the error I got was: {}", err);
      }
    }

    if let Some(input) = parsed_args.input_file {
      let input: &Path = input.as_ref();
      if input.is_dir() {
        compile_all_files(input);
      } else {
        compile_file(input);
      }
    } else {
      run_pseudo_repl();
    }

  }
}
