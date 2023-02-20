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

use gdlisp::gdscript::{decl, library};
use gdlisp::command_line::{parse_args, show_help_message};
use gdlisp::repl::Repl;
use gdlisp::pipeline::Pipeline;
use gdlisp::pipeline::config::ProjectConfig;
use gdlisp::pipeline::source::{SourcedValue, SourceOffset};
use gdlisp::runner::version::{VersionInfo, get_godot_version_as_string};

use walkdir::WalkDir;

use std::io::{self, BufRead, Write};
use std::env;
use std::path::{PathBuf, Path};
use std::str::FromStr;
use std::thread::sleep;
use std::time::Duration;

// Legacy REPL
fn run_pseudo_repl(godot_version: VersionInfo) {
  let stdin = io::stdin();
  let config = ProjectConfig {
    root_directory: PathBuf::from_str(".").unwrap(), // Infallible
    optimizations: true,
    godot_version,
  };
  let mut pipeline = Pipeline::new(config);

  for line in stdin.lock().lines() {
    let line = line.unwrap();
    match pipeline.compile_code("./REPL.lisp", &line) {
      Err(err) => {
        let err = SourcedValue::new(&err, &line);
        println!("Error: {}", err);
      }
      Ok(trans) => {
        let cls = decl::TopLevelClass::from(trans);
        print!("{}", cls.to_gd());
      }
    }
  }

}

fn run_repl(godot_version: VersionInfo) {

  fn prompt() -> Option<String> {
    let mut line = String::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    print!("> ");
    stdout.flush().expect("Failed to flush stdout");
    let bytes = stdin.read_line(&mut line).expect("Could not read from stdin; broken pipe?");
    if bytes == 0 {
      None
    } else {
      Some(line)
    }
  }

  let config = ProjectConfig {
    root_directory: PathBuf::from_str(".").unwrap(), // Infallible
    optimizations: true,
    godot_version,
  };
  let mut repl = Repl::new(config);
  repl.force_load();

  while let Some(line) = prompt() {
    let result = repl.parse_and_run_code(&line);
    match result {
      Err(err) => {
        let err = SourcedValue::new(&err, &line);
        println!("Error: {}", err);
      }
      Ok(result) => {
        println!("{}", result);
      }
    }

    // Check and see if the Godot process has terminated. If the user
    // calls `get_tree().quit()`, then the process will terminate at
    // the end of the current frame. Godot's default FPS is 60, so
    // this should take 1/60th of a second, or 16 milliseconds. Out of
    // an abundance of caution, we pause for 100 milliseconds here.
    sleep(Duration::from_millis(100));
    if !repl.is_running() {
      break;
    }
  }

}

fn compile_file<P : AsRef<Path> + ?Sized>(input: &P, godot_version: VersionInfo) {
  let input = input.as_ref();
  let mut pipeline = Pipeline::new(ProjectConfig { root_directory: input.parent().unwrap_or(input).to_owned(), optimizations: true, godot_version });
  match pipeline.load_file(input.file_name().unwrap(), SourceOffset(0)) {
    Err(err) => {
      let err = SourcedValue::from_file(&err, input).unwrap();
      println!("Error: {}", err);
    }
    Ok(_unit) => {}
  }
}

fn compile_all_files<P : AsRef<Path> + ?Sized>(input: &P, godot_version: VersionInfo) {
  let input = input.as_ref();
  let mut pipeline = Pipeline::new(ProjectConfig { root_directory: input.to_owned(), optimizations: true, godot_version });
  for entry in WalkDir::new(input).into_iter().filter_map(|e| e.ok()) {
    if entry.path().is_file() && entry.path().extension() == Some("lisp".as_ref()) {
      println!("Compiling {} ...", entry.path().to_string_lossy());
      let path = entry.path().strip_prefix(&pipeline.config().root_directory).expect("Non-local file load detected");
      match pipeline.load_file(path, SourceOffset(0)) {
        Err(err) => {
          let err = SourcedValue::from_file(&err, entry.path()).unwrap();
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
    show_help_message(program_name);
  } else {

    let version = get_godot_version_as_string();
    match &version {
      Ok(version) => {
        println!("GDLisp v1.0.0");
        println!("Running under Godot {}", version);
      }
      Err(err) => {
        eprintln!("Warning: `godot` is not on your path. A significant portion of the GDLisp compiler depends on the Godot engine. It is strongly recommended that you add `godot` to your path before continuing.");
        eprintln!("Warning: While looking for Godot, the error I got was: {}", err);
      }
    }
    // If Godot isn't on the path, then any version checks will return
    // 0.0.0 since we can't get that information accurately.
    let version = version.unwrap_or_default();

    if parsed_args.compile_stdlib_flag {
      library::load_stdlib_to_file();
      println!("Stdlib compiled successfully.")
    } else if let Some(input) = parsed_args.input_file {
      let input: &Path = input.as_ref();
      if input.is_dir() {
        compile_all_files(input, VersionInfo::parse(&version));
      } else {
        compile_file(input, VersionInfo::parse(&version));
      }
    } else if parsed_args.legacy_repl_flag {
      run_pseudo_repl(VersionInfo::parse(&version));
    } else {
      run_repl(VersionInfo::parse(&version));
    }

  }
}
