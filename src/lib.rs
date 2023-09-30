use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::process::exit;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::chunk::{Instr, ModuleChunk};
use crate::chunk::OpCode::OpReturn;
use crate::compiler::{CompilationResult, Compiler};
use crate::vm::{ExecutionMode, VMState, VM, Global};
use crate::InterpretResult::InterpretOK;

pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod gc;
pub mod io;
pub mod log;
pub mod native;
pub mod precedence;
pub mod resolver;
pub mod run;
pub mod scanner;
pub mod utils;
pub mod value;
pub mod vm;

#[derive(Debug, PartialEq)]
pub enum InterpretResult {
    InterpretOK(VMState, Vec<ModuleChunk>),
    InterpretCompileError,
    InterpretRuntimeError,
}


pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn fly(file: String, source: String, debug: bool, quiet: bool) -> InterpretResult {
    let mut compiler = Compiler::new_file(file, source, quiet, 0, DEBUG);
    let result = compiler.compile(debug);
    if result.is_none() {
        return InterpretResult::InterpretCompileError;
    }

    let result = result.unwrap();
    let mut vm = if debug {
        VM::new(ExecutionMode::Trace, result, quiet)
    } else {
        VM::new(ExecutionMode::Default, result, quiet)
    };
    vm.run()
}

pub fn repl() -> Result<(), ReadlineError> {
    let mut rl = DefaultEditor::new()?;
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let s = &String::new();
    let file_name = "script.phx".to_string();
    let mut vm = VM::new(if DEBUG { ExecutionMode::Trace } else { ExecutionMode::Default }, CompilationResult::default(), false);
    let mut state: Option<VMState> = None;
    let mut modules = Vec::new();
    let mut compiler = Compiler::new_file(file_name, s.clone(), false, 0, DEBUG);
    let mut last_state;
    let mut last_compiler;
    loop {
        // let readline = rl.readline(" \x1b[32m>>\x1b[0m");
        let readline = rl.readline(">>");
        match readline {
            Ok(mut line) => {
                last_state = state.clone();
                last_compiler = compiler.clone();
                rl.add_history_entry(line.as_str())
                    .expect("failed to add history");
                // let _result = fly(file_name.clone(), line, DEBUG, false);
                // dbg!(&compiler.current_module().scanner.code);
                line.push('\n');
                compiler.current_module().scanner.code.push_str(&line);
                // dbg!(&compiler.current_module().scanner.code);
                let result = compiler.compile(DEBUG);
                if result.is_none() {
                    compiler = last_compiler;
                    continue;
                }

                let mut result = result.unwrap();
                // pop the return instructions
                result.modules[0].functions[0].chunk.code.retain(|x| x.op_code != OpReturn);
                let line_num = result.modules[0].functions[0].chunk.code.last().unwrap().line_num;
                result.modules[0].functions[0].chunk.code.push(Instr { op_code: OpReturn, line_num });
                state = if let Some(mut s) = state {
                    while s.globals[0].len() < result.modules[0].identifiers.len() {
                        s.globals[0].push(Global::Uninit);
                    }
                    Some(s)
                } else { state };
                vm.modules_cache = result.modules;
                let s = vm.run_state(state.clone(), modules.clone());
                if let InterpretOK(mut s, m) = s {
                    s.current_frame.ip -= 1;
                    state = Some(s);
                    modules = m;
                    if DEBUG {
                        // println!("state: {:#?}, modules: {:#?}", state, modules);
                    }
                } else {
                    state = last_state;
                    compiler = last_compiler;
                }
            }
            Err(ReadlineError::Interrupted) => {
                // println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                // println!("CTRL-D");
                break;
            }
            Err(err) => {
                phoenix_error!("Error: {:?}", err);
            }
        }
    }
    rl.save_history("history.txt")?;
    println!("Saved history. Goodbye!");
    Ok(())
}

pub fn run_file(filename: String, debug: bool) -> InterpretResult {
    let path = Path::new(&filename);
    let path_display = path.display();

    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(why) => {
            eprintln!("Failed to open {}: {}", path_display, why);
            exit(1);
        }
    };

    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Ok(_) => fly(filename, s, debug, false),
        Err(why) => {
            eprintln!("Failed to read {}: {}", path_display, why);
            exit(1);
        }
    }
}

pub const DEBUG: bool = true;
