use rustyline::config::Configurer;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::process::exit;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::chunk::OpCode::OpReturn;
use crate::chunk::{Instr, ModuleChunk};
use crate::compiler::{CompilationResult, Compiler};
use crate::vm::{ExecutionMode, Global, VMState, VM};
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

pub fn fly(file: String, source: String, debug: bool, quiet: bool, wait: bool) -> InterpretResult {
    let mut compiler = Compiler::new_file(file, source, quiet, 0, DEBUG, wait);
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
    let mut vm = VM::new(
        if DEBUG {
            ExecutionMode::Trace
        } else {
            ExecutionMode::Default
        },
        CompilationResult::default(),
        false,
    );
    let mut state: Option<VMState> = None;
    let mut modules = Vec::new();
    let mut compiler = Compiler::new_file(file_name, s.clone(), false, 0, DEBUG, false);
    let mut last_state;
    let mut last_compiler;
    let mut current_prompt = ">>";
    let default_prompt = ">>";
    let mut current_line = "".to_string();
    // tracks how many blocks deep we are (and thus how many spaces to indent)
    let mut block_offset = 0;
    loop {
        // let readline = rl.readline(" \x1b[32m>>\x1b[0m");
        let readline = rl.readline_with_initial(current_prompt, (&*"  ".repeat(block_offset), ""));
        match readline {
            Ok(mut line) => {
                rl.set_max_history_size(10000)
                    .expect("failed to set max history size");
                rl.add_history_entry(line.as_str())
                    .expect("failed to add history");
                line.push('\n');
                current_line.push_str(&line);
                // check if there are any open blocks/parens (check for: {, (, [)
                let mut open_blocks = 0;
                let mut open_parens = 0;
                let mut open_brackets = 0;
                for c in current_line.chars() {
                    match c {
                        '{' => open_blocks += 1,
                        '}' => open_blocks -= 1,
                        '(' => open_parens += 1,
                        ')' => open_parens -= 1,
                        '[' => open_brackets += 1,
                        ']' => open_brackets -= 1,
                        _ => (),
                    }
                }
                if open_blocks > 0 || open_parens > 0 || open_brackets > 0 {
                    block_offset = open_blocks;
                    current_prompt = "..";
                    continue;
                }
                current_prompt = default_prompt;
                last_state = state.clone();
                last_compiler = compiler.clone();
                // let _result = fly(file_name.clone(), line, DEBUG, false);
                // dbg!(&compiler.current_module().scanner.code);

                compiler
                    .current_module()
                    .scanner
                    .code
                    .push_str(&current_line);
                current_line = "".to_string();
                block_offset = 0;
                // dbg!(&compiler.current_module().scanner.code);
                let result = compiler.compile(DEBUG);
                if result.is_none() {
                    compiler = last_compiler;
                    continue;
                }

                let mut result = result.unwrap();
                // pop the return instructions
                result.modules[0].functions[0]
                    .chunk
                    .code
                    .retain(|x| x.op_code != OpReturn);
                let line_num = result.modules[0].functions[0]
                    .chunk
                    .code
                    .last()
                    .unwrap()
                    .line_num;
                result.modules[0].functions[0].chunk.code.push(Instr {
                    op_code: OpReturn,
                    line_num,
                });
                state = if let Some(mut s) = state {
                    while s.globals[0].len() < result.modules[0].identifiers.len() {
                        s.globals[0].push(Global::Uninit);
                    }
                    Some(s)
                } else {
                    state
                };
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

pub fn run_file(filename: String, debug: bool, wait: bool) -> InterpretResult {
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
        Ok(_) => fly(filename, s, debug, false, wait),
        Err(why) => {
            eprintln!("Failed to read {}: {}", path_display, why);
            exit(1);
        }
    }
}

#[cfg(feature = "debug")]
pub const DEBUG: bool = true;

#[cfg(not(feature = "debug"))]
pub const DEBUG: bool = false;
