use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::process::exit;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::chunk::{FunctionChunk, ModuleChunk};
use crate::compiler::{CompilationResult, Compiler};
use crate::vm::{ExecutionMode, VMState, VM};
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

pub const DEV: bool = false;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn fly(file: String, source: String, debug: bool, quiet: bool) -> InterpretResult {
    let mut compiler = Compiler::new_file(file, source, quiet, 0);
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
    let mut global_result = CompilationResult::new();
    let mut global_code = String::new();
    let mut curr_line = 0;
    let mut cur_pos = 0;
    let s = &String::new();
    let mut functions: Vec<FunctionChunk> = Vec::new();
    let file_name = "script.phx".to_string();
    let mut vm = VM::new(ExecutionMode::Default, CompilationResult::default(), false);
    let mut state = None;
    let mut modules = Vec::new();
    loop {
        // let readline = rl.readline(" \x1b[32m>>\x1b[0m");
        let readline = rl.readline(">>");
        match readline {
            Ok(line) => {
                if DEV {
                    curr_line += 1;
                    // println!("curr_line: {}, cur_pos: {cur_pos}", curr_line);
                    rl.add_history_entry(line.as_str())
                        .expect("failed to add history");
                    // line.push_str("\n");
                    global_code.push_str(&line);
                    // print whole code
                    println!("whole code: {}", global_code);
                    // let compiler_before = compiler.clone();
                    // s = &global_code;
                    // s = &line;
                    let p = cur_pos;
                    let sp = String::new();
                    for i in 0..global_code.chars().collect::<Vec<char>>().len() {
                        if i == p {
                            print!("|");
                        }
                        print!("{}", global_code.chars().nth(i).unwrap());
                    }
                    println!("|");
                    println!(
                        "curr_line: {}, cur_pos: {}: {}, global_string len: {}",
                        curr_line,
                        p,
                        sp,
                        global_code.len()
                    );
                    let mut compiler =
                        Compiler::new_file("script".to_string(), global_code.clone(), false, 0);
                    println!("{}|", compiler.current_module().scanner.code);
                    if compiler.new_start(
                        file_name.clone(),
                        global_code.clone(),
                        false,
                        curr_line,
                        cur_pos,
                    ) {
                        // remove the last line of code
                        let mut lines = global_code.lines().collect::<Vec<&str>>();
                        lines.pop();
                        compiler.new_start(file_name.clone(), s.clone(), false, curr_line, cur_pos);
                        global_code = lines.join("\n");
                        curr_line -= 1;
                        // compiler = compiler_before;
                        continue;
                    }
                    println!("{}|", compiler.current_module().scanner.code);
                    for f in &functions {
                        compiler.current_module().functions.push(f.clone());
                    }
                    println!(
                        "compiler functions len: {}",
                        compiler.current_module().functions.len()
                    );
                    println!(
                        "curr_line: {}, cur_pos: {}: {}, global_string len: {}",
                        curr_line,
                        p,
                        sp,
                        global_code.len()
                    );
                    // compiler.scanner.cur_pos = cur_pos;
                    let result = compiler.compile(DEBUG);
                    if result.is_none() {
                        // remove the last line of code
                        let mut lines = global_code.lines().collect::<Vec<&str>>();
                        lines.pop();
                        compiler.new_start(file_name.clone(), s.clone(), false, curr_line, cur_pos);
                        global_code = lines.join("\n");
                        curr_line -= 1;
                        // compiler = compiler_before;
                        continue;
                    }
                    println!("Compilation was successful");

                    let result = result.unwrap();
                    let was_cur_pos = cur_pos;
                    cur_pos = result.cur_pos;
                    let cp = global_result.clone();
                    // global_result.append(result.clone());
                    // let mut vm = if DEBUG {
                    //     VM::new(ExecutionMode::Trace, result, false)
                    // } else {
                    //     VM::new(ExecutionMode::Default, result, false)
                    // };
                    vm.append(result);
                    if vm.run() == InterpretResult::InterpretRuntimeError {
                        global_result = cp;
                        // remove the last line of code
                        let mut lines = global_code.lines().collect::<Vec<&str>>();
                        lines.pop();
                        global_code = lines.join("\n");
                        curr_line -= 1;
                        cur_pos = was_cur_pos;
                        // compiler = compiler_before;
                        continue;
                    }
                    for f in &compiler.current_module().functions {
                        // check if there is another function also called f.name and remove it
                        functions.retain(|fu| f.name != fu.name);
                        functions.push(f.clone());
                    }
                    global_code.push('\n');
                } else {
                    rl.add_history_entry(line.as_str())
                        .expect("failed to add history");
                    // let _result = fly(file_name.clone(), line, DEBUG, false);
                    let mut compiler = Compiler::new_file(file_name.clone(), line, DEBUG, 0);
                    let result = compiler.compile(DEBUG);
                    if result.is_none() {
                        continue;
                    }

                    let result = result.unwrap();
                    vm.append(result);
                    let s = vm.run_state(state.clone(), modules.clone());
                    if let InterpretOK(s, m) = s {
                        state = Some(s);
                        modules = m;
                        if DEBUG {
                            // println!("state: {:#?}, modules: {:#?}", state, modules);
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
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

pub const DEBUG: bool = false;
