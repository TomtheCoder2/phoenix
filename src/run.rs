use std::process::exit;
use std::time::Instant;

use clap::{Parser, Subcommand};
use lz4_compression::prelude::{compress, decompress};

use crate::compiler::{CompilationResult, Compiler};
use crate::io::write_to_file;
use crate::io::{get_file_as_byte_vec, read_file};
use crate::vm::{ExecutionMode, VM};
use crate::{debug, error, info, repl, run_file, DEBUG};

// Comments on how it works
// general: phx are scripts and phc are compiled scripts
// similar to rust:
// * phoenix build <file.phx> [-o <output.phc>] (default output is <file.phc>)
// * phoenix run <file> (either .phx or .phc)
// * phoenix (without any arguments the repl is started)

#[derive(Parser, Debug)]
#[command(
    author,
    version,
    about,
    long_about = "The compiler and interpreter for the phoenix programming language.\n\n\
To simply interpret a file, give it as the [input file] parameter.\n
To compile to a phc file, specify an output file with the output-file flag.\n
To start the REPL, simply dont provide any arguments.\n"
)]
/// The compiler and interpreter for the phoenix programming language.
struct Args {
    /// The command to execute
    #[command(subcommand)]
    command: Option<Commands>,
    /// The file to run
    file: Option<String>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Run a file
    Run {
        /// The file to run
        file: String,
    },
    /// Build a file
    Build {
        /// The file to build
        file: String,
        /// The output file
        #[arg(short, long)]
        output_file: Option<String>,
    },
    /// Compile a file (same as build)
    Compile {
        /// The file to compile
        file: String,
        /// The output file
        #[arg(short, long)]
        output_file: Option<String>,
    },
}

pub fn main() {
    let cli = Args::parse();
    if cli.command.is_some() {
        if cli.file.is_some() {
            error!("Can't run and build at the same time.");
        }
        match cli.command.unwrap() {
            Commands::Build { file, output_file } | Commands::Compile { file, output_file } => {
                if file == output_file.clone().unwrap_or("a.phc".to_string()) {
                    error!("The input file and the output file cannot be the same.");
                }
                compile(file, output_file.unwrap_or("a.phc".to_string()))
            }
            Commands::Run { file } => {
                run_f(file);
            }
        }
    } else if cli.file.is_none() {
        info!("Starting Phoenix Console mode...\nUse phoenix help, --help, or -h to open the help page.");
        if repl().is_ok() {}
        exit(0);
    } else {
        run_f(cli.file.unwrap());
    }
}

fn run_f(file: String) {
    // check if the file ends in .phx
    if file.ends_with(".phx") {
        run_file(file, DEBUG);
    } else {
        compiled_run(file);
    }
}

fn compiled_run(input_file: String) {
    let binary_data = match get_file_as_byte_vec(input_file) {
        Ok(s) => s,
        Err(e) => {
            error!("Error reading file: {}", e);
            exit(64);
        }
    };
    let decompressed_data = decompress(&binary_data).unwrap();
    let decoded: CompilationResult = bincode::deserialize(&decompressed_data[..]).unwrap();
    let mut vm = if DEBUG {
        VM::new(ExecutionMode::Trace, decoded, false)
    } else {
        VM::new(ExecutionMode::Default, decoded, false)
    };
    vm.run();
}

fn compile(input_file: String, actual_output_file: String) {
    let t1 = Instant::now();
    // compile the code an save it to the output file
    let file_name = input_file;
    let code = match read_file(file_name.clone()) {
        Ok(s) => s,
        Err(e) => {
            error!("Error reading file: {}", e);
            exit(64);
        }
    };
    let mut compiler = Compiler::new_file(file_name, code.clone(), true, 0, DEBUG);
    let res = if let Some(res) = compiler.compile(false) {
        res
    } else {
        error!("Compilation failed");
        return;
    };
    let encoded: Vec<u8> = bincode::serialize(&res).unwrap();
    info!("Size before compression: {} bytes", encoded.len());
    let compressed_data = compress(&encoded);
    // let decompressed_data = decompress(&compressed_data).unwrap();
    info!("Size after compression: {} bytes", compressed_data.len());
    if DEBUG {
        debug!("data: {:?}", compressed_data);
    }
    info!("Size of code: {} bytes", code.len());
    info!("Writing output to {}", actual_output_file);
    write_to_file(&actual_output_file, compressed_data);
    let t2 = Instant::now();
    info!("Compilation took {:?}", t2.duration_since(t1));
}
