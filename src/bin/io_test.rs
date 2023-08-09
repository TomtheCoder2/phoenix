use phoenix::compiler::{CompilationResult, Compiler};
use phoenix::vm::{ExecutionMode, VM};

fn main() {
    let code = "print(\"hello\");".to_string();
    let mut compiler = Compiler::new_default(&code, true, 0);
    let res = if let Some(res) = compiler.compile(false) {
        res
    } else {
        eprintln!("Compilation failed");
        return;
    };
    let encoded: Vec<u8> = bincode::serialize(&res).unwrap();
    println!("{:?}", encoded);

    let decoded: CompilationResult = bincode::deserialize(&encoded[..]).unwrap();
    let vm = VM::new(ExecutionMode::Default, decoded, false);
    vm.run();
}
