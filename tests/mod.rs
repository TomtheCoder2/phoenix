fn test(mut n: i32) {
    while n > 0 {
        n -= 1;
    }
}

#[test]
fn fib_test() {
    // start time
    let start = std::time::Instant::now();
    test(4000000);
    // end time
    let end = std::time::Instant::now();
    // duration
    let duration = end.duration_since(start);
    println!("Time elapsed in expensive_function() is: {:?}", duration);
}


use phoenix_lang::compiler::{CompilationResult, Compiler};
use phoenix_lang::vm::{ExecutionMode, VM};

#[test]
fn io_test() {
    let code = "print(\"hello\");".to_string();
    let mut compiler = Compiler::new_file("script".to_string(), code, false, 0);
    let res = if let Some(res) = compiler.compile(false) {
        res
    } else {
        eprintln!("Compilation failed");
        return;
    };
    let encoded: Vec<u8> = bincode::serialize(&res).unwrap();
    println!("{:?}", encoded);

    let decoded: CompilationResult = bincode::deserialize(&encoded[..]).unwrap();
    let mut vm = VM::new(ExecutionMode::Default, decoded, false);
    vm.run();
}


#[test]
fn lang_test() {
    let code = "print(1 + 11);".to_string();
    let mut compiler = Compiler::new_file("script".to_string(), code, false, 0);
    let res = compiler.compile(false).unwrap();
    // print the code
    println!("{:?}", res);
}
