use phoenix::compiler::Compiler;

fn main() {
    let code = "print 1 + 11;".to_string();
    let mut compiler = Compiler::new_default(&code, false, 0);
    let res = compiler.compile(false).unwrap();
    // print the code
    println!("{:?}", res);
}
