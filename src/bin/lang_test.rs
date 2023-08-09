use phoenix::compiler::Compiler;

fn main() {
    let code = "print 1 + 11;".to_string();
    let mut compiler = Compiler::new_file("script".to_string(), code, false);
    let res = compiler.compile(false).unwrap();
    // print the code
    println!("{:?}", res);
}
