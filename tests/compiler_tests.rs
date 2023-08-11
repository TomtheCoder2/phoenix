use phoenix_lang::chunk::FunctionType::{Function, Script};
use phoenix_lang::chunk::OpCode::*;
use phoenix_lang::chunk::{Chunk, FunctionChunk, Instr, ModuleChunk};
use phoenix_lang::compiler::{CompilationResult, Compiler};
use phoenix_lang::value::Value;
use phoenix_lang::value::Value::*;
use phoenix_lang::{chunk, VERSION};

/// Compile a piece of code and check that the result is as expected.
/// (Everything in one module)
fn do_test_functions(
    code: &str,
    constants: Vec<Value>,
    identifiers: Vec<String>,
    functions: Vec<FunctionChunk>,
    classes: Vec<chunk::ClassChunk>,
) {
    let result = Compiler::compile_code(code.to_string(), false);
    let cur_pos = result.clone().unwrap().cur_pos;
    assert_eq!(
        result,
        Some(CompilationResult {
            /* we ignore the version for now */
            version: VERSION.to_string(),
            modules: vec![ModuleChunk {
                name: "main".parse().unwrap(),
                main: true,
                file: "script".parse().unwrap(),
                constants,
                identifiers,
                functions,
                classes,
                has_init: false,
            }],
            modules_table: Default::default(),
            cur_pos,
        })
    );
}

/// Test only script without declaring other functions
fn do_test(
    code: &str,
    constants: Vec<Value>,
    identifiers: Vec<String>,
    functions: Vec<Instr>,
    classes: Vec<chunk::ClassChunk>,
) {
    do_test_functions(
        code,
        constants,
        identifiers,
        vec![FunctionChunk {
            arity: 0,
            chunk: Chunk { code: functions },
            name: None,
            upvalues: None,
            fn_type: Script,
        }],
        classes,
    );
}

#[test]
fn addition() {
    do_test(
        "1 + 2;",
        vec![Long(1), Long(2)],
        vec![],
        vec![
            Instr {
                op_code: OpConstant(0),
                line_num: 0,
            },
            Instr {
                op_code: OpConstant(1),
                line_num: 0,
            },
            Instr {
                op_code: OpAdd,
                line_num: 0,
            },
            Instr {
                op_code: OpPop,
                line_num: 0,
            },
            Instr {
                op_code: OpNil,
                line_num: 0,
            },
            Instr {
                op_code: OpReturn,
                line_num: 0,
            },
        ],
        vec![],
    );
}

#[test]
fn subtraction() {
    do_test(
        "1 - 2;",
        vec![Long(1), Long(2)],
        vec![],
        vec![
            Instr {
                op_code: OpConstant(0),
                line_num: 0,
            },
            Instr {
                op_code: OpConstant(1),
                line_num: 0,
            },
            Instr {
                op_code: OpSubtract,
                line_num: 0,
            },
            Instr {
                op_code: OpPop,
                line_num: 0,
            },
            Instr {
                op_code: OpNil,
                line_num: 0,
            },
            Instr {
                op_code: OpReturn,
                line_num: 0,
            },
        ],
        vec![],
    );
}

#[test]
fn multiplication() {
    do_test(
        "1 * 2;",
        vec![Long(1), Long(2)],
        vec![],
        vec![
            Instr {
                op_code: OpConstant(0),
                line_num: 0,
            },
            Instr {
                op_code: OpConstant(1),
                line_num: 0,
            },
            Instr {
                op_code: OpMultiply,
                line_num: 0,
            },
            Instr {
                op_code: OpPop,
                line_num: 0,
            },
            Instr {
                op_code: OpNil,
                line_num: 0,
            },
            Instr {
                op_code: OpReturn,
                line_num: 0,
            },
        ],
        vec![],
    );
}

#[test]
fn division() {
    do_test(
        "1 / 2;",
        vec![Long(1), Long(2)],
        vec![],
        vec![
            Instr {
                op_code: OpConstant(0),
                line_num: 0,
            },
            Instr {
                op_code: OpConstant(1),
                line_num: 0,
            },
            Instr {
                op_code: OpDivide,
                line_num: 0,
            },
            Instr {
                op_code: OpPop,
                line_num: 0,
            },
            Instr {
                op_code: OpNil,
                line_num: 0,
            },
            Instr {
                op_code: OpReturn,
                line_num: 0,
            },
        ],
        vec![],
    );
}

#[test]
fn complex1() {
    do_test(
        "1 + 2 * 3;",
        vec![Long(1), Long(2), Long(3)],
        vec![],
        vec![
            Instr {
                op_code: OpConstant(0),
                line_num: 0,
            },
            Instr {
                op_code: OpConstant(1),
                line_num: 0,
            },
            Instr {
                op_code: OpConstant(2),
                line_num: 0,
            },
            Instr {
                op_code: OpMultiply,
                line_num: 0,
            },
            Instr {
                op_code: OpAdd,
                line_num: 0,
            },
            Instr {
                op_code: OpPop,
                line_num: 0,
            },
            Instr {
                op_code: OpNil,
                line_num: 0,
            },
            Instr {
                op_code: OpReturn,
                line_num: 0,
            },
        ],
        vec![],
    );
}

#[test]
fn complex2() {
    do_test(
        "1 * 2 + 3;",
        vec![Long(1), Long(2), Long(3)],
        vec![],
        vec![
            Instr {
                op_code: OpConstant(0),
                line_num: 0,
            },
            Instr {
                op_code: OpConstant(1),
                line_num: 0,
            },
            Instr {
                op_code: OpMultiply,
                line_num: 0,
            },
            Instr {
                op_code: OpConstant(2),
                line_num: 0,
            },
            Instr {
                op_code: OpAdd,
                line_num: 0,
            },
            Instr {
                op_code: OpPop,
                line_num: 0,
            },
            Instr {
                op_code: OpNil,
                line_num: 0,
            },
            Instr {
                op_code: OpReturn,
                line_num: 0,
            },
        ],
        vec![],
    );
}

#[test]
fn calling_function() {
    do_test(
        "print(\"Hello World!\");",
        vec![PhoenixString("Hello World!".to_string())],
        vec![],
        vec![
            Instr {
                op_code: OpConstant(0),
                line_num: 0,
            },
            Instr {
                op_code: OpPrint,
                line_num: 0,
            },
            Instr {
                op_code: OpNil,
                line_num: 0,
            },
            Instr {
                op_code: OpReturn,
                line_num: 0,
            },
        ],
        vec![],
    );
}

#[test]
fn declare_and_call_function() {
    do_test_functions(
        "\
        fun foo() {\
            print(\"Hello World!\");\
        }\
        foo();\
    ",
        vec![PhoenixString("Hello World!".to_string()), PhoenixFunction(1)],
        vec!["foo".to_string()],
        vec![
            FunctionChunk {
                chunk: Chunk {
                    code: vec![
                        Instr {
                            op_code: OpConstant(1),
                            line_num: 0,
                        },
                        Instr {
                            op_code: OpDefineGlobal(0),
                            line_num: 0,
                        },
                        Instr {
                            op_code: OpCallGlobal(0, 0, 0),
                            line_num: 0,
                        },
                        Instr {
                            op_code: OpPop,
                            line_num: 0,
                        },
                        Instr {
                            op_code: OpNil,
                            line_num: 0,
                        },
                        Instr {
                            op_code: OpReturn,
                            line_num: 0,
                        },
                    ],
                },
                name: None,
                arity: 0,
                fn_type: Script,
                upvalues: None,
            },
            FunctionChunk {
                chunk: Chunk {
                    code: vec![
                        Instr {
                            op_code: OpConstant(0),
                            line_num: 0,
                        },
                        Instr {
                            op_code: OpPrint,
                            line_num: 0,
                        },
                        Instr {
                            op_code: OpNil,
                            line_num: 0,
                        },
                        Instr {
                            op_code: OpReturn,
                            line_num: 0,
                        },
                    ],
                },
                name: Some("foo".to_string()),
                arity: 0,
                fn_type: Function,
                upvalues: None,
            },
        ],
        vec![],
    );
}
