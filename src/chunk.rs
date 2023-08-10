use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::resolver::{Resolver, UpValue};
use crate::scanner::{Scanner, Token};
use crate::value::Value;

// todo: fix comments
#[derive(PartialEq, PartialOrd, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum OpCode {
    OpReturn,
    OpPop,

    // Index of the String name for this variable name in the identifiers vec
    OpDefineGlobal(usize),
    // ^
    OpGetGlobal(usize),
    // ^
    OpSetGlobal(usize),
    // ^
    OpGetSuper(usize),
    //  module_index, string name, arity
    OpCallGlobal(usize, usize, usize), // A combination of OpCall and OpGetGlobal

    OpGetModuleVar(usize, usize),

    // Index on the stack
    OpGetLocal(usize),

    OpSetLocal(usize),
    // ^
    /// Combines a GetProperty and a Call. Contains the exact same information. First usize is the index for the property name, second is for the arity, third is for the module index
    OpInvoke(usize, usize, usize),

    // WIP
    OpImport(usize),
    OpGetProperty(usize),
    // Index of the String name for this variable name in the identifiers vec corresponding with the property name
    OpSetProperty(usize),
    // ^
    // Optimization note: Is there any way to resolve properties at compile time? Phoenix allows arbitrary properties to be added at any time, so I don't believe it's possible
    OpGetUpvalue(usize),
    // upvalue index for a closure
    OpSetUpvalue(usize),
    // Wraps the top value of the stack (must be a PhoenixFunction) in a PhoenixClosure, capturing the appropriate UpValues at the same time
    OpClosure,

    // Jump ip offset
    OpJump(usize),
    OpJumpIfFalse(usize),
    // Jump backwards by offset
    OpLoop(usize),
    // Arity, module index
    OpCall(usize, usize),
    // Index into the classes vec for the ClassChunk object
    OpClass(usize),
    // Index of the constant we want to retrieve
    OpConstant(usize),

    OpNil,
    OpTrue,
    OpFalse,

    OpNegate,
    OpNot,

    OpAdd,
    OpAddAssign,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpEqual,
    OpGreater,
    OpLess,

    OpPrint,
    // get element of list at index
    OpGetIndex,
    // set element of list at index
    OpSetIndex,
    // create a new list with the last n elements on the stack
    OpCreateList(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Instr {
    pub op_code: OpCode,
    pub line_num: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Chunk {
    pub code: Vec<Instr>,
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Chunk {
    pub fn write_instruction(&mut self, instruction: Instr) {
        self.code.push(instruction);
    }

    pub fn new() -> Chunk {
        Chunk { code: Vec::new() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

/// Compile time representation of a function, ie its code, name, resolved closure information
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FunctionChunk {
    pub chunk: Chunk,
    pub name: Option<String>,
    // None for the top level script
    pub arity: usize,
    pub fn_type: FunctionType,
    pub upvalues: Option<Vec<UpValue>>, // None while the function is being defined and for functions without upvalues. If the function does have upvalues, this field must be set and must be binded with an OpClosure
}

impl FunctionChunk {
    pub fn new(name: Option<String>, arity: usize, fn_type: FunctionType) -> FunctionChunk {
        FunctionChunk {
            chunk: Chunk::new(),
            name,
            arity,
            fn_type,
            upvalues: None,
        }
    }

    pub fn set_upvalues(&mut self, upvalues: Vec<UpValue>) {
        self.upvalues = Some(upvalues);
    }
}

/// Compile time repr of a class
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ClassChunk {
    pub name: String,
    pub methods: HashMap<usize, usize>,
    pub superclass: Option<usize>,
    pub has_init: bool,
}

impl ClassChunk {
    pub fn new(name: String) -> ClassChunk {
        ClassChunk {
            name,
            methods: HashMap::new(),
            superclass: None,
            has_init: false,
        }
    }
}

/// Compile time repr of an imported module
#[derive(Debug, Clone)]
pub struct CompilerModuleChunk {
    /// The name of the module
    pub name: String,
    /// Whether or not this is the main module
    pub main: bool,
    /// The chunk of code for the module
    pub chunk: Chunk,
    pub scanner: Scanner,
    pub tokens: Vec<Token>,
    pub constants: Vec<Value>,
    pub functions: Vec<FunctionChunk>,
    pub classes: Vec<ClassChunk>,
    pub(crate) current_function: usize,
    pub current_class: Option<usize>,
    /// Which FunctionChunk should the the compiler return to after. Acts as a stack
    pub parent_functions: Vec<usize>,
    /// Manages the slots for the local variables and upvalues, represented as a Vec of individual ResolverNodes
    pub resolver: Resolver,
    pub identifier_constants: Vec<String>,
    // for now we ignore the init functions in modules
    pub has_init: bool,
}

impl CompilerModuleChunk {
    pub fn new(main: bool, name: String, file: String, code: String) -> CompilerModuleChunk {
        CompilerModuleChunk {
            main,
            name,
            chunk: Chunk::new(),
            functions: Vec::new(),
            constants: Vec::new(),
            classes: Vec::new(),
            current_function: 0,
            current_class: None,
            parent_functions: vec![],
            resolver: Resolver::new(),
            identifier_constants: vec![],
            has_init: false,
            scanner: Scanner::new(file, code, 0),
            tokens: vec![],
        }
    }
}

/// Compile time repr of an imported module, but without the scanner, instead with the file name
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ModuleChunk {
    /// The name of the module
    pub name: String,
    /// Whether or not this is the main module
    pub main: bool,
    /// The chunk of code for the module
    pub chunk: Chunk,
    /// The file name of the module
    pub file: String,
    pub constants: Vec<Value>,
    pub identifiers: Vec<String>,
    pub functions: Vec<FunctionChunk>,
    pub classes: Vec<ClassChunk>,
    // for now we ignore the init functions in modules
    pub has_init: bool,
}

impl ModuleChunk {
    pub fn new(main: bool, name: String, file: String) -> ModuleChunk {
        ModuleChunk {
            main,
            name,
            chunk: Chunk::new(),
            functions: Vec::new(),
            constants: Vec::new(),
            classes: Vec::new(),
            has_init: false,
            file,
            identifiers: vec![],
        }
    }

    pub fn from(module: CompilerModuleChunk) -> ModuleChunk {
        ModuleChunk {
            main: module.main,
            name: module.name,
            chunk: module.chunk,
            functions: module.functions,
            constants: module.constants,
            classes: module.classes,
            has_init: module.has_init,
            file: module.scanner.file,
            identifiers: module.identifier_constants,
        }
    }
}
