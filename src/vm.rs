use crate::chunk::OpCode::*;
use crate::chunk::{ClassChunk, FunctionChunk, Instr, ModuleChunk};
use crate::compiler::CompilationResult;
use crate::debug::*;
use crate::gc::GC;
use crate::native::*;
use crate::resolver::UpValue;
use crate::value::{
    is_falsey, values_equal, HeapObj, HeapObjType, HeapObjVal, ObjBoundMethod, ObjClosure,
    ObjInstance, ObjList, Value,
};
use crate::{error, phoenix_error, warn, InterpretResult, VERSION};
use std::collections::HashMap;

const FRAMES_MAX: usize = 255;

#[derive(Debug)]
pub enum ExecutionMode {
    Default,
    Trace,
}

/// This ended up not being very useful since we usually don't care what kind of deref error we get, they usually mean the same thing, that we tried to use a value in a way it wasn't supposed to be used
#[derive(Debug, Clone, Copy)]
pub enum DerefError {
    NotPointer,
    WrongType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    /// Index into the VM.modules Vec for which module is being called
    pub module: usize,
    /// Index into the VM.functions Vec for which function is being called
    function: usize,

    pub ip: usize,
    frame_start: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Global {
    Init(Value),
    Uninit,
}

// Is it good rust to split these into two very coupled but separate structs or is there a way to keep them together while not angering the borrow checker?
//
// I think this setup worked quite well, but I'm sure there's a better way to do it
/// Manages all the state involved with the VM's execution, namely the stack, global variables, the heap, and the call frames
#[derive(PartialEq, Debug, Clone)]
pub struct VMState {
    pub current_frame: CallFrame,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    // we have a Vec of Vecs because each module has its own set of globals
    pub(crate) globals: Vec<Vec<Global>>,
    gc: GC,
    // Not implemented due to it destroying my code => multiple upvalues pointing to the same original value in a function will NOT affect each other. This is a small enough edge case that I'm willing to just let it go
    // upvalues: Vec<Value>,
    /// stack of called modules for returning home after entering a new module for a function call
    module_stack: Vec<usize>,
}

impl VMState {
    fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(x) => x,
            None => {
                phoenix_error!("VM panic! Attempted to pop a value when the value stack was empty");
            }
        }
    }

    // Note to future self: peek_mut SHOULD NEVER BE IMPLEMENTED!
    // Values on the stack are always implicit copy/cloned, any persistent values must be allocated with the Gc and represented with PhoenixPointers instead

    fn peek(&self) -> &Value {
        self.peek_at(0)
    }

    fn peek_at(&self, dist: usize) -> &Value {
        match self.stack.get(self.stack.len() - dist - 1) {
            Some(x) => x,
            None => {
                phoenix_error!(
                    "VM panic! Attempted to peek a value when the value stack was empty"
                );
            }
        }
    }

    fn alloc(&mut self, val: HeapObj) -> Value {
        self.gc
            .alloc(val, &self.stack, &self.globals[self.current_frame.module])
    }

    // Fixme: Figure out how to not copy paste this code for mut and immut
    pub fn deref(&self, pointer: usize) -> &HeapObj {
        match self.gc.instances.get(pointer) {
            Some(x) => x,
            None => {
                phoenix_error!("VM panic! Invalid pointer");
            }
        }
    }

    fn deref_mut(&mut self, pointer: usize) -> &mut HeapObj {
        match self.gc.instances.get_mut(pointer) {
            Some(x) => x,
            None => {
                phoenix_error!("VM panic! Invalid pointer");
            }
        }
    }

    /// Attempts to
    /// 1. Take the given Value as a PhoenixPointer
    /// 2. Deref it into a HeapObj
    /// 3. Match the obj_types
    fn deref_into(
        &self,
        pointer_val: &Value,
        obj_type: HeapObjType,
    ) -> Result<&HeapObjVal, DerefError> {
        if let Value::PhoenixPointer(pointer) = pointer_val {
            let obj = self.deref(*pointer);
            if obj.obj_type == obj_type {
                Ok(&obj.obj)
            } else {
                Err(DerefError::WrongType)
            }
        } else {
            Err(DerefError::NotPointer)
        }
    }

    fn deref_into_mut(
        &mut self,
        pointer_val: &Value,
        obj_type: HeapObjType,
    ) -> Result<&mut HeapObjVal, DerefError> {
        if let Value::PhoenixPointer(pointer) = pointer_val {
            let obj = self.deref_mut(*pointer);
            if obj.obj_type == obj_type {
                Ok(&mut obj.obj)
            } else {
                Err(DerefError::WrongType)
            }
        } else {
            Err(DerefError::NotPointer)
        }
    }

    fn current_closure(&self) -> &ObjClosure {
        let pointer_val = match self.stack.get(self.current_frame.frame_start) {
            Some(x) => x,
            None => {
                phoenix_error!("VM panic! Unable to get current closure?");
            }
        };
        match self.deref_into(pointer_val, HeapObjType::PhoenixClosure) {
            Ok(closure_obj) => closure_obj.as_closure(),
            Err(x) => {
                phoenix_error!("VM panic! Unable to get current closure? {:?}", x);
            }
        }
    }

    fn current_closure_mut(&mut self) -> &mut ObjClosure {
        let pointer_val = match self.stack.get(self.current_frame.frame_start) {
            Some(x) => x,
            None => {
                phoenix_error!("VM panic! Unable to get current closure?");
            }
        }
            .clone();
        match self.deref_into_mut(&pointer_val, HeapObjType::PhoenixClosure) {
            Ok(closure_obj) => closure_obj.as_closure_mut(),
            Err(x) => {
                phoenix_error!("VM panic! Unable to get current closure? {:?}", x);
            }
        }
    }

    fn increment_ip(&mut self) {
        self.current_frame.ip += 1;
    }

    fn jump(&mut self, offset: usize) {
        self.current_frame.ip += offset - 1;
    }

    fn jump_back(&mut self, neg_offset: usize) {
        self.current_frame.ip -= neg_offset + 1;
    }

    fn capture_upvalue(&self, upvalue: &UpValue) -> Value {
        if upvalue.is_local {
            // Just copy the value from the current stack frame (which is the parents)
            self.stack[self.current_frame.frame_start + upvalue.index].clone()
        } else {
            // Look at the current frame's closure's upvalue vec and copy it from there
            let parent_closure = self.current_closure();
            parent_closure.values[upvalue.index].clone()
        }
    }

    /// Push an upvalue onto the stack
    fn push_upvalue(&mut self, index: usize) {
        let closure = self.current_closure();
        let val = match closure.values.get(index) {
            Some(x) => x,
            None => {
                phoenix_error!("VM panic! Attempted to push an upvalue that doesn't exist");
            }
        }
            .clone();
        self.stack.push(val);
    }

    /// Set an upvalue with the top value of the stack
    fn set_upvalue(&mut self, index: usize) {
        let val = self.peek().clone();
        let closure = self.current_closure_mut();
        closure.values[index] = val;
    }

    /// Checks if the targeted Value is callable {PhoenixPointer to a PhoenixClosure, NativeFn, PhoenixClass, PhoenixBoundMethod}, passes it to call() to continue attempting the call if necessary.
    ///
    /// Note: This function or call() must fulfill the promise made in Resolver about what value sits in slot 0 of the local variables.
    /// Whether that's 'this' or a placeholder
    ///
    /// Returns a String containing an error message or None
    fn call_value(
        &mut self,
        arg_count: usize,
        function_defs: &[FunctionChunk],
        class_defs: &[ClassChunk],
        init_slot: &Option<usize>,
        _module: &ModuleChunk,
    ) -> Option<String> {
        let callee = self.peek_at(arg_count);
        if let Value::PhoenixPointer(_) = callee {
            match self.deref_into(callee, HeapObjType::PhoenixClosure) {
                Ok(closure) => {
                    let closure = closure.as_closure();
                    let fn_index = closure.function;
                    self.call(fn_index, arg_count, function_defs)
                }
                Err(_) => Some(String::from("Can only call functions and classes")),
            }
        } else if let Value::PhoenixFunction(fn_index) = callee {
            let index = *fn_index;
            self.call(index, arg_count, function_defs)
        } else if let Value::PhoenixBoundMethod(method) = callee {
            let fn_index = method.method;
            let index = self.stack.len() - arg_count - 1; // Index to put the PhoenixPointer to represent the "this" variable
            self.stack[index] = Value::PhoenixPointer(method.pointer);
            self.call(fn_index, arg_count, function_defs)
        } else if let Value::PhoenixClass(class) = callee {
            let instance_obj = ObjInstance::new(*class);
            let class_def = &class_defs[*class];
            let ptr = self.alloc(HeapObj::new_instance(instance_obj));
            let index = self.stack.len() - arg_count - 1;
            self.stack[index] = ptr; // Replace the PhoenixClass with the pointer

            // Call the initializer if it exists
            // If the PhoenixClass was called with arguments the stack will look like this: PhoenixClass | arg1 | arg2
            // So we want to call with the stack as: PhoenixPointer => PhoenixInstance | arg1 | arg2
            // And we need the init() fn to return the PhoenixInstance
            if class_def.has_init {
                if init_slot.is_none() {
                    phoenix_error!("VM panic! Attempted to call a custom initializer without it existing as a method identifier?");
                }
                self.call(
                    *class_def.methods.get(&init_slot.unwrap()).unwrap(),
                    arg_count,
                    function_defs,
                )
            } else if arg_count != 0 {
                Some(format!(
                    "Expected 0 arguments but got {} instead",
                    arg_count
                ))
            } else {
                // hacky fix because class definitions dont return with a classic return opcode
                Some("back".to_string())
            }
        } else if let Value::NativeFunction(native_arg_count, native_fn) = callee {
            let native_fn = *native_fn;
            self.call_native(&native_fn, arg_count, *native_arg_count)
        } else {
            Some(String::from("Can only call functions and classes"))
        }
    }

    /// Attempts to call a function with the values on the stack, with the given # of arguments
    fn call(
        &mut self,
        fn_index: usize,
        arg_count: usize,
        function_defs: &[FunctionChunk],
    ) -> Option<String> {
        let target_fn = match function_defs.get(fn_index) {
            Some(x) => x,
            None => return Some(format!("Function with index {} does not exist", fn_index)),
        };
        if arg_count != target_fn.arity {
            return Some(format!(
                "Expected {} arguments but got {} instead",
                target_fn.arity, arg_count
            ));
        }
        if self.frames.len() == FRAMES_MAX {
            return Some(String::from("Stack overflow"));
        }

        let mut frame = CallFrame {
            module: self.current_frame.module,
            function: fn_index,
            ip: 0,
            frame_start: self.stack.len() - arg_count - 1,
        };

        // Swap on the new call frame for the old one
        std::mem::swap(&mut self.current_frame, &mut frame);

        // Put the old one onto the stack
        self.frames.push(frame);
        None
    }

    /// Attempts to call a native (rust) function
    fn call_native(
        &mut self,
        native_fn: &NativeFn,
        arg_count: usize,
        native_arg_count: Option<usize>,
    ) -> Option<String> {
        let mut args: Vec<Value> = Vec::new();
        for _ in 0..arg_count {
            args.push(self.pop());
        }
        self.pop(); // Pop off the Value::NativeFunction
        args.reverse();
        if let Some(n_a_count) = native_arg_count {
            if arg_count != n_a_count {
                return Some(format!(
                    "Expected {} arguments but got {} instead",
                    n_a_count, arg_count
                ));
            }
        }
        let result = match native_fn(args) {
            Ok(x) => x,
            Err(e) => return Some(e),
        };
        self.stack.push(result);
        None
    }

    /// Defines all native functions
    ///
    /// Searches for references to native functions and adds them in if they're used in the program
    fn define_std_lib(&mut self, identifiers: &[String]) {
        for (str, nf) in match NATIVE_FUNCTIONS.lock() {
            Ok(x) => x,
            Err(_) => phoenix_error!("Failed to lock native functions mutex"),
        }
            .iter()
        {
            if let Some(index) = identifiers.iter().position(|x| x == str) {
                self.globals[self.current_frame.module][index] =
                    Global::Init(Value::NativeFunction(nf.0, nf.1));
            }
        }
    }

    /// Initializes the VMState with:
    ///
    /// - A CallFrame for function #0 and module #0 (should be the main module)
    /// - Defined global variables for the native functions
    /// - A Value::PhoenixFunction for function #0 pushed onto the stack => Satisfies the resolver assumption that the first locals slot is filled with something
    /// update the globals each time a new module is loaded
    fn new(identifiers: &Vec<String>) -> VMState {
        let first_fn = CallFrame {
            module: 0,
            function: 0,
            ip: 0,
            frame_start: 0,
        };

        let first_val = Value::PhoenixFunction(0);
        let stack = vec![first_val];

        let mut state = VMState {
            current_frame: first_fn,
            stack,
            frames: Vec::new(),
            // globals: vec![Global::Uninit; identifiers.len()],
            globals: vec![vec![Global::Uninit; identifiers.len()]],
            gc: GC::new(),
            module_stack: vec![0],
        };

        // todo: make this work with modules
        state.define_std_lib(identifiers);
        state
    }
}

/// Contains all the information outputted by the compiler
/// ie: All function and class definitions
pub struct VM {
    quiet_mode: bool,
    mode: ExecutionMode,
    pub modules_cache: Vec<ModuleChunk>,
    pub modules_table: HashMap<String, usize>,
    init_slot: Option<usize>,
}

impl VM {
    pub fn new(mode: ExecutionMode, result: CompilationResult, quiet: bool) -> VM {
        // compare version of the compilationResult and the VM
        if result.version != *VERSION {
            warn!(
                "Version mismatch! Expected {} but got {}",
                VERSION, result.version
            );
        }
        let init_slot = result
            .modules
            .get(0)
            .and_then(|x| x.clone().identifiers.iter().position(|x| x == "init"));
        let modules = result.modules;
        VM {
            quiet_mode: quiet,
            mode,
            modules_cache: modules,
            init_slot,
            modules_table: result.modules_table,
        }
    }

    pub fn append(&mut self, result: CompilationResult) {
        let init_slot = result.modules[0]
            .clone()
            .identifiers
            .iter()
            .position(|x| x == "init");
        if !self.modules_cache.is_empty() { self.modules_cache[0].functions[0].chunk.code.pop(); }
        self.modules_cache.extend(result.modules);
        self.modules_table.extend(result.modules_table);
        self.init_slot = init_slot;
    }

    fn runtime_error(&self, msg: &str, state: &VMState, modules: &[ModuleChunk]) {
        if self.quiet_mode {
            return;
        }

        error!("{}", msg);
        for call_frame in [state.current_frame.clone()]
            .iter()
            .chain(state.frames.iter().rev())
        {
            let function = &modules[state.current_frame.module]
                .functions
                .get(call_frame.function)
                .unwrap();
            eprint!(
                "[{}:{}] in ",
                modules[state.current_frame.module].file,
                function.chunk.code.get(call_frame.ip).unwrap().line_num + 1
            );
            match &function.name {
                Some(name) => eprintln!("{}", name),
                None => eprintln!("script"),
            }
        }
    }

    // /// Returns the current module
    // pub(crate) fn current_module(state: &VMState, modules: &Vec<ModuleChunk>) -> &ModuleChunk {
    //     &modules[state.current_frame.module]
    // }

    /// Should only be used for getting debugging and error reporting
    ///
    /// * For the global instructions, just the index should suffice
    /// * For instance properties and fields, the hashmaps are keyed on the usize corresponding to the identifier string
    /// * Local variable names are erased completely by the resolver at compile time
    fn get_variable_name<'a>(
        index: usize,
        state: &VMState,
        modules: &'a [ModuleChunk],
    ) -> &'a String {
        let name_val = &modules[state.current_frame.module].identifiers.get(index);
        if let Some(var_name) = name_val {
            var_name
        } else {
            panic!("VM panic: Found a non PhoenixString value for a variable name");
        }
    }

    // fn get_current_code(&self, state: &VMState, modules: &Vec<ModuleChunk>) -> &Vec<Instr> {
    //     &modules[state.current_frame.module]
    //         .functions
    //         .get(state.current_frame.function)
    //         .unwrap()
    //         .chunk
    //         .code
    // }

    pub fn run(&mut self) -> InterpretResult {
        self.run_state(None, Vec::new())
    }

    pub fn run_state(&mut self, state: Option<VMState>, m: Vec<ModuleChunk>) -> InterpretResult {
        if let ExecutionMode::Trace = self.mode {
            eprintln!("== Starting execution | Mode: {:?} ==", self.mode);
            debug_print_constants(self, &self.modules_cache);
        }

        // look at the functions in the first module
        let mut state = if let Some(s) = state {
            s
        } else {
            VMState::new(&self.modules_cache[0].identifiers)
        };
        let modules = self.modules_cache.clone();
        self.modules_cache = m;
        // todo: make this work with modules
        // modules[0].define_std_lib(&modules[0].identifiers, &modules[0]);
        // let mut states = vec![&mut state];
        // let mut parent_states = Vec::new();

        // Makes getting new instructions faster
        // Update this vec whenever
        let mut current_code = &modules[state.current_frame.module]
            .functions
            .get(state.current_frame.function)
            .unwrap()
            .chunk
            .code[..];

        // Move this into a match arm that matches all the binary ops, and then matches on the individual opcodes?
        macro_rules! op_binary {
            ($val_type: path, $oper: tt) => {
                {
                    //if let ($val_type(a), $val_type(b)) = (self.pop(), self.pop()) {
                    let var_a = state.pop();
                    let var_b = state.pop();
                    if let (Value::Float(a), Value::Float(b)) = (var_a.clone(), var_b.clone()) {
                        state.stack.push($val_type(b $oper a))
                    } else if let (Value::Long(a), Value::Long(b)) = (var_a, var_b) {
                        state.stack.push($val_type(b $oper a))
                    } else {
                        self.runtime_error("Operands must be numbers", &state, &modules);
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
            };
            ($val_type: path, $val_type2: path, $oper: tt) => {
                {
                    //if let ($val_type(a), $val_type(b)) = (self.pop(), self.pop()) {
                    let var_a = state.pop();
                    let var_b = state.pop();
                    if let (Value::Float(a), Value::Float(b)) = (var_a.clone(), var_b.clone()) {
                        state.stack.push($val_type(b $oper a))
                    } else if let (Value::Long(a), Value::Long(b)) = (var_a.clone(), var_b.clone()) {
                        state.stack.push($val_type2(b $oper a))
                    } else if let (Value::Long(_a), Value::Float(_b)) = (var_a.clone(), var_b.clone()) {
                        self.runtime_error(concat!("Operands must have the same type for the ", stringify!($oper), " operation. (long and float)"), &state, &modules);
                        return InterpretResult::InterpretRuntimeError;
                    } else if let (Value::Float(_a), Value::Long(_b)) = (var_a.clone(), var_b.clone()) {
                        self.runtime_error(concat!("Operands must have the same type for the ", stringify!($oper), " operation. (float and long)"), &state, &modules);
                        return InterpretResult::InterpretRuntimeError;
                    } else {
                        self.runtime_error(concat!("Operands must be numbers for the ", stringify!($oper), " operation"), &state, &modules);
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
            }
        }

        loop {
            let instr = match current_code.get(state.current_frame.ip) {
                Some(instr) => instr,
                None => {
                    phoenix_error!(
                        "Tried to access an invalid instruction, index: {}, length: {}",
                        state.current_frame.ip,
                        current_code.len()
                    );
                }
            };
            state.increment_ip(); // Preincrement the ip so OpLoops to 0 are possible

            if let ExecutionMode::Trace = self.mode {
                debug_trace(self, instr, &state, &modules);
            }

            match instr.op_code {
                OpImport(module_index) => {
                    // println!(
                    //     "Importing module: {}, stack len: {}",
                    //     module_index,
                    //     state.stack.len()
                    // );
                    if state.frames.len() == FRAMES_MAX {
                        self.runtime_error("Stack overflow", &state, &modules);
                        return InterpretResult::InterpretRuntimeError;
                    }
                    let mut frame = CallFrame {
                        module: module_index,
                        function: 0,
                        ip: 0,
                        frame_start: 0,
                    };

                    // Swap on the new call frame for the old one
                    std::mem::swap(&mut state.current_frame, &mut frame);

                    // Put the old one onto the stack
                    state.frames.push(frame);
                    current_code = &modules[state.current_frame.module]
                        .functions
                        .get(state.current_frame.function)
                        .unwrap()
                        .chunk
                        .code[..];
                    if state.globals.len() < module_index + 1 {
                        // println!("Pushing new globals for module: {:?}", module_index);
                        let len = modules[module_index].identifiers.len();
                        state.globals.push(vec![Global::Uninit; len]);
                    }
                }
                OpReturn => {
                    let result = state.pop(); // Save the result (the value on the top of the stack)
                    for _ in 0..(state.stack.len() - state.current_frame.frame_start) {
                        // Clean up the call frame part of that stack
                        state.pop();
                    }

                    if state.frames.is_empty() {
                        return InterpretResult::InterpretOK(state, modules);
                    } else {
                        state.current_frame = state.frames.pop().unwrap(); // Update the current frame
                        if state.module_stack != vec![0] {
                            state.current_frame.module = state.module_stack.pop().unwrap();
                        }
                        current_code = &modules[state.current_frame.module]
                            .functions
                            .get(state.current_frame.function)
                            .unwrap()
                            .chunk
                            .code[..]; // Update the current code
                        state.stack.push(result); // Push the result back
                    }
                }
                OpPop => {
                    state.pop();
                }
                OpDefineGlobal(index) => {
                    let var_val = state.pop();
                    state.globals[state.current_frame.module]
                        [index] = Global::Init(var_val);
                }
                OpCallGlobal(module_index, index, arity) => {
                    let cur_module = state.current_frame.module;
                    state.module_stack.push(cur_module);
                    state.current_frame.module = module_index;
                    let var_val = &state.globals[state.current_frame.module][index];
                    match var_val {
                        Global::Init(x) => {
                            let new = x.clone();
                            let index = state.stack.len() - arity;
                            state.stack.insert(index, new);
                            let result = state.call_value(
                                arity,
                                &modules[state.current_frame.module].functions,
                                &modules[state.current_frame.module].classes,
                                &self.init_slot,
                                &modules[state.current_frame.module],
                            );

                            if let Some(msg) = result {
                                if msg == "back" {
                                    // when classes are initialized, there is no return opcode emitted, so we have to return to the old module by ourself
                                    state.current_frame.module = state.module_stack.pop().unwrap();
                                } else {
                                    self.runtime_error(&msg[..], &state, &modules);
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            }
                            current_code = &modules[state.current_frame.module]
                                .functions
                                .get(state.current_frame.function)
                                .unwrap()
                                .chunk
                                .code[..]; // Update the current code
                        }
                        _ => {
                            self.runtime_error(
                                format!(
                                    "Undefined variable '{}'",
                                    VM::get_variable_name(index, &state, &modules)
                                )
                                    .as_str(),
                                &state,
                                &modules,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpGetGlobal(index) => {
                    let var_val = &state.globals[state.current_frame.module][index];
                    match var_val {
                        Global::Init(x) => {
                            let new = x.clone();
                            state.stack.push(new)
                        }
                        _ => {
                            self.runtime_error(
                                format!(
                                    "Undefined variable '{}'",
                                    VM::get_variable_name(index, &state, &modules)
                                )
                                    .as_str(),
                                &state,
                                &modules,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpGetModuleVar(module_index, index) => {
                    // println!("globals: {:?}", state.globals);
                    let var_val = &state.globals[module_index][index];
                    match var_val {
                        Global::Init(x) => {
                            let new = x.clone();
                            state.stack.push(new)
                        }
                        _ => {
                            self.runtime_error(
                                format!(
                                    "Undefined variable '{}'",
                                    VM::get_variable_name(index, &state, &modules)
                                )
                                    .as_str(),
                                &state,
                                &modules,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpSetGlobal(index) => {
                    // We don't want assignment to pop the value since this is an expression
                    // this will almost always be in a expression statement, which will pop the value
                    let var_val = state.peek().clone();
                    match &state.globals[state.current_frame.module][index] {
                        Global::Init(_) => {
                            state.globals[state.current_frame.module][index] = Global::Init(var_val)
                        } // We require it to be initialized (ie defined earlier by OpDefineGlobal)
                        _ => {
                            self.runtime_error(
                                format!(
                                    "Undefined variable '{}'",
                                    VM::get_variable_name(index, &state, &modules)
                                )
                                    .as_str(),
                                &state,
                                &modules,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpGetLocal(index) => state
                    .stack
                    .push(state.stack[state.current_frame.frame_start + index].clone()), // Note: We gotta clone these values around the stack because our operators pop off the top and we also don't want to modify the variable value
                OpSetLocal(index) => {
                    let dest = state.current_frame.frame_start + index;
                    state.stack[dest] = state.peek().clone(); // Same idea as OpSetGlobal, don't pop value since it's an expression
                }
                OpInvoke(name_index, arg_count, module_index) => {
                    let cur_module = state.current_frame.module;
                    state.module_stack.push(cur_module);
                    state.current_frame.module = module_index;
                    let pointer_val = state.peek_at(arg_count).clone();
                    let obj = if let Value::PhoenixPointer(pointer) = pointer_val {
                        state.deref(pointer)
                    } else {
                        self.runtime_error(
                            "Can only invoke methods on instances and lists",
                            &state,
                            &modules,
                        );
                        return InterpretResult::InterpretRuntimeError;
                    };

                    let result =
                        // match state.deref_into(&pointer_val.clone(), HeapObjType::PhoenixInstance) {
                        match &obj.obj {
                            HeapObjVal::PhoenixInstance(_) => {
                                let instance = obj.obj.as_instance();
                                let class_def =
                                    &&modules[state.current_frame.module].classes[instance.class];
                                if instance.fields.contains_key(&name_index) {
                                    // Guard against the weird edge case where instance.thing() is actually calling a closure instance.thing, not a method invocation
                                    let value = instance.fields.get(&name_index).unwrap().clone();
                                    let index = state.stack.len() - 1 - arg_count;
                                    state.stack[index] = value; // Remove the instance and replace with the value
                                    state.call_value(
                                        arg_count,
                                        &modules[state.current_frame.module].functions,
                                        &modules[state.current_frame.module].classes,
                                        &self.init_slot,
                                        &modules[state.current_frame.module],
                                    )
                                    // Perform the call
                                } else if class_def.methods.contains_key(&name_index) {
                                    // We know that the top of the stack is PhoenixPointer | arg1 | arg2
                                    // So we can go ahead and call
                                    let fn_index = class_def.methods.get(&name_index).unwrap();
                                    state.call(
                                        *fn_index,
                                        arg_count,
                                        &modules[state.current_frame.module].functions,
                                    )
                                } else {
                                    Some(format!(
                                        "Undefined property '{}' in {:?}",
                                        VM::get_variable_name(name_index, &state, &modules),
                                        instance
                                    ))
                                }
                            }
                            HeapObjVal::PhoenixList(_) => {
                                // check if its a list
                                let value = state.pop();
                                let curr_module = state.current_frame.module;
                                // we need to deref the pointer to get the actual list
                                match state.deref_into_mut(&pointer_val, HeapObjType::PhoenixList) {
                                    Ok(&mut ref mut list) => {
                                        match list {
                                            HeapObjVal::PhoenixList(ref mut list) => {
                                                match &*modules[curr_module].identifiers[name_index] {
                                                    "push" => {
                                                        list.values.push(value);
                                                    }
                                                    "pop" => {
                                                        if let Some(val) = list.values.pop() {
                                                            state.stack.push(val);
                                                        } else {
                                                            self.runtime_error(
                                                                "Attempted to pop from an empty list",
                                                                &state,
                                                                &modules,
                                                            );
                                                            return InterpretResult::InterpretRuntimeError;
                                                        }
                                                    }
                                                    "len" => {
                                                        let len = list.values.len() as i64;
                                                        state.stack.push(Value::Long(len));
                                                    }
                                                    _ => {
                                                        self.runtime_error(
                                                            format!("Function {} not found on list", &*modules[curr_module].identifiers[name_index]).as_str(),
                                                            &state,
                                                            &modules,
                                                        );
                                                        return InterpretResult::InterpretRuntimeError;
                                                    }
                                                };
                                            }
                                            _ => {
                                                self.runtime_error(
                                                    "Attempted to index a non-indexable value",
                                                    &state,
                                                    &modules,
                                                );
                                                return InterpretResult::InterpretRuntimeError;
                                            }
                                        };
                                        None
                                    }
                                    Err(_) => {
                                        Some(String::from(
                                            "Can only invoke methods on class instances and lists",
                                        ))
                                    }
                                }
                            }
                            _ => Some(format!(
                                "Can only invoke methods on instances and lists, not {:?}",
                                obj
                            )),
                        };

                    if let Some(error) = result {
                        self.runtime_error(error.as_str(), &state, &modules);
                        return InterpretResult::InterpretRuntimeError;
                    }
                    current_code = &modules[state.current_frame.module]
                        .functions
                        .get(state.current_frame.function)
                        .unwrap()
                        .chunk
                        .code[..]; // Update the current code
                }
                OpGetProperty(name_index) => {
                    let pointer_val = state.peek();

                    // Todo: Combine this and SetProperty into a macro so it doesn't hurt me everytime i have to read this
                    match state.deref_into(pointer_val, HeapObjType::PhoenixInstance) {
                        Ok(instance) => {
                            let instance = instance.as_instance();
                            if instance.fields.contains_key(&name_index) {
                                // See if we tried to get a field
                                let value = instance.fields.get(&name_index).unwrap().clone();
                                state.pop(); // Remove the instance
                                state.stack.push(value); // Replace with the value
                            } else {
                                let class_chunk =
                                    &&modules[state.current_frame.module].classes[instance.class]; // if not a field, then we must be getting a function. Create a PhoenixBoundMethod for it
                                if class_chunk.methods.contains_key(&name_index) {
                                    let bound_value = ObjBoundMethod {
                                        method: *class_chunk.methods.get(&name_index).unwrap(),
                                        pointer: pointer_val.as_pointer(),
                                    };
                                    state.pop(); // Remove the instance
                                    state.stack.push(Value::PhoenixBoundMethod(bound_value));
                                    // Replace with bound method
                                } else {
                                    self.runtime_error(
                                        format!(
                                            "Undefined property '{}' in {:?}",
                                            VM::get_variable_name(name_index, &state, &modules),
                                            instance
                                        )
                                            .as_str(),
                                        &state,
                                        &modules,
                                    );
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            }
                        }
                        Err(_) => {
                            let msg = format!("Only class instances can access properties with '.' Found {} instead", pointer_val.to_string(self, &state, &modules));
                            self.runtime_error(msg.as_str(), &state, &modules);
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpSetProperty(name_index) => {
                    // Fixme: this is nearly identical to OpGetProperty, is there any way to combine them nicely?
                    let val = state.pop();
                    let pointer_val = state.peek().clone();

                    match state.deref_into_mut(&pointer_val, HeapObjType::PhoenixInstance) {
                        Ok(instance) => {
                            let instance = instance.as_instance_mut();
                            instance.fields.insert(name_index, val.clone());
                        }
                        Err(_) => {
                            let msg = format!("Only class instances can access properties with '.' Found {} instead", pointer_val.to_string(self, &state, &modules));
                            self.runtime_error(msg.as_str(), &state, &modules);
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }

                    // We return on an error, so we can clean up the stack now
                    state.pop(); // Instance
                    state.stack.push(val); // Return the value to the stack
                }
                // This is almost identical to OpGetProperty, but it goes one extra jump to get the method from the superclass, and binds it to itself
                OpGetSuper(name_index) => {
                    let pointer_val = state.peek();
                    let superclass_val = state.peek_at(1);
                    if let Value::PhoenixClass(superclass) = superclass_val {
                        // Todo: Combine this and SetProperty into a macro so it doesn't hurt me everytime i have to read this
                        match state.deref_into(pointer_val, HeapObjType::PhoenixInstance) {
                            Ok(instance) => {
                                let instance = instance.as_instance();
                                let superclass_chunk =
                                    &&modules[state.current_frame.module].classes[*superclass];
                                if superclass_chunk.methods.contains_key(&name_index) {
                                    let bound_value = ObjBoundMethod {
                                        method: *superclass_chunk.methods.get(&name_index).unwrap(),
                                        pointer: pointer_val.as_pointer(),
                                    };
                                    // println!("Superclass get method found method {:?} ", bound_value);
                                    // println!("Superclass methods {:?}", superclass_chunk.methods);
                                    // println!("Superclass for {:?} is {:?}", instance, class_chunk.superclass);
                                    state.pop(); // Remove the instance
                                    state.stack.push(Value::PhoenixBoundMethod(bound_value));
                                    // Replace with bound method
                                } else {
                                    self.runtime_error(
                                        format!(
                                            "Undefined superclass method '{}' for {}",
                                            VM::get_variable_name(name_index, &state, &modules),
                                            &modules[state.current_frame.module]
                                                .classes
                                                .get(instance.class)
                                                .unwrap()
                                                .name,
                                        )
                                            .as_str(),
                                        &state,
                                        &modules,
                                    );
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            }
                            Err(_) => {
                                panic!(
                                    "VM panic! Failed to obtain instance PhoenixPointer for super"
                                );
                            }
                        }
                    } else {
                        panic!("VM panic! Failed to obtain superclass index for super, got {:?} instead", superclass_val);
                    }
                }

                OpGetUpvalue(index) => {
                    state.push_upvalue(index);
                }
                OpSetUpvalue(index) => {
                    state.set_upvalue(index);
                }

                OpClosure => {
                    if let Value::PhoenixFunction(function) = state.pop() {
                        let mut closure = ObjClosure::new(function); // Capture values into the closure here

                        let fn_chunk = &modules[state.current_frame.module]
                            .functions
                            .get(function)
                            .unwrap();
                        for upvalue in fn_chunk.upvalues.as_ref().unwrap().iter() {
                            closure.values.push(state.capture_upvalue(upvalue))
                        }
                        let ptr = state.alloc(HeapObj::new_closure(closure));
                        state.stack.push(ptr);
                    } else {
                        panic!("VM panic! Attempted to wrap a non-function value in a closure");
                    }
                }

                OpJump(offset) => state.jump(offset),
                OpJumpIfFalse(offset) => {
                    if is_falsey(state.peek()) {
                        // Does not pop the value off the top of the stack because we need them for logical operators
                        state.jump(offset);
                    }
                }
                OpLoop(neg_offset) => state.jump_back(neg_offset),
                OpCall(arity, module_index) => {
                    let callee = state.peek_at(arity);
                    // do we need to switch modules?
                    if let Value::PhoenixModule(module) = callee {
                        state.current_frame.module = *module;
                        // pop the module off the stack
                        state.pop();
                    }
                    let cur_module = state.current_frame.module;
                    state.module_stack.push(cur_module);
                    state.current_frame.module = module_index;
                    let result = state.call_value(
                        arity,
                        &modules[state.current_frame.module].functions,
                        &modules[state.current_frame.module].classes,
                        &self.init_slot,
                        &modules[state.current_frame.module],
                    );
                    current_code = &modules[state.current_frame.module]
                        .functions
                        .get(state.current_frame.function)
                        .unwrap()
                        .chunk
                        .code[..]; // Update the current code
                    if let Some(msg) = result {
                        self.runtime_error(&msg[..], &state, &modules);
                        return InterpretResult::InterpretRuntimeError;
                    }
                    // state.current_frame.module = cur_module;
                }

                OpClass(index) => state.stack.push(Value::PhoenixClass(index)),
                OpConstant(index) => state
                    .stack
                    .push(modules[state.current_frame.module].constants[index].clone()),
                OpTrue => state.stack.push(Value::Bool(true)),
                OpFalse => state.stack.push(Value::Bool(false)),
                OpNil => state.stack.push(Value::Nil),
                OpAdd => {
                    let t = (state.pop(), state.pop());
                    if let (Value::PhoenixString(a), Value::PhoenixString(b)) = t {
                        state
                            .stack
                            .push(Value::PhoenixString(format!("{}{}", b, a)))
                    } else if let (Value::Float(a), Value::Float(b)) = t {
                        state.stack.push(Value::Float(a + b))
                    } else if let (Value::Long(a), Value::Long(b)) = t {
                        state.stack.push(Value::Long(a + b))
                    } else {
                        self.runtime_error(
                            "Operands must be numbers or strings and must have the same type",
                            &state,
                            &modules,
                        );
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
                OpAddAssign => {
                    // add the current value on the stack to the one before
                    let t = (&state.pop(), &state.pop());
                    dbg!(t);
                }
                OpDivide => op_binary!(Value::Float, Value::Long, /),
                OpSubtract => op_binary!(Value::Float, Value::Long, -),
                OpMultiply => op_binary!(Value::Float, Value::Long, *),
                OpGreater => op_binary!(Value::Bool, >),
                OpLess => op_binary!(Value::Bool, <),
                OpEqual => {
                    let t = (&state.pop(), &state.pop());
                    state.stack.push(Value::Bool(values_equal(t)));
                }

                OpNot => {
                    let val = Value::Bool(is_falsey(&state.pop()));
                    state.stack.push(val);
                }
                OpNegate => {
                    let value = state.pop().as_float();
                    match value {
                        Some(x) => state.stack.push(Value::Float(x * -1.0)),
                        None => {
                            let value = state.pop().as_long();
                            match value {
                                Some(x) => state.stack.push(Value::Long(-x)),
                                None => {
                                    self.runtime_error(
                                        "Attempted to negate a non-number value",
                                        &state,
                                        &modules,
                                    );
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            }
                        }
                    }
                }
                OpPrint => {
                    println!("{}", state.pop().to_string(self, &state, &modules));
                }
                OpGetIndex => {
                    let index = match state.pop() {
                        Value::Long(i) => i as usize,
                        Value::Float(i) => i as usize,
                        _ => {
                            self.runtime_error(
                                "Attempted to index a non-iterable value",
                                &state,
                                &modules,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    let value = state.pop();
                    match value {
                        Value::PhoenixString(s) => {
                            let val = match s.chars().nth(index) {
                                Some(c) => c,
                                None => {
                                    self.runtime_error(
                                        format!("Attempted to index a string with an out-of-bounds index (index: {}, length: {})", index, s.len()).as_str(),
                                        &state,
                                        &modules,
                                    );
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            };
                            state.stack.push(Value::PhoenixString(val.to_string()));
                        }
                        Value::PhoenixPointer(list_index) => {
                            // get the list from the allocated lists
                            let v = state.deref_mut(list_index);
                            let value = match v.obj {
                                HeapObjVal::PhoenixList(ref mut list) => {
                                    if list.values.len() <= index {
                                        self.runtime_error(
                                            format!("Attempted to index a list with an out-of-bounds index (index: {}, length: {})", index, list.values.len()).as_str(),
                                            &state,
                                            &modules,
                                        );
                                        return InterpretResult::InterpretRuntimeError;
                                    }
                                    list.values[index].clone()
                                }
                                _ => {
                                    self.runtime_error(
                                        "Attempted to index a non-indexable value",
                                        &state,
                                        &modules,
                                    );
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            };
                            state.stack.push(value);
                        }
                        _ => {
                            self.runtime_error(
                                "Attempted to index a non-indexable value",
                                &state,
                                &modules,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpSetIndex => {
                    // the new value
                    let value = state.pop();
                    // the index
                    let index = match state.pop() {
                        Value::Long(i) => i as usize,
                        Value::Float(i) => i as usize,
                        _ => {
                            self.runtime_error(
                                "Attempted to index a non-iterable value",
                                &state,
                                &modules,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    // the target (list or string)
                    let mut target = state.pop();
                    match target {
                        Value::PhoenixString(s) => {
                            if s.len() <= index {
                                self.runtime_error(
                                    format!("Attempted to index a string with an out-of-bounds index (index: {}, length: {})", index, s.len()).as_str(),
                                    &state,
                                    &modules,
                                );
                                return InterpretResult::InterpretRuntimeError;
                            }
                            if let Value::PhoenixString(new_val) = value {
                                let mut new_string = String::new();
                                for (i, c) in s.chars().enumerate() {
                                    if i == index {
                                        new_string.push(match new_val.parse() {
                                            Ok(v) => v,
                                            Err(_) => {
                                                self.runtime_error(
                                                    "Attempted to set a string index to a non-string value",
                                                    &state,
                                                    &modules,
                                                );
                                                return InterpretResult::InterpretRuntimeError;
                                            }
                                        });
                                    } else {
                                        new_string.push(c);
                                    }
                                }
                                target = Value::PhoenixString(new_string);
                            } else {
                                self.runtime_error(
                                    "Attempted to set a string index to a non-string value",
                                    &state,
                                    &modules,
                                );
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                        Value::PhoenixPointer(list_index) => {
                            // get the list from the allocated lists
                            let v = state.deref_mut(list_index);
                            match v.obj {
                                HeapObjVal::PhoenixList(ref mut list) => {
                                    if list.values.len() <= index {
                                        self.runtime_error(
                                            format!("Attempted to index a list with an out-of-bounds index (index: {}, length: {})", index, list.values.len()).as_str(),
                                            &state,
                                            &modules,
                                        );
                                        return InterpretResult::InterpretRuntimeError;
                                    }
                                    list.values[index] = value;
                                }
                                _ => {
                                    self.runtime_error(
                                        "Attempted to index a non-indexable value",
                                        &state,
                                        &modules,
                                    );
                                    return InterpretResult::InterpretRuntimeError;
                                }
                            };
                        }
                        _ => {
                            self.runtime_error(
                                "Attempted to index a non-indexable value",
                                &state,
                                &modules,
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                    state.stack.push(target);
                }
                OpCreateList(size) => {
                    let mut list = Vec::new();
                    for _ in 0..size {
                        list.push(state.pop());
                    }
                    list.reverse();
                    // allocate the list
                    let list_obj = ObjList::new(list);
                    let ptr = state.alloc(HeapObj::new_list(list_obj));
                    state.stack.push(ptr);
                }
            }
        }
    }
}

fn debug_state_trace(state: &VMState, _vm: &VM, modules: &[ModuleChunk]) {
    eprintln!("> Frame: {:?}", state.current_frame);
    eprintln!("> Stack: ");
    eprint!(">>");
    for value in state.stack.iter() {
        eprint!(" [ {:?} ] ", value);
    }
    eprintln!();
    eprintln!("> Frames: ");
    eprint!(">>");
    for value in state.frames.iter() {
        eprint!(" [ {:?} ] ", value);
    }
    eprintln!();
    eprintln!("> Globals: ");
    for (index, val) in state.globals[state.current_frame.module].iter().enumerate() {
        if let Global::Init(global) = val {
            eprintln!(
                ">> {} => {:?}",
                VM::get_variable_name(index, state, modules),
                global
            );
        }
    }
    debug_instances(state);
}

fn debug_instances(state: &VMState) {
    eprintln!("> Instances: ");
    for (i, instance) in state.gc.instances.iter().enumerate() {
        eprintln!(">> [{}] {:?}", i, instance)
    }
}

fn debug_trace(vm: &VM, instr: &Instr, state: &VMState, modules: &[ModuleChunk]) {
    eprintln!("---");
    eprint!("> Next instr (#{}): ", state.current_frame.ip - 1);
    disassemble_instruction(
        instr,
        state.current_frame.ip - 1,
        &modules[state.current_frame.module].constants,
        &modules[state.current_frame.module].identifiers,
    );
    debug_state_trace(state, vm, modules);
    eprintln!("---\n");
}

fn debug_print_constants(_vm: &VM, modules: &[ModuleChunk]) {
    eprintln!("---");
    eprintln!("> Constants");
    for m in modules.iter() {
        eprintln!("--- [ module: {:?} ] ---", m.name);
        for val in m.constants.iter() {
            eprintln!("\t>> [ {:?} ]", val);
        }
    }
    eprintln!("---\n");
    // debug all m.identifiers
    eprintln!("---");
    eprintln!("> Identifiers");
    for m in modules.iter() {
        eprintln!("--- [ module: {:?} ] ---", m.name);
        for (i, val) in m.identifiers.iter().enumerate() {
            eprintln!("\t>> [ {} ] => {:?}", i, val);
        }
    }
    eprintln!("---\n");
}
