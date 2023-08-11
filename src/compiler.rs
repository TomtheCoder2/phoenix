use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::process::exit;

use serde::{Deserialize, Serialize};

use crate::chunk::CompilerModuleChunk;
use crate::chunk::OpCode::*;
use crate::chunk::{Chunk, ClassChunk, FunctionChunk, FunctionType, Instr, ModuleChunk, OpCode};
use crate::debug::{disassemble_class_chunk, disassemble_fn_chunk};
use crate::native::NATIVE_FUNCTIONS;
use crate::precedence::{get_rule, ParseFn, Precedence};

use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;
use crate::VERSION;

const DEFAULT_FILE_NAME: &str = "script";
const MAX_MODULES: usize = 255;
const MAX_JUMP: usize = 65535;

#[derive(Debug)]
pub struct Compiler {
    /// The modules that are imported (and the main module)
    modules: Vec<CompilerModuleChunk>,
    /// The name of the module and the index in the modules vector
    module_table: HashMap<String, usize>,
    /// Which ModuleChunkScanner should the the compiler return to after. Acts as a stack
    parent_modules: Vec<usize>,
    // we are always in a module, because the code in the main file is in a module (index 0)
    current_module: usize,
    // normally equal to the current_module, but this says from which module we are currently compiling
    current_module_code: usize,

    had_error: bool,
    panic_mode: bool,
    quiet_mode: bool,
    debug_mode: bool,
}

impl Compiler {
    fn current_chunk(&mut self) -> &mut Chunk {
        let index = self.current_module().current_function;
        &mut self
            .current_module()
            .functions
            .get_mut(index)
            .unwrap()
            .chunk
    }

    fn current_chunk_code(&mut self) -> &mut Chunk {
        let index = self.current_code_module().current_function;
        &mut self
            .current_code_module()
            .functions
            .get_mut(index)
            .unwrap()
            .chunk
    }

    fn current_chunk_ref(&self) -> &Chunk {
        &self
            .current_module_ref()
            .functions
            .get(self.current_module_ref().current_function)
            .unwrap()
            .chunk
    }

    fn current_fn(&mut self) -> &mut FunctionChunk {
        let index = self.current_module_ref().current_function;
        self.current_module().functions.get_mut(index).unwrap()
    }

    fn current_fn_type(&self) -> FunctionType {
        self.current_module_ref()
            .functions
            .get(self.current_module_ref().current_function)
            .unwrap()
            .fn_type
    }

    /// Panics if not in a class. Only call this if you're sure you're in a class def!
    fn current_class(&mut self) -> &mut ClassChunk {
        let index = self.current_module().current_class.unwrap();
        self.current_module().classes.get_mut(index).unwrap()
    }

    pub fn current_module(&mut self) -> &mut CompilerModuleChunk {
        self.modules.get_mut(self.current_module).unwrap()
    }

    pub fn current_code_module(&mut self) -> &mut CompilerModuleChunk {
        self.modules.get_mut(self.current_module_code).unwrap()
    }

    // uses the current_module_code
    pub fn set_current_module(&mut self, m: CompilerModuleChunk) {
        self.modules[self.current_module_code] = m;
    }

    fn current_module_ref(&self) -> &CompilerModuleChunk {
        self.modules.get(self.current_module).unwrap()
    }

    fn current_code_module_ref(&self) -> &CompilerModuleChunk {
        self.modules.get(self.current_module_code).unwrap()
    }

    fn advance(&mut self) {
        // heck, but idk another way, cause i cant have 2 mutable borrows
        let mut m = self.current_code_module().clone();
        let mut m2 = self.current_code_module().clone();
        m2.tokens.push(m.scanner.scan_token()); // Fixme: Wastes memory by not just dropping the older tokens, make advance() drop older tokens after i finish the code?
        self.set_current_module(m);
        self.current_code_module().tokens = m2.tokens;
        if self.current().token_type == TokenType::Error {
            self.error(self.current().lexeme.clone().as_str());
            self.advance();
        }
    }

    fn previous(&self) -> &Token {
        &self.current_code_module_ref().tokens[self.current_code_module_ref().tokens.len() - 2]
    }

    fn current(&self) -> &Token {
        &self.current_code_module_ref().tokens[self.current_code_module_ref().tokens.len() - 1]
    }

    fn consume(&mut self, token_type: TokenType, msg: &str) {
        self.advance();
        if !(self.previous().token_type == token_type) {
            self.error(msg);
        }
    }

    fn match_cur(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current().token_type == token_type
    }

    fn error(&mut self, message: &str) {
        if self.panic_mode {
            return;
        } // Ignore other errors while in panic_mode

        self.had_error = true;
        self.panic_mode = true;

        if self.quiet_mode {
            return;
        }

        let token = self.previous();
        eprint!(
            "[{}:{}] Error",
            self.current_module_ref().scanner.file.clone(),
            token.line_num + 1
        );
        match token.token_type {
            TokenType::EOF => eprint!(" at end of file"),
            TokenType::Error => (), // nothing
            _ => eprint!(" at '{}'", token.lexeme),
        }

        eprintln!(": {}", message);
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.check(TokenType::EOF) {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }
            match self.current().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }
            self.advance();
        }
    }

    fn emit_instr(&mut self, op_code: OpCode) {
        // println!("Emitting instr {:?} from token {:?}", op_code, self.previous()); kinda useful
        let instr = Instr {
            op_code,
            line_num: self.previous().line_num,
        };
        self.current_chunk_code().write_instruction(instr)
    }

    fn emit_instrs(&mut self, op_codes: &[OpCode]) {
        for oc in op_codes {
            self.emit_instr(*oc)
        }
    }

    fn emit_constant(&mut self, value: Value) -> usize {
        let index = self.add_constant(value);
        self.emit_instr(OpConstant(index));
        index
    }

    fn add_constant(&mut self, value: Value) -> usize {
        match self
            .current_module()
            .constants
            .iter()
            .position(|x| x == &value)
        {
            Some(i) => i,
            None => {
                self.current_module().constants.push(value);
                self.current_module().constants.len() - 1
            }
        }
    }

    fn emit_return(&mut self) {
        if self.current_fn_type() == FunctionType::Initializer {
            self.emit_instrs(&[OpGetLocal(0), OpReturn]);
        } else {
            self.emit_instrs(&[OpNil, OpReturn]);
        }
    }

    /// Emits OpCode::OpJump
    ///
    /// Returns the index of the jump instruction for patching
    fn emit_jump(&mut self) -> usize {
        self.emit_instr(OpJump(usize::MAX));
        self.current_chunk().code.len() - 1
    }

    /// Emits OpCode::OpJumpIfFalse
    ///
    /// Returns the index of the jump instruction for patching
    fn emit_jif(&mut self) -> usize {
        self.emit_instr(OpJumpIfFalse(usize::MAX));
        self.current_chunk().code.len() - 1
    }

    /// Given the index of the jump instruction in the chunk, update the opcode to jump to the instruction after the current one
    fn patch_jump(&mut self, index: usize) {
        let jump_amount = self.current_chunk().code.len() - index;
        if jump_amount > MAX_JUMP {
            self.error("Too much code to jump over");
        }

        let jump_instr = self.current_chunk().code.get_mut(index).unwrap();
        macro_rules! replace_jump {
            ($jump_type: path) => {{
                jump_instr.op_code = $jump_type(jump_amount)
            }};
        }

        match jump_instr.op_code {
            OpJump(_) => replace_jump!(OpCode::OpJump),
            OpJumpIfFalse(_) => replace_jump!(OpCode::OpJumpIfFalse),
            _ => panic!(
                "Compiler panic: Attempted to patch a non_jump op code instruction: {:?}",
                jump_instr
            ),
        }
    }

    /// loop_start: Index of the instruction to jump back to
    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().code.len() - loop_start;
        let loop_op = OpLoop(offset);

        if offset > (u16::MAX as usize) {
            self.error("Loop body too large");
        }

        self.emit_instr(loop_op);
    }

    /// Emits an OpReturn
    fn end_compilation(&mut self) {
        self.emit_return();
    }

    /// End scope by emitting pop instructions and cleaning the resolver
    fn end_scope(&mut self) {
        for _ in 0..self.current_module().resolver.end_scope() {
            self.emit_instr(OpPop); // Remove old local variables
        }
    }

    /// Calls Resolver::declare_variable() with the previous Token's lexeme (TokenIdentifier)
    fn declare_variable(&mut self) {
        let str_val = self.previous().lexeme.clone();
        let success = self.current_module().resolver.declare_variable(str_val);
        if !success {
            self.error("Variable with this name already declared in this scope");
        }
    }

    fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();

        // Parse the start of the prefix expression
        // We know this must be a prefix because we can't start with something that is an infix (eg + 3 2)
        let prefix_rule = (get_rule(self.previous().token_type)).prefix;
        // Used only by variable() to determine if a TokenIdentifier is for an assignment or get
        let can_assign = prec <= Precedence::Assignment;
        self.call_parse_fn(prefix_rule, can_assign);

        // Parse any number of infix expressions, as long as they have higher precedence
        while prec <= get_rule(self.current().token_type).precedence {
            self.advance();
            let infix_rule = (get_rule(self.previous().token_type)).infix;
            self.call_parse_fn(infix_rule, can_assign);
        }

        // Show compilation error for a TokenEqual found in an infix position
        if can_assign && self.previous().token_type == TokenType::Equal {
            self.error("Invalid assignment target");
        }
    }

    fn call_parse_fn(&mut self, parse_fn: ParseFn, can_assign: bool) {
        match parse_fn {
            ParseFn::None => self.error("Expected expression"),
            ParseFn::Binary => self.binary(),
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Number => self.number(),
            ParseFn::Literal => self.literal(),
            ParseFn::String => self.string(),
            ParseFn::List => self.list(can_assign),
            ParseFn::Variable => self.variable(can_assign),
            ParseFn::And => self.and_operator(),
            ParseFn::Or => self.or_operator(),
            ParseFn::Call => self.call(),
            ParseFn::Dot => self.dot(can_assign),
            ParseFn::This => self.this(),
            ParseFn::Super => self.super_(),
            ParseFn::Increment => { /* gets handled by named_variable() */ }
        }
    }

    fn declaration(&mut self) {
        if self.match_cur(TokenType::Fun) {
            self.fun_declaration();
        } else if self.match_cur(TokenType::Class) {
            self.class_declaration();
        } else if self.match_cur(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode {
            self.synchronize();
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expected function name");
        self.current_module().resolver.mark_initialized(); // Initialize the function object if we are in a local scope
        self.function(FunctionType::Function);
        self.define_variable(global); // Emit the define instr if we are in the global scope
    }

    fn class_declaration(&mut self) {
        self.consume(
            TokenType::Identifier,
            "Expected class name after keyword 'class'",
        );
        let name = self.previous().lexeme.clone();
        let name_index = self.identifier_constant(&name);
        self.declare_variable();

        let class = ClassChunk::new(name);
        let old_class = self.current_module().current_class;
        self.current_module().classes.push(class);

        let class_index = self.current_module().classes.len() - 1;
        self.current_module().current_class = Some(class_index);

        self.emit_instr(OpClass(class_index));
        self.define_variable(name_index);

        // Check for superclass
        if self.match_cur(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expected superclass name");
            // Resolve the superclass methods entirely at compile time instead of runtime because it fits how everything else works
            // However because the compiler is single pass, you can only inherit a class that has already been defined
            // Note: we know that all the methods the superclass will ever own must already be defined, since it will have had the same superclass resolution at compile time < Phoenix classes are closed
            // Note: I like this bit of code, it is a really nice shiny implementation of superclasses that doesnt require any new opcodes and does not require any copying of the FunctionChunks. Fucking sick
            let superclass_name = &self.previous().lexeme.clone();
            let mut superclass_index: Option<usize> = None;
            for (i, class_def) in self.current_module().classes.iter().enumerate() {
                if class_def.name.eq(superclass_name) {
                    superclass_index = Some(i);
                }
            }

            if superclass_index == self.current_module().current_class {
                self.error("A class cannot inherit from itself");
            }

            match superclass_index {
                Some(i) => {
                    let superclass = &self.current_module().classes[i];
                    for (name_index, fn_index) in superclass.methods.clone().iter() {
                        self.current_class().methods.insert(*name_index, *fn_index);
                        // Inherit all the methods by just copying in all the fn_indices, nicely handles multiple levels of inheritance
                        let name = self.current_module().identifier_constants[*name_index].clone();
                        if name.as_str().eq("init") {
                            self.current_class().has_init = true;
                        }
                    }
                    self.current_class().superclass = superclass_index;
                }
                None => {
                    self.error(format!("'{}' is not a valid superclass", superclass_name).as_str())
                }
            }
        }

        self.consume(TokenType::LeftBrace, "Expected '{' before class body");
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.method();
        }
        self.consume(TokenType::RightBrace, "Expected '}' after class body");

        self.current_module().current_class = old_class;
    }

    // Note: Since this constantly confuses me, I'm gonna keep a note here so that I don't forget how variables work in Phoenix
    // Globals: The opcodes GetGlobal and SetGlobal take a PhoenixString from the constants vec and map it into a HashMap in the VM, no resolving/checking is done before runtime
    // Locals: Local variables live on the stack and since they are the ONLY values that do not get popped after statements, we know that they must live at the very bottom of the stack,
    // and thus we can just raw index from the bottom of the stack to the index of the variable by looking at how many locals have been defined in this scope
    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name");
        if self.match_cur(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_instr(OpNil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration",
        );
        self.define_variable(global);
    }

    /// Match the identifier token and pass it into identifier_constant to be added to the chunk if current scope is global
    ///
    /// Calls declare_variable() if the current scope is local
    fn parse_variable(&mut self, error_msg: &str) -> usize {
        self.consume(TokenType::Identifier, error_msg);
        // check if the variable has the same name as a native function
        if NATIVE_FUNCTIONS
            .lock()
            .unwrap()
            .contains_key(&*self.previous().lexeme)
        {
            self.error("Cannot redefine a native function");
        }
        self.declare_variable();

        if self.current_module().resolver.is_global() {
            let str_val = self.previous().lexeme.clone();
            self.identifier_constant(&str_val)
        } else {
            0
        }
    }

    /// Add a string to the chunk as a constant and return the index
    ///
    /// Only used for global variables
    fn identifier_constant(&mut self, str_val: &String) -> usize {
        // self.add_constant(Value::PhoenixString(str_val.to_string()))
        match self
            .current_module()
            .identifier_constants
            .iter()
            .position(|x| x == str_val)
        {
            Some(i) => i,
            None => {
                self.current_module()
                    .identifier_constants
                    .push(str_val.to_string());
                self.current_module().identifier_constants.len() - 1
            }
        }
    }

    /// Add a string to the chunk as a constant and return the index
    ///
    /// Only used for global variables
    pub fn identifier_constant_module(&mut self, str_val: &String, module: usize) -> usize {
        // self.add_constant(Value::PhoenixString(str_val.to_string()))
        match self
            .modules
            .get_mut(module)
            .unwrap()
            .identifier_constants
            .iter()
            .position(|x| x == str_val)
        {
            Some(i) => i,
            None => {
                self.modules
                    .get_mut(module)
                    .unwrap()
                    .identifier_constants
                    .push(str_val.to_string());
                self.modules
                    .get_mut(module)
                    .unwrap()
                    .identifier_constants
                    .len()
                    - 1
            }
        }
    }

    /// Emits the instruction to define the global variable
    /// or to set the local variable as initialized
    fn define_variable(&mut self, global: usize) {
        if self.current_module().resolver.is_global() {
            self.emit_instr(OpDefineGlobal(global));
        } else {
            self.current_module().resolver.mark_initialized();
        }
    }

    fn statement(&mut self) {
        if self.match_cur(TokenType::Print) {
            self.print_statement();
        } else if self.match_cur(TokenType::Import) {
            self.import_statement();
        } else if self.match_cur(TokenType::Return) {
            self.return_statement();
        } else if self.match_cur(TokenType::If) {
            self.if_statement();
        } else if self.match_cur(TokenType::While) {
            self.while_statement();
        } else if self.match_cur(TokenType::For) {
            self.for_statement();
        } else if self.match_cur(TokenType::LeftBrace) {
            self.current_module().resolver.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after value in print statement",
        );
        self.emit_instr(OpPrint);
    }

    fn import_statement(&mut self) {
        // first we need to check if we are in a script function and else we error, because we can't import in a non-script function (for now)
        if self.current_fn_type() != FunctionType::Script {
            self.error("Can't import in a function");
        }
        // so we have something like "import foo" or "import bar from foo"
        // for now we only support the first one
        self.consume(TokenType::Identifier, "Expected identifier after import");
        let name = self.previous().lexeme.clone();
        self.consume(TokenType::Semicolon, "Expected ';' after import statement");
        // now we have the name of the module we want to import

        // first we try to open the file by doing current_dir + name + .phx
        let mut path = env::current_dir().unwrap();
        path.push(name.clone());
        path.set_extension("phx");
        // debug the path
        if self.debug_mode {
            println!("Importing module: {}", path.clone().to_str().unwrap());
        }
        let mut file = match File::open(path.clone()) {
            Ok(f) => f,
            Err(e) => {
                self.error(format!("Could not open file: {}", e).as_str());
                return;
            }
        };
        // now we have the contents of the file in contents
        let mut contents = String::new();
        match file.read_to_string(&mut contents) {
            Ok(_) => {}
            Err(e) => {
                self.error(format!("Could not read file: {}", e).as_str());
                return;
            }
        }
        let module = CompilerModuleChunk::new(
            false,
            name.clone(),
            path.to_str().unwrap().to_string(),
            contents,
        );
        if self.modules.len() > MAX_MODULES {
            println!(
                "[{}:{}] Can't load more than {} modules (maybe recursive imports?)",
                self.current_module().scanner.file.clone(),
                self.current_module().scanner.cur_line,
                MAX_MODULES
            );
            exit(0);
        }
        self.modules.push(module);
        self.parent_modules.push(self.current_module);
        self.current_module = self.modules.len() - 1;
        self.current_module_code = self.current_module;
        self.module_table
            .insert(name.clone(), self.modules.len() - 1);

        // self.current_module().scanner.cur_line = 0;
        let first_token = self.current_module().scanner.scan_token();
        self.current_module().tokens.push(first_token.clone()); // Load up the first token

        self.current_module()
            .functions
            .push(FunctionChunk::new(None, 0, FunctionType::Script)); // Start the compilation with a top level function

        // Hack to account for the case where the first token is a TokenError
        if let TokenType::Error = first_token.token_type {
            self.advance();
            self.error(first_token.lexeme.as_str());
        }
        while !self.match_cur(TokenType::EOF) {
            self.declaration();
        }
        // idk if we need this tbh
        self.end_compilation();

        // now we need to add the module to the module table
        let index_of_module = self.current_module;
        self.current_module = self.parent_modules.pop().unwrap();
        self.current_module_code = self.current_module;

        // we create a global variable with the name of the module
        self.emit_constant(Value::PhoenixModule(index_of_module));
        let index = self.identifier_constant(&name);
        self.define_variable(index);
        self.emit_instr(OpImport(index_of_module));
        // self.emit_instr(OpCode::OpPop);
    }

    fn return_statement(&mut self) {
        if self.current_fn_type() == FunctionType::Script {
            self.error("Cannot return from top-level code");
        }

        if self.match_cur(TokenType::Semicolon) {
            // Nil return
            self.emit_return();
        } else {
            if self.current_fn_type() == FunctionType::Initializer {
                self.error("Cannot return a value from an initializer");
            }

            self.expression();
            self.consume(TokenType::Semicolon, "Expected ';' after return value");
            self.emit_instr(OpReturn);
        }
    }

    fn if_statement(&mut self) {
        // optional ( ) around the condition
        // self.consume(TokenType::LeftParen, "Expected '(' after 'if'");
        if self.match_cur(TokenType::LeftParen) {
            self.advance();
            self.expression();
            self.consume(TokenType::RightParen, "Expected ')' after condition");
        } else {
            self.expression();
        }

        // Keep track of where we put the first conditional jump
        let jump_index = self.emit_jif();

        self.emit_instr(OpPop); // Pop off the if conditional in the 'then' case
        self.statement(); // Then case

        if self.match_cur(TokenType::Else) {
            let else_jump = self.emit_jump(); // Keep track of where we put the jump to go over the else statement
            self.patch_jump(jump_index);
            self.emit_instr(OpPop); // Pop off the if conditional if we jump over the 'then' case
            self.statement(); // Else case
            self.patch_jump(else_jump);
        } else {
            self.patch_jump(jump_index); // No else case, so just jump to right after
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();

        // self.consume(TokenType::LeftParen, "Expected '(' after 'while'");
        self.expression();
        // self.consume(TokenType::RightParen, "Expected ')' after loop condition");

        let exit_jump = self.emit_jif();

        self.emit_instr(OpPop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_instr(OpPop);
    }

    fn for_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expected '(' after 'for'");

        self.current_module().resolver.begin_scope();

        // First clause: Can be var declaration or expression
        if self.match_cur(TokenType::Semicolon) {
            // Do nothing
        } else if self.match_cur(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement(); //
        }

        let mut loop_start = self.current_chunk().code.len(); // Loop should include 2nd and 3rd clauses (if they exist)
        let mut exit_jump = None;

        // Loop conditional
        if !self.match_cur(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expected ';' after loop condition");
            exit_jump = Some(self.emit_jif());
            self.emit_instr(OpPop); // Pop condition if we didn't jump
        } // Note: if this conditional is not found, then there is no way to jump out of the loop

        if !self.match_cur(TokenType::RightParen) {
            // Jump to body, set this point to be the one to loop back to after executing the body, jump to next iteration
            let body_jump = self.emit_jump(); // Jump to after the increment and the loop

            let increment_start = self.current_chunk().code.len();
            self.expression(); // Parse the increment expression
            self.emit_instr(OpPop); // Pop the remaining value
            self.consume(TokenType::RightParen, "Expected ')' after for loop clauses");

            self.emit_loop(loop_start); // Loop back to the start after increment
            loop_start = increment_start; // Make the body go to the start of the increment instead of the start of the loop

            self.patch_jump(body_jump); // Patching up the body jump
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(offset) = exit_jump {
            self.patch_jump(offset);
            self.emit_instr(OpPop);
        }

        self.end_scope();
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expected '}' after block"); // Fails if we hit EOF instead
    }

    /// Parses a 'this' keyword by just treating it as a special class-only variable that will be magically instantiated
    /// Our resolver will automatically put the 'this' variable in locals slot 0 for any methods, so this (ha) will always result in a Get/Set Local op being emitted
    fn this(&mut self) {
        if self.current_module().current_class.is_none() {
            self.error("Cannot use keyword 'this' outside of a class");
        }
        self.variable(false);
    }

    /// Consumes super.method_name and emits an OpGetSuper(index of the "method_name" identifier)
    fn super_(&mut self) {
        if self.current_module().current_class.is_none() {
            self.error("Cannot use keyword 'super' outside of a class");
            return; // Ideally we would attempt to compile the rest of the expression, but trying to continue will cause a panic
        }

        let superclass_index = if self.current_class().superclass.is_none() {
            self.error("Cannot use keyword 'super' in a class which does not inherit a class");
            0 // Random value, we don't care that this value is wrong because we're going to exit because of the error anyway
        } else {
            self.current_class().superclass.unwrap()
        };

        self.consume(TokenType::Dot, "Expected '.' after 'super'");
        self.consume(TokenType::Identifier, "Expected superclass method name");
        let name = self.previous().lexeme.clone();
        let name_index = self.identifier_constant(&name);

        // At the time of OpGetSuper we want to know 2 things
        // 1. The superclass we're going to be looking for values in
        // 2. A pointer to the instance we want to bind the method to

        let superclass_val = Value::PhoenixClass(superclass_index);
        self.emit_constant(superclass_val);
        self.named_variable(&String::from("this"), false); // Slightly better?
        self.emit_instr(OpGetSuper(name_index));
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expected method name");
        let name = self.previous().lexeme.clone();
        let name_index = self.identifier_constant(&name);

        let index = if name.eq("init") {
            self.current_class().has_init = true;
            self.function(FunctionType::Initializer)
        } else {
            self.function(FunctionType::Method)
        };
        self.current_class().methods.insert(name_index, index); // Note: This provides method overriding since we do not check if the name already existed in the map

        // NOTE!! this way of doing methods does NOT bind closures... So there is a very very stupid way this could go wrong
        // Something like fun thing() { class Inner { method() { // use a local variable from thing in here }}}
        // ...
        // ...
        // Fuck it
        // This is a feature
        // Not a bug
        // I swear
    }

    /// Compiles the function into a new FunctionChunk, adds it to the current parser, adds the PhoenixFunction object to the constants stack, emits a OpConstant pointing to it and a OpClosure to wrap it
    fn function(&mut self, fun_type: FunctionType) -> usize {
        //let mut function_parser = self.from_old(fun_type);

        let index = self.start_child(fun_type);
        self.current_module().resolver.begin_scope();

        self.consume(TokenType::LeftParen, "Expected '(' after function name");
        if !self.check(TokenType::RightParen) {
            loop {
                let param_constant = self.parse_variable("Expected parameter name");
                self.define_variable(param_constant);

                let cur_function = self.current_fn();
                cur_function.arity += 1;
                if cur_function.arity > 255 {
                    self.error("Cannot have more than 255 parameters");
                }

                if !self.match_cur(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            "Expected ')' after function parameters",
        );

        self.consume(TokenType::LeftBrace, "Expected '{' before function body");
        self.block();

        let upvalues = self.current_module().resolver.pop();
        let has_upvalues = !upvalues.is_empty();
        if !upvalues.is_empty() {
            self.current_fn().set_upvalues(upvalues); // Gotta set this before end_child() switches what the current_fn is
        }

        self.end_child();

        if fun_type != FunctionType::Method && fun_type != FunctionType::Initializer {
            // We don't need this for methods because they are statically loaded into the ClassChunk, not at runtime on the stack
            self.emit_constant(Value::PhoenixFunction(index));

            if has_upvalues {
                self.emit_instr(OpClosure);
            }
        }

        index
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after value");
        self.emit_instr(OpPop);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    fn and_operator(&mut self) {
        let end_jump = self.emit_jif();

        self.emit_instr(OpPop);
        self.parse_precedence(Precedence::And); // Parse right hand side of the infix expression
        self.patch_jump(end_jump); // Jump to after it if the first argument was already false, leaving the false value on the top of the stack to be the result
                                   // Otherwise the first argument is true, so the value of the whole and is equal to the value of the second argument, so just proceed as normal
    }

    fn or_operator(&mut self) {
        // If false then execute the other expression
        let else_jump = self.emit_jif();

        // If the first one is already truthy, go to the end
        let end_jump = self.emit_jump();

        self.patch_jump(else_jump);
        self.emit_instr(OpPop);
        self.parse_precedence(Precedence::Or);

        self.patch_jump(end_jump);
    }

    fn number(&mut self) {
        // We trust that the scanner has given us something that looks like a number (124214.52)
        // BUT the scanner does NOT check the size, so this parse to f32 can still fail due to overflow

        let s = &self.previous().lexeme;

        // we want to cast all numbers first to i64, and if that fails to f32, because normally we want integers
        if let Ok(value) = s.parse::<i64>() {
            self.emit_constant(Value::Long(value));
        } else {
            // we try it as a long, maybe it's an integer
            if let Ok(value) = if s.ends_with('f') {
                &s[0..s.len() - 1]
            } else {
                s
            }
            .parse::<f32>()
            {
                self.emit_constant(Value::Float(value));
            } else {
                self.error(format!("Invalid number: {s}").as_str());
            }
        }
    }

    fn literal(&mut self) {
        match self.previous().token_type {
            TokenType::False => self.emit_instr(OpFalse),
            TokenType::True => self.emit_instr(OpTrue),
            TokenType::Nil => self.emit_instr(OpNil),
            _ => unreachable!(
                "Unreachable state reached, attempted to make a literal out of a non-literal type???"
            ),
        }
    }

    fn string(&mut self) {
        let str_val = self.previous().lexeme.clone();
        let cleaned = str_val[1..str_val.len() - 1].to_string();

        self.emit_constant(Value::PhoenixString(cleaned));
    }

    fn list(&mut self, _can_assign: bool) {
        let mut size = 0;
        if !self.check(TokenType::RightBracket) {
            loop {
                self.expression();
                size += 1;
                if !self.match_cur(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightBracket, "Expected ']' after list");
        self.emit_instr(OpCreateList(size));
    }

    /// Parse an identifier that we know to be a variable
    ///
    /// Eventually emits a get instr or a set instr + the instructions to process the expr
    ///
    /// Note: Uses named_variable to do all the heavy lifting
    fn variable(&mut self, can_assign: bool) {
        let name = &self.previous().lexeme.clone();
        self.named_variable(name, can_assign)
    }

    // Note: parse_precedence with TokenIdentifier => variable() -> named_variable(previous.lexeme)
    // Could be a getter or a setter, so lookahead for a '='
    /// Helper function for variable.
    /// 1. Determine if this is a local var, upvalue, or global and make the get and set ops
    /// 2. Determine if this is a get or a set based on can_assign and the existence of a '='
    fn named_variable(&mut self, name: &String, can_assign: bool) {
        let local_arg = match self.current_module().resolver.resolve_local(name) {
            Some(opt) => opt,
            None => {
                self.error("Cannot read local variable in its own initializer");
                return;
            }
        };

        // Figure out which type of get/set OpCodes we want
        let (get_op, set_op) = if let Some(local_index) = local_arg {
            (OpGetLocal(local_index), OpSetLocal(local_index))
        } else if let Some(upvalue_index) = self.current_module().resolver.resolve_upvalue(name) {
            (OpGetUpvalue(upvalue_index), OpSetUpvalue(upvalue_index))
        } else {
            let global_arg = self.identifier_constant(name); // Does NOT check at compile time if this variable can be resolved

            if self.match_cur(TokenType::LeftParen) {
                let arg_count = self.argument_list();
                (
                    OpCallGlobal(self.current_module, global_arg, arg_count),
                    OpSetGlobal(global_arg),
                )
            } else {
                (OpGetGlobal(global_arg), OpSetGlobal(global_arg))
            }
        };

        // hacky way to implement all kinds of operations
        // todo: make this better
        // todo: check if this works correctly with classes and all that stuff
        // Figure out if we want to use the get or the set from the pair of possible ops we determined earlier
        macro_rules! emit_assign {
            ($operation: path) => {{
                self.emit_instr(get_op);
                self.advance();
                self.expression();
                self.emit_instr($operation);
                self.emit_instr(set_op);
            }};
        }
        if can_assign {
            match self.current().token_type {
                TokenType::Equal => {
                    self.advance();
                    self.expression();
                    self.emit_instr(set_op);
                }
                TokenType::PlusAssign => emit_assign!(OpAdd),
                TokenType::MinusAssign => emit_assign!(OpSubtract),
                TokenType::StarAssign => emit_assign!(OpMultiply),
                TokenType::SlashAssign => emit_assign!(OpDivide),
                TokenType::PlusPlus => {
                    self.emit_instr(get_op);
                    self.emit_constant(Value::Float(1.0));
                    self.emit_instr(OpAdd);
                    self.emit_instr(set_op);
                }
                TokenType::MinusMinus => {
                    self.emit_instr(get_op);
                    self.emit_constant(Value::Float(1.0));
                    self.emit_instr(OpSubtract);
                    self.emit_instr(set_op);
                }
                TokenType::LeftBracket => {
                    // check if this is a list
                    self.advance();
                    self.emit_instr(get_op);
                    self.expression();
                    self.consume(TokenType::RightBracket, "Expected ']' after list index");
                    if self.match_cur(TokenType::Equal) {
                        self.expression();
                        self.emit_instr(OpSetIndex);
                        // self.emit_instr(set_op);
                    } else {
                        self.emit_instr(OpGetIndex);
                    }
                }
                _ => self.emit_instr(get_op),
            }
        } else {
            self.emit_instr(get_op);
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expected ')' after expression");
    }

    fn unary(&mut self) {
        let operator_type = self.previous().token_type;
        self.parse_precedence(Precedence::Unary); // evaluate the expression in the unary
        match operator_type {
            TokenType::Minus => self.emit_instr(OpNegate),
            TokenType::Bang => self.emit_instr(OpNot),
            _ => (), // Error?
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous().token_type;

        let rule = get_rule(operator_type);
        self.parse_precedence(rule.next_precedence());

        // Stack based vm, so emit the binary instr after
        match operator_type {
            TokenType::Plus => self.emit_instr(OpAdd),
            TokenType::Minus => self.emit_instr(OpSubtract),
            TokenType::Star => self.emit_instr(OpMultiply),
            TokenType::Slash => self.emit_instr(OpDivide),
            TokenType::BangEqual => self.emit_instrs(&[OpEqual, OpNot]),
            TokenType::EqualEqual => self.emit_instr(OpEqual),
            TokenType::Greater => self.emit_instr(OpGreater),
            TokenType::GreaterEqual => self.emit_instrs(&[OpLess, OpNot]),
            TokenType::Less => self.emit_instr(OpLess),
            TokenType::LessEqual => self.emit_instrs(&[OpGreater, OpNot]),
            _ => {
                self.error("Invalid binary operator");
            } // error?
        }
    }

    /// Infix operation for function calls, assumes that the PhoenixFunction will be at the top of the stack when this is called, usually
    /// via a global/local variable resolution
    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_instr(OpCall(arg_count, self.current_module))
    }

    /// Parses expressions while looking for commas between and for the closing paren. Leaves the values on the stack
    fn argument_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error("Cannot have more than 255 arguments");
                }
                arg_count += 1;

                if !self.match_cur(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            "Expected ')' after function argument list",
        );
        arg_count
    }

    fn dot(&mut self, can_assign: bool) {
        let module_name = &self.current_module_ref().tokens
            [self.current_module_ref().tokens.len() - 3]
            .lexeme
            .clone();
        // check if this is a module name
        if self.module_table.contains_key(module_name) {
            // pop the name of the module off the stack
            self.emit_instr(OpPop);
            // get the index of the module
            let module_index = *self.module_table.get(module_name).unwrap();
            // let function_index = self.identifier_constant_module(function_name, module_index);
            // self.emit_instr(OpGetModuleVar(module_index, function_index));
            // self.emit_instr(OpCall(arg_count, module_index));
            let current_module = self.current_module;
            self.current_module = module_index;
            // dbg!(self.current());
            self.expression();
            self.current_module = current_module;
            return;
        }
        self.consume(TokenType::Identifier, "Expected property name after '.'");
        let name_index = self.identifier_constant(&self.previous().lexeme.clone());
        // let function_name = &self.current_module_ref().tokens
        //     [self.current_module_ref().tokens.len() - 2]
        //     .lexeme
        //     .clone();

        if can_assign && self.match_cur(TokenType::Equal) {
            // We check can_assign so that a + b.c = 3 does not invalidly emit a set op
            // Setter
            self.expression();
            self.emit_instr(OpSetProperty(name_index));
        } else if self.match_cur(TokenType::LeftParen) {
            // A left paren after the initializer will usually mean a method invocation, so compress that into a single OpCode here
            // but it could also be a call to a function in a module with that name, so we need to check for that
            // println!("{}.{} {}", module_name, function_name, name_index);
            let arg_count = self.argument_list();
            self.emit_instr(OpInvoke(name_index, arg_count, self.current_module));
        } else if self.match_cur(TokenType::LeftBracket) {
            self.emit_instr(OpGetProperty(name_index));
            self.expression();
            self.consume(TokenType::RightBracket, "Expected ']' after index");
            if self.match_cur(TokenType::Equal) {
                self.expression();
                self.emit_instr(OpSetIndex);
                // self.emit_instr(set_op);
            } else {
                self.emit_instr(OpGetIndex);
            }
        } else {
            self.emit_instr(OpGetProperty(name_index));
        }
    }

    /// Sets the compiler to generate a new function chunk for the next segment of code
    fn start_child(&mut self, function_type: FunctionType) -> usize {
        let function_name = self.previous().lexeme.clone();
        self.current_module().functions.push(FunctionChunk::new(
            Some(function_name),
            0,
            function_type,
        ));
        self.current_module().resolver.push(function_type);
        let index = self.current_module_ref().current_function;
        self.current_module().parent_functions.push(index);
        self.current_module().current_function = self.current_module().functions.len() - 1;

        self.current_module().functions.len() - 1
    }

    /// Switches the current chunk out of the new function def
    fn end_child(&mut self) {
        // Emit an implicit nil return if not specified explicit
        let last_instr = self.current_chunk_ref().code.last();
        if last_instr.is_none() || last_instr.unwrap().op_code != OpReturn {
            self.emit_return();
        }
        self.current_module().current_function =
            self.current_module().parent_functions.pop().unwrap();
    }

    #[deprecated(note = "Use new instead")]
    pub fn new_default(code: String, quiet: bool, start_line: usize) -> Compiler {
        Compiler::new_file(DEFAULT_FILE_NAME.to_string(), code, quiet, start_line)
    }

    pub fn new_file(file: String, code: String, quiet: bool, start_line: usize) -> Compiler {
        let mut compiler = Compiler {
            modules: vec![CompilerModuleChunk::new(
                true,
                "main".to_string(),
                file.to_string(),
                code.clone(),
            )],
            module_table: Default::default(),
            parent_modules: vec![],
            current_module: 0,
            current_module_code: 0,

            had_error: false,
            panic_mode: false,
            quiet_mode: quiet,
            debug_mode: false,
        };
        compiler.new_start(file, code, quiet, start_line, 0);
        compiler
    }

    /// whether it had an  error
    pub fn new_start(
        &mut self,
        file: String,
        code: String,
        quiet: bool,
        start_line: usize,
        start_pos: usize,
    ) -> bool {
        self.current_module().scanner = Scanner::new(file, code, start_line);
        self.current_module().scanner.cur_pos = start_pos;
        self.current_module().scanner.start_pos = start_pos;
        self.quiet_mode = quiet;

        let first_token = self.current_module().scanner.scan_token();
        self.current_module().tokens.push(first_token.clone()); // Load up the first token

        self.current_module()
            .functions
            .push(FunctionChunk::new(None, 0, FunctionType::Script)); // Start the compilation with a top level function

        // Hack to account for the case where the first token is a TokenError
        if let TokenType::Error = first_token.token_type {
            self.advance();
            self.error(first_token.lexeme.as_str());
            return true;
        }
        false
    }

    pub fn compile(&mut self, debug: bool) -> Option<CompilationResult> {
        self.debug_mode = debug;
        while !self.match_cur(TokenType::EOF) {
            self.declaration();
        }
        self.end_compilation();

        if !self.had_error {
            if debug {
                for m in self.modules.iter() {
                    println!("========= module: {} =========", m.scanner.file);
                    for (index, fn_chunk) in m.functions.iter().enumerate() {
                        if fn_chunk.fn_type != FunctionType::Method
                            && fn_chunk.fn_type != FunctionType::Initializer
                        {
                            disassemble_fn_chunk(
                                m.scanner.file.as_str().to_string(),
                                index,
                                fn_chunk,
                                &m.constants,
                                &m.identifier_constants,
                            );
                        }
                    }

                    for class_chunk in m.classes.iter() {
                        disassemble_class_chunk(
                            class_chunk,
                            &m.functions,
                            &m.classes,
                            &m.constants,
                            &m.identifier_constants,
                        );
                    }
                }
            }
            Some(CompilationResult {
                version: VERSION.to_string(),
                modules: self
                    .modules
                    .clone()
                    .iter()
                    .map(|m| ModuleChunk::from(m.clone()))
                    .collect(),
                modules_table: self.module_table.clone(),
                cur_pos: self.current_module().scanner.cur_pos,
            })
        } else {
            None
        }
    }

    pub fn compile_code(code: String, debug: bool) -> Option<CompilationResult> {
        let mut compiler = Compiler::new_file(DEFAULT_FILE_NAME.to_string(), code, false, 0);
        compiler.compile(debug)
    }
}

impl Clone for Compiler {
    fn clone(&self) -> Self {
        Compiler {
            modules: self.modules.clone(),
            module_table: self.module_table.clone(),
            parent_modules: self.parent_modules.clone(),
            current_module: self.current_module,
            current_module_code: 0,

            had_error: self.had_error,
            panic_mode: self.panic_mode,
            quiet_mode: self.quiet_mode,
            debug_mode: false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CompilationResult {
    pub version: String,
    pub modules: Vec<ModuleChunk>,
    pub modules_table: HashMap<String, usize>,
    pub cur_pos: usize,
}

impl Default for CompilationResult {
    fn default() -> Self {
        CompilationResult::new()
    }
}

impl CompilationResult {
    pub fn new() -> CompilationResult {
        CompilationResult {
            version: VERSION.to_string(),
            modules: vec![],
            modules_table: Default::default(),
            cur_pos: 0,
        }
    }

    // pub fn append(&mut self, mut other: CompilationResult) {
    //     self.classes.append(&mut other.classes);
    //     self.functions.append(&mut other.functions);
    //     // filter out all functions that f.fn_type == FunctionType::Script
    //     self.functions.retain(|f| f.fn_type != FunctionType::Script);
    //     // remove all duplicates (fu.name == f.name)
    //     let mut used_names = Vec::new();
    //     self.functions.retain(|f| {
    //         return if used_names.contains(&f.clone().name.unwrap()) {
    //             false
    //         } else {
    //             used_names.push(f.clone().name.unwrap());
    //             true
    //         };
    //     });
    //     // debug the functions
    //     for f in self.functions.clone() {
    //         println!("got function {}", f.name.unwrap());
    //     }
    //     self.constants.append(&mut other.constants);
    //     self.identifier_constants.append(&mut other.identifier_constants);
    // }
}
