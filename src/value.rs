use std::collections::HashMap;

use crate::chunk::ModuleChunk;
use serde::{Deserialize, Serialize};

use crate::native::native_functions::NativeFn;
use crate::native::native_methods::NativeMethod;
use crate::vm::{VMState, VM};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub enum Value {
    Float(f32),
    Long(i64),
    Bool(bool),
    #[default]
    Nil,
    // todo: fix this, cause this type is only used for initialisation of strings
    // todo: create static strings, for example for prints, so we can allocate them once and have multiple references to them
    PhoenixString(String),
    // Index of the function in the functions Vec in VM // Fixme: Is this even reachable? Can this be completely removed and the parameter put in OpClosure?
    PhoenixFunction(usize),
    #[serde(skip)]
    NativeFunction(Option<usize>, NativeFn),
    #[serde(skip)]
    NativeMethod(Option<usize>, NativeMethod),
    PhoenixClass(usize),
    // similar to class, but for modules
    PhoenixModule(usize),
    // for strings, instances and lists
    PhoenixPointer(usize),
    PhoenixBoundMethod(ObjBoundMethod),
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        values_equal((self, other))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ValueArray {
    pub values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> ValueArray {
        ValueArray { values: Vec::new() }
    }

    pub fn write(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn free(&mut self) {
        self.values.clear();
    }
}

impl Value {
    /// Used for print statements, use {:?} debug formatting for trace and stack examining
    pub fn to_string(&self, vm: &VM, state: &VMState, modules: &Vec<ModuleChunk>) -> String {
        match self {
            Value::Float(x) => format!("{}", x),
            Value::Long(x) => format!("{}", x),
            Value::Bool(x) => format!("{}", x),
            Value::PhoenixString(x) => x.to_string(),
            Value::Nil => String::from("nil"),
            Value::PhoenixFunction(x) => format!(
                "<fn {}>",
                &modules[state.current_frame.module]
                    .functions
                    .get(*x)
                    .unwrap()
                    .name
                    .as_ref()
                    .unwrap()
            ),
            Value::NativeFunction(_x, _) => "<native_fn>".to_string(),
            Value::NativeMethod(_x, _) => "<native_method>".to_string(),
            Value::PhoenixClass(class) => format!("<class {}>", class),
            Value::PhoenixPointer(pointer) => {
                // hacky way to check if this is a list or string
                if let HeapObjVal::PhoenixList(_) = &state.deref(*pointer).obj {
                    state.deref(*pointer).to_string(vm, state, modules)
                } else if let HeapObjVal::PhoenixString(_) = &state.deref(*pointer).obj {
                    state.deref(*pointer).to_string(vm, state, modules)
                } else {
                    format!(
                        "<pointer {}> to {}",
                        pointer,
                        state.deref(*pointer).to_string(vm, state, modules)
                    )
                }
            } // Suggestion: Don't reveal to the user the internals?
            Value::PhoenixBoundMethod(method) => format!(
                "<method {} from {}",
                &modules[state.current_frame.module]
                    .functions
                    .get(method.method)
                    .unwrap()
                    .name
                    .as_ref()
                    .unwrap(),
                state.deref(method.pointer).to_string(vm, state, modules)
            ),
            Value::PhoenixModule(module) => format!("<module {}>", module),
        }
    }

    pub fn as_float(&self) -> Option<f32> {
        if let Value::Float(val) = self {
            Some(*val)
        } else if let Value::Long(val) = self {
            Some(*val as f32)
        } else {
            None
        }
    }

    pub fn as_long(&self) -> Option<i64> {
        if let Value::Long(val) = self {
            Some(*val)
        } else if let Value::Float(val) = self {
            Some(*val as i64)
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(val) => Some(*val),
            Value::Long(val) => Some(*val != 0),
            Value::Float(val) => Some(*val != 0.0),
            _ => None,
        }
    }

    pub fn as_string<'a>(&'a self, state: &'a VMState) -> Option<&String> {
        if let Value::PhoenixPointer(x) = self {
            Some(&state.deref_string(*x).value)
        } else {
            None
        }
    }

    pub const fn get_type(&self) -> &str {
        match self {
            Value::Float(_) => "float",
            Value::Long(_) => "long",
            Value::Bool(_) => "bool",
            Value::Nil => "nil",
            Value::PhoenixFunction(_) => "function",
            Value::PhoenixClass(_) => "class",
            Value::NativeFunction(_, _) => "native_function",
            Value::PhoenixPointer(_) => "pointer",
            Value::PhoenixBoundMethod(_) => "bound_method",
            _ => "unknown (Please report this bug)",
        }
    }

    /// Hard cast to a ObjPointer. Panics if this value is not a PhoenixPointer
    pub fn as_pointer(&self) -> usize {
        if let Value::PhoenixPointer(ptr) = self {
            *ptr
        } else {
            panic!(
                "VM panic! Failed to cast value to a pointer. Found {:?} instead",
                self
            )
        }
    }

    /// Convert arg to a float and do some error checks (without casting), never panics, unlike as_float()
    pub fn to_float(&self) -> Result<f32, String> {
        match self.as_float() {
            Some(num) => {
                if num.is_nan() {
                    return Err("Invalid argument: expected number, got NaN".to_string());
                }
                Ok(num)
            }
            None => Err(format!(
                "Invalid argument: expected number: instead got {}",
                self.get_type()
            )),
        }
    }

    /// Convert arg to a long and do some error checks (without casting), never panics, unlike as_long()
    pub fn to_long(&self) -> Result<i64, String> {
        match self.as_long() {
            Some(num) => Ok(num),
            None => Err(format!(
                "Invalid argument: expected number: instead got {}",
                self.get_type()
            )),
        }
    }

    /// Convert arg to a bool and do some error checks (without casting), never panics, unlike as_bool()
    pub fn to_bool(&self) -> Result<bool, String> {
        match self.as_bool() {
            Some(val) => Ok(val),
            None => Err(format!(
                "Invalid argument: expected boolean: instead got {}",
                self.get_type()
            )),
        }
    }

    /// Convert arg to a list and some error checks (without casting), never panics, unlike as_list()
    pub fn to_list(&self, state: &VMState) -> Result<Vec<Value>, String> {
        if let Value::PhoenixPointer(p) = self {
            if let HeapObjVal::PhoenixList(_) = &state.deref(*p).obj {
                return Ok(state.deref(*p).obj.as_list().values.clone());
            }
        }
        Err(format!(
            "Invalid argument: expected list: instead got {}",
            self.get_type()
        ))
    }

    /// Convert self to a class and some error checks (without casting), never panics, unlike as_class()
    pub fn to_class<'a>(&self, state: &'a VMState) -> Result<&'a ObjInstance, String> {
        if let Value::PhoenixPointer(p) = self {
            if let HeapObjVal::PhoenixInstance(_) = &state.deref(*p).obj {
                return Ok(state.deref(*p).obj.as_instance());
            }
        }
        Err(format!(
            "Invalid argument: expected class: instead got {}",
            self.get_type()
        ))
    }
}

pub fn is_falsey(val: &Value) -> bool {
    matches!(val, Value::Bool(false) | Value::Nil)
}

pub fn values_equal(t: (&Value, &Value)) -> bool {
    match t {
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Long(x), Value::Long(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        (Value::PhoenixString(x), Value::PhoenixString(y)) => x.eq(y),
        (Value::PhoenixPointer(x), Value::PhoenixPointer(y)) => x == y,
        (Value::PhoenixClass(x), Value::PhoenixClass(y)) => x == y,
        (Value::PhoenixFunction(x), Value::PhoenixFunction(y)) => x == y,
        (Value::NativeFunction(x, x2), Value::NativeFunction(y, y2)) => x == y && x2 == y2,
        (Value::PhoenixBoundMethod(x), Value::PhoenixBoundMethod(y)) => x == y,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Ord, PartialOrd, Eq)]
pub struct ObjBoundMethod {
    pub method: usize,
    // Index into the functions vec for which function to call
    pub pointer: usize, // Pointer to the PhoenixInstance that this method is bound to
}

// End of stack/implicit copy objects

// Heap Objects

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum HeapObjType {
    HeapPlaceholder,
    PhoenixInstance,
    PhoenixClosure,
    PhoenixList,
    PhoenixString,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct HeapObj {
    pub obj: HeapObjVal,
    pub obj_type: HeapObjType,
    pub is_marked: bool,
}

impl HeapObj {
    fn to_string(&self, vm: &VM, state: &VMState, modules: &Vec<ModuleChunk>) -> String {
        self.obj.to_string(vm, state, modules)
    }

    pub fn new_instance(val: ObjInstance) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::PhoenixInstance(val),
            obj_type: HeapObjType::PhoenixInstance,
            is_marked: false,
        }
    }

    pub fn new_closure(val: ObjClosure) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::PhoenixClosure(val),
            obj_type: HeapObjType::PhoenixClosure,
            is_marked: false,
        }
    }

    pub fn new_placeholder() -> HeapObj {
        HeapObj {
            obj: HeapObjVal::HeapPlaceholder,
            obj_type: HeapObjType::HeapPlaceholder,
            is_marked: false,
        }
    }

    pub fn new_list(val: ObjList) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::PhoenixList(val),
            obj_type: HeapObjType::PhoenixList,
            is_marked: false,
        }
    }

    pub fn new_string(val: ObjString) -> HeapObj {
        HeapObj {
            obj: HeapObjVal::PhoenixString(val),
            obj_type: HeapObjType::PhoenixString,
            is_marked: false,
        }
    }
}

// I swear i really tried to not have this be duplicate with HeapObjType, but couldn't figure out a way to do it
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum HeapObjVal {
    HeapPlaceholder,
    PhoenixInstance(ObjInstance),
    PhoenixClosure(ObjClosure),
    PhoenixString(ObjString),
    PhoenixList(ObjList),
}

impl HeapObjVal {
    fn to_string(&self, vm: &VM, state: &VMState, modules: &Vec<ModuleChunk>) -> String {
        match self {
            HeapObjVal::PhoenixClosure(closure) => format!(
                "<fn {} | {:?}>",
                &modules[state.current_frame.module]
                    .functions
                    .get(closure.function)
                    .unwrap()
                    .name
                    .as_ref()
                    .unwrap(),
                closure
            ),
            HeapObjVal::PhoenixInstance(instance) => format!(
                "<instance {}>",
                &modules[state.current_frame.module]
                    .classes
                    .get(instance.class)
                    .unwrap()
                    .name
            ),
            HeapObjVal::PhoenixList(list) => format!(
                "[{}]",
                list.values
                    .iter()
                    .map(|x| x.to_string(vm, state, modules))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            HeapObjVal::HeapPlaceholder => {
                panic!("VM panic! How did a placeholder value get here?")
            }
            HeapObjVal::PhoenixString(string) => string.value.clone(),
        }
    }

    pub fn as_closure(&self) -> &ObjClosure {
        if let HeapObjVal::PhoenixClosure(closure) = self {
            closure
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_closure_mut(&mut self) -> &mut ObjClosure {
        if let HeapObjVal::PhoenixClosure(closure) = self {
            closure
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_instance(&self) -> &ObjInstance {
        if let HeapObjVal::PhoenixInstance(instance) = self {
            instance
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_instance_mut(&mut self) -> &mut ObjInstance {
        if let HeapObjVal::PhoenixInstance(instance) = self {
            instance
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_list(&self) -> &ObjList {
        if let HeapObjVal::PhoenixList(list) = self {
            list
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_list_mut(&mut self) -> &mut ObjList {
        if let HeapObjVal::PhoenixList(list) = self {
            list
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_string(&self) -> &ObjString {
        if let HeapObjVal::PhoenixString(string) = self {
            string
        } else {
            panic!("VM panic!")
        }
    }

    pub fn as_string_mut(&mut self) -> &mut ObjString {
        if let HeapObjVal::PhoenixString(string) = self {
            string
        } else {
            panic!("VM panic!")
        }
    }
}

/// Runtime instantiation of class definitions
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ObjInstance {
    pub class: usize,
    // Which class was this instance made from?
    pub fields: HashMap<usize, Value>, // Stores the field values. FunctionChunks are stored in the ClassChunk, which is not ideal since it adds an extra vec lookup before getting to the function
}

impl ObjInstance {
    pub fn new(class: usize) -> ObjInstance {
        ObjInstance {
            class,
            fields: HashMap::new(),
        }
    }
}

/// Runtime representation of the closure, ie what variables are in scope
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ObjClosure {
    pub function: usize,
    pub values: Vec<Value>, // Will be filled at runtime
}

impl ObjClosure {
    pub fn new(function: usize) -> ObjClosure {
        ObjClosure {
            function,
            values: Vec::new(),
        }
    }
}

/// Runtime representation of a list
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ObjList {
    pub values: Vec<Value>,
}

impl ObjList {
    pub fn new(v: Vec<Value>) -> ObjList {
        ObjList { values: v }
    }
}

/// Runtime representation of a string
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ObjString {
    pub value: String,
}

impl ObjString {
    pub fn new(value: String) -> ObjString {
        ObjString { value }
    }
}
