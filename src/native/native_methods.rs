use lazy_static::lazy_static;
use std::sync::Mutex;

use crate::chunk::ModuleChunk;
use crate::value::Value::*;
use crate::value::{HeapObjVal, Value};
use crate::vm::{VMState, VM};
use std::collections::HashMap;

/// Format: self, args, vm, vm_state, modules, if it returns none, this function doesnt support the type
/// Note: self is not mutable, because its the Pointer to the object, or a "primitive" value
pub type NativeMethod =
    fn(Value, Vec<Value>, &VM, &mut VMState, &[ModuleChunk]) -> Option<Result<Value, String>>;

lazy_static! {
    /// Format: name, (arity, function) if arity is None, the function is variadic
    pub static ref NATIVE_METHODS: Mutex<HashMap<&'static str, (Option<usize>, NativeMethod)>> =
        Mutex::new(HashMap::from([
            (
                "to_string",
                (Some(0), to_string as NativeMethod)
            ),
            (
                "type",
                (Some(0), |this,_,_,_,_| {
                    Some(Ok(PhoenixString(this.get_type().to_string())))
                })
            ),
            (
                "push",
                (Some(1), |this, args, vm, state, modules| {
                    // prepare a string in case we need it
                    let push_string = &args[0].to_string(vm, state, &modules.to_vec());
                    match this {
                        PhoenixPointer(ptr) => {
                            let obj = state.deref_mut(ptr);
                            match obj.obj {
                                HeapObjVal::PhoenixList(ref mut list) => {
                                    list.values.push(args[0].clone());
                                    Some(Ok(Nil))
                                }
                                HeapObjVal::PhoenixString(ref mut string) => {
                                    string.value.push_str(push_string);
                                    Some(Ok(Nil))
                                }
                                _ => None
                            }
                        }
                        _ => None
                    }
                })
            ),
            (
                "pop",
                (Some(0), |this,_,_,state,_| {
                    match this {
                        PhoenixPointer(ptr) => {
                            let obj = state.deref_mut(ptr);
                            match obj.obj {
                                HeapObjVal::PhoenixList(ref mut list) => {
                                    Some(Ok(list.values.pop().unwrap_or(Nil)))
                                }
                                HeapObjVal::PhoenixString(ref mut string) => {
                                    Some(Ok(PhoenixString(string.value.pop().unwrap_or('\0').to_string())))
                                }
                                _ => None
                            }
                        }
                        _ => None
                    }
                })
            ),
            (
                "len",
                (Some(0), |this,_,_,state,_| {
                    match this {
                        PhoenixPointer(ptr) => {
                            let obj = state.deref_mut(ptr);
                            match obj.obj {
                                HeapObjVal::PhoenixList(ref mut list) => {
                                    Some(Ok(Long(list.values.len() as i64)))
                                }
                                HeapObjVal::PhoenixString(ref mut string) => {
                                    Some(Ok(Long(string.value.len() as i64)))
                                }
                                HeapObjVal::PhoenixHashMap(ref mut hashmap) => {
                                    Some(Ok(Long(hashmap.map.len() as i64)))
                                }
                                _ => None
                            }
                        }
                        _ => None
                    }
                })
            ),
            (
                "sort",
                (Some(0), |this,_,_,state,_| {
                    match this {
                        PhoenixPointer(ptr) => {
                            let obj = state.deref_mut(ptr);
                            match obj.obj {
                                HeapObjVal::PhoenixList(ref mut list) => {
                                    list.values.sort_by(|a, b| {
                                        if let Value::Float(a) = a {
                                            if let Value::Float(b) = b {
                                                return a.partial_cmp(b).unwrap();
                                            }
                                        }
                                        if let Value::Long(a) = a {
                                            if let Value::Long(b) = b {
                                                return a.partial_cmp(b).unwrap();
                                            }
                                        }
                                        panic!("Attempted to sort a list with non-numeric values");
                                    });
                                    Some(Ok(Nil))
                                }
                                _ => None
                            }
                        }
                        _ => None
                    }
                })
            ),
            (
                "get",
                (Some(1), |this,args,_,state,_| {
                    match this {
                        PhoenixPointer(ptr) => {
                            let obj = state.deref_mut(ptr);
                            match obj.obj {
                                HeapObjVal::PhoenixHashMap(ref mut map) => {
                                    println!("{:?}, arg: {:?}", map.map, args);
                                    Some(Ok(map.map.get(&args[0]).unwrap_or(&Nil).clone()))
                                }
                                _ => None
                            }
                        }
                        _ => None
                    }
                })
            ),
        ]));
}

fn to_string(
    this: Value,
    _args: Vec<Value>,
    vm: &VM,
    state: &mut VMState,
    modules: &[ModuleChunk],
) -> Option<Result<Value, String>> {
    Some(Ok(PhoenixString(this.to_string(
        vm,
        state,
        &modules.to_vec(),
    ))))
}
