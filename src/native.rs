use lazy_static::lazy_static;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::value::Value;
use crate::value::Value::*;
use std::collections::HashMap;
use crate::chunk::ModuleChunk;
use crate::vm::{VM, VMState};
use rand::Rng;

// todo: create methods for eg lists, classes or strings and replace the hard coded versions in the vm (push, pop, len, etc)
pub type NativeFn = fn(Vec<Value>, &VM, &mut VMState, &[ModuleChunk]) -> Result<Value, String>;

lazy_static! {
    // format: name, (arity, function) if arity is None, the function is variadic
    pub static ref NATIVE_FUNCTIONS: Mutex<HashMap<&'static str, (Option<usize>, NativeFn)>> =
        Mutex::new(HashMap::from([
            ("clock", (Some(0), clock as NativeFn)),
            (
                "to_string",
                (Some(1), |args,_,_,_| {
                    Ok(PhoenixString(value_to_string(&args[0])))
                })
            ),
            (
                "type",
                (Some(1), |args,_,_,_| {
                    Ok(PhoenixString(args[0].get_type().to_string()))
                })
            ),
            ("printf", (None, printf as NativeFn)),
            ("int", (Some(1), int as NativeFn)),
            ("float", (Some(1), float as NativeFn)),
            ("rand", (Some(0), |_,_,_,_| {
                Ok(Float(rand::random::<f32>()))
            })),
            ("rand_int", (Some(2), |args,_,_,_| {
                let min = match args[0] {
                    Float(f) => f as i32,
                    Long(l) => l as i32,
                    _ => {
                        return Err(format!("Expected int or float as first argument, got {}", args[0].get_type()));
                    }
                } as i64;
                let max = match args[1] {
                    Float(f) => f as i32,
                    Long(l) => l as i32,
                    _ => {
                        return Err(format!("Expected int or float as second argument, got {}", args[1].get_type()));
                    }
                } as i64;
                if min - max == 0 {
                    return Err("min and max cannot be the same!".to_string());
                }
                Ok(Long(rand::thread_rng().gen_range(min..max)))
            })),
            ("rand_float", (Some(2), |args,_,_,_| {
                let min = match args[0] {
                    Float(f) => f,
                    Long(l) => l as f32,
                    _ => {
                        return Err(format!("Expected int or float as first argument, got {}", args[0].get_type()));
                    }
                };
                let max = match args[1] {
                    Float(f) => f,
                    Long(l) => l as f32,
                    _ => {
                        return Err(format!("Expected int or float as second argument, got {}", args[1].get_type()));
                    }
                };
                Ok(Float(rand::thread_rng().gen_range(min..max)))
            })),
        ]));
}

pub fn clock(_args: Vec<Value>, _: &VM, _: &mut VMState, _: &[ModuleChunk]) -> Result<Value, String> {
    let start = SystemTime::now();
    let since_the_epoch = match start.duration_since(UNIX_EPOCH) {
        Ok(n) => n,
        Err(_) => {
            return Err("Could not get time!".to_string());
        }
    };
    // println!("debug: time: {}", since_the_epoch.as_millis());
    Ok(Long(since_the_epoch.as_millis() as i64))
}

fn value_to_string(v: &Value) -> String {
    match v {
        Float(x) => format!("{}", x),
        Long(x) => format!("{}", x),
        Bool(x) => format!("{}", x),
        Nil => "NIL".to_string(),
        _ => {
            todo!("to_string() not implemented for this type")
        }
    }
}

/// This formats and prints messages to the console like print!("{} {}", "Hello", "World"); in rust
fn printf(args: Vec<Value>, vm: &VM, state: &mut VMState, modules: &[ModuleChunk]) -> Result<Value, String> {
    match &args[0] {
        PhoenixPointer(p) => {
            let s = &state.deref_string(*p).value;
            // get all {} in the string and replace it with the corresponding argument
            let mut args = args.clone();
            args.remove(0);
            let mut i = 0;
            let mut text = s.clone();
            while i < text.len() {
                if text.chars().nth(i).unwrap() == '{' && text.chars().nth(i + 1).unwrap() == '}' {
                    text.remove(i);
                    text.remove(i);
                    if args.is_empty() {
                        return Err(format!(
                            "[printf] To few arguments! Expected {} got {}!",
                            i + 1,
                            args.len()
                        ));
                    }
                    // text.insert_str(i, &value_to_string(args.get(0).unwrap()));
                    text.insert_str(i, &args.get(0).unwrap().to_string(vm, state, &modules.to_vec()));
                    args.remove(0);
                }
                i += 1;
            }
            println!("{}", text);
        }
        _ => {
            return Err("First argument must be a string!".to_string());
        }
    }

    Ok(Nil)
}

pub fn int(args: Vec<Value>, _: &VM, _: &mut VMState, _: &[ModuleChunk]) -> Result<Value, String> {
    Ok(match &args[0] {
        Float(f) => Long(*f as i64),
        Long(l) => Long(*l),
        Bool(b) => Long(*b as i64),
        PhoenixString(s) => Long(match s.parse::<i64>() {
            Ok(i) => i,
            Err(_) => {
                return Err(format!("Could not convert \"{s}\" to int!"));
            }
        }),
        _ => {
            return Err(format!(
                "Could not convert {} to int!",
                args[0].get_type()
            ));
        }
    })
}

pub fn float(args: Vec<Value>, _: &VM, _: &mut VMState, _: &[ModuleChunk]) -> Result<Value, String> {
    Ok(match &args[0] {
        Float(f) => Float(*f),
        Long(l) => Float(*l as f32),
        Bool(b) => Float(*b as i32 as f32),
        PhoenixString(s) => Float(
            match if s.ends_with('f') {
                &s[0..s.len() - 1]
            } else {
                s
            }
                .parse::<f32>()
            {
                Ok(i) => i,
                Err(_) => {
                    return Err(format!("Could not convert {} to float!", s));
                }
            },
        ),
        _ => {
            return Err(format!(
                "Could not convert {} to float!",
                args[0].get_type()
            ));
        }
    })
}
