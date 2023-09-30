use lazy_static::lazy_static;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::value::Value;
use crate::value::Value::*;
use std::collections::HashMap;

pub type NativeFn = fn(Vec<Value>) -> Result<Value, String>;

lazy_static! {
    // format: name, (arity, function) if arity is None, the function is variadic
    pub static ref NATIVE_FUNCTIONS: Mutex<HashMap<&'static str, (Option<usize>, NativeFn)>> =
        Mutex::new(HashMap::from([
            ("clock", (Some(0), clock as NativeFn)),
            (
                "to_string",
                (Some(1), |args| {
                    Ok(PhoenixString(value_to_string(&args[0])))
                })
            ),
            (
                "type",
                (Some(1), |args| {
                    Ok(PhoenixString(args[0].get_type_string().to_string()))
                })
            ),
            ("printf", (None, printf as NativeFn)),
            ("int", (Some(1), int as NativeFn)),
            ("float", (Some(1), float as NativeFn))
        ]));
}

pub fn clock(_args: Vec<Value>) -> Result<Value, String> {
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
        PhoenixString(x) => x.to_string(),
        _ => {
            todo!("to_string() not implemented for this type")
        }
    }
}

/// This formats and prints messages to the console like print!("{} {}", "Hello", "World"); in rust
fn printf(args: Vec<Value>) -> Result<Value, String> {
    match &args[0] {
        PhoenixString(s) => {
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
                    text.insert_str(i, &value_to_string(args.get(0).unwrap()));
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

pub fn int(args: Vec<Value>) -> Result<Value, String> {
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
                args[0].get_type_string()
            ));
        }
    })
}

pub fn float(args: Vec<Value>) -> Result<Value, String> {
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
                args[0].get_type_string()
            ));
        }
    })
}
