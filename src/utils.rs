use crate::value::{HeapObjVal, Value};
use crate::vm::VMState;

/// Convert arg to a float and do some error checks (without casting)
pub fn to_float(arg: &Value) -> Result<f32, String> {
    match arg.as_float() {
        Some(num) => {
            if num.is_nan() {
                return Err("Invalid argument: expected number, got NaN".to_string());
            }
            Ok(num)
        }
        None => Err(format!(
            "Invalid argument: expected number: instead got {}",
            arg.get_type()
        )),
    }
}

/// Convert arg to a long and do some error checks (without casting)
pub fn to_long(arg: &Value) -> Result<i64, String> {
    match arg.as_long() {
        Some(num) => Ok(num),
        None => Err(format!(
            "Invalid argument: expected number: instead got {}",
            arg.get_type()
        )),
    }
}

/// Convert arg to a bool and do some error checks (without casting)
pub fn to_bool(arg: &Value) -> Result<bool, String> {
    match arg.as_bool() {
        Some(val) => Ok(val),
        None => Err(format!(
            "Invalid argument: expected boolean: instead got {}",
            arg.get_type()
        )),
    }
}

/// Convert arg to a list and some error checks (without casting)
pub fn to_list(arg: &Value, state: &VMState) -> Result<Vec<Value>, String> {
    if let Value::PhoenixPointer(p) = arg {
        if let HeapObjVal::PhoenixList(_) = &state.deref(*p).obj {
            return Ok(state.deref(*p).obj.as_list().values.clone())
        }
    }
    Err(format!(
        "Invalid argument: expected list: instead got {}",
        arg.get_type()
    ))
}