use crate::value::Value;
use crate::vm::VMState;

/// Convert arg to a float and do some error checks (without casting)
#[deprecated]
pub fn to_float(arg: &Value) -> Result<f32, String> {
    arg.to_float()
}

/// Convert arg to a long and do some error checks (without casting)
#[deprecated]
pub fn to_long(arg: &Value) -> Result<i64, String> {
    arg.to_long()
}

/// Convert arg to a bool and do some error checks (without casting)
#[deprecated]
pub fn to_bool(arg: &Value) -> Result<bool, String> {
    arg.to_bool()
}

/// Convert arg to a list and some error checks (without casting)
#[deprecated]
pub fn to_list(arg: &Value, state: &VMState) -> Result<Vec<Value>, String> {
    arg.to_list(state)
}
