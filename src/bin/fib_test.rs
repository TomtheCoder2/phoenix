fn test(mut n: i32) {
    while n > 0 {
        n = n - 1;
    }
}

fn main() {
    // start time
    let start = std::time::Instant::now();
    test(4000000);
    // end time
    let end = std::time::Instant::now();
    // duration
    let duration = end.duration_since(start);
    println!("Time elapsed in expensive_function() is: {:?}", duration);
}
