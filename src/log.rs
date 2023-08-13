#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {{
        use colored::Colorize;
        println!("{} {}", "[INFO]".green(), format!($($arg)*));
    }};
}

#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => {{
        use colored::Colorize;
        println!("{} {}", "[WARNING]".yellow(), format!($($arg)*));
    }};
}

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {{
        use colored::Colorize;
        // reference to: https://xkcd.com/2200/
        eprintln!("{} {}", "[ERROR]".red(), format!($($arg)*));
    }};
}

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        use colored::Colorize;
        print!("{} ", "[DEBUG]".blue());
        dbg!(format!($($arg)*));
    };
}

#[macro_export]
macro_rules! phoenix_error {
    ($($arg:tt)*) => {{
        use colored::Colorize;
        use std::process::exit;
        // reference to: https://xkcd.com/2200/
        eprintln!("[{}:{}] {} {}\nPlease Report this as a bug here: https://github.com/TomtheCoder2/phoenix\n\n{}", file!(), line!(), "[PHOENIX ERROR]".red(), format!($($arg)*), "\
        If you're seeing this, the code is in what I thought was an unreachable state. \
        I could give you advice for what to do. \
        But honestly, why should you trust me? \
        I clearly screwed this up. I'm writing a message that should never appear, yet \
        I know it will probably appear someday. \
        On a deep level, I know I'm not up to this task. I'm so sorry.");
        exit(1);
    }};
}
