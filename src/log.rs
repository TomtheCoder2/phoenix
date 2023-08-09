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
        eprintln!("{} {}\n\tPlease Report this as a bug", "[PHOENIX ERROR]".red(), format!($($arg)*));
        exit(1);
    }};
}
