use crate::{error, info};
use std::fs;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::process::exit;

pub fn read_file(file_name: String) -> Result<String, String> {
    let path = Path::new(&file_name);
    let path_display = path.display();

    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(why) => {
            return Err(format!("Failed to open {}: {}", path_display, why));
        }
    };
    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Ok(_) => Ok(s.to_string()),
        Err(why) => Err(format!("Failed to read {}: {}", path_display, why)),
    }
}

pub fn get_file_as_byte_vec(filename: String) -> Result<Vec<u8>, String> {
    let mut f = match File::open(&filename) {
        Ok(file) => file,
        Err(why) => {
            return Err(format!("Failed to open {}: {}", filename, why));
        }
    };
    let metadata = match fs::metadata(&filename) {
        Ok(metadata) => metadata,
        Err(why) => {
            return Err(format!("Failed to get metadata for {}: {}", filename, why));
        }
    };
    let mut buffer = vec![0; metadata.len() as usize];
    match f.read_exact(&mut buffer) {
        Ok(_) => {}
        Err(why) => {
            return Err(format!("Failed to read {}: {}", filename, why));
        }
    }

    Ok(buffer)
}

pub fn write_to_file(filename: &String, content: Vec<u8>) {
    let path = Path::new(&filename);
    let path_display = path.display();

    let mut file = match File::create(path) {
        Ok(file) => file,
        Err(why) => {
            eprintln!("Failed to create {}: {}", path_display, why);
            exit(1);
        }
    };

    match file.write_all(&content) {
        Ok(_) => {
            info!("Successfully wrote to {}", path_display);
        }
        Err(why) => {
            error!("Failed to write to {}: {}", path_display, why);
            exit(1);
        }
    };
}
