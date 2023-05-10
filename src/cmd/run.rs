use std::path::PathBuf;
use colored::Colorize;

use crate::config::Config;

pub fn run(config: Config, file: Option<PathBuf>) {
    let files = if let Some(file) = file {
        Ok(vec![file])
    } else {
        discoverer::discover(&["php"], &config.paths.iter().map(String::as_str).collect::<Vec<&str>>()[..])
    };

    if files.is_err() {
        eprintln!("Failed to discover files.");
        return;
    }

    let files = files.unwrap();

    if files.is_empty() {
        println!("No files to format.");
        return;
    }

    let mut successes = 0;
    let mut errors = 0;

    for file in files.iter() {
        match format(file) {
            FormatResult::Formatted => {
                print!("{}", "✔".green());
                successes += 1;
            },
            FormatResult::Untouched => {
                print!(".");
            },
            FormatResult::Error(message) => {
                print!("{}", "✘".red());
                eprintln!("Failed to format file: {}", file.display());
                eprintln!("{}", message);
                errors += 1;
                break;
            }
        }
    }

    println!();
    println!("Formatted {} file(s), {} error(s).", successes, errors);
}

pub enum FormatResult {
    Formatted,
    Untouched,
    Error(String),
}

fn format(file: &PathBuf) -> FormatResult {
    let contents = match std::fs::read_to_string(file) {
        Ok(contents) => contents,
        Err(_) => return FormatResult::Error("Failed to read file.".into()),
    };

    let ast = match php_parser_rs::parse(&contents) {
        Ok(ast) => ast,
        Err(error_stack) => return FormatResult::Error(error_stack.report(&contents, None, true, false).unwrap()),
    };

    let formatted = crate::printer::print(&ast);

    if formatted.as_str() == contents {
        return FormatResult::Untouched;
    }

    match std::fs::write(file, formatted) {
        Ok(_) => {},
        Err(_) => return FormatResult::Error("Failed to write file".to_string()),
    };

    FormatResult::Formatted
}