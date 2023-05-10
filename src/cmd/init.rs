use std::path::Path;

const CONFIG_STUB: &'static str = include_str!("../../stubs/phpfmt.toml");

pub fn init() {
    if Path::new("./phpfmt.toml").exists() {
        println!("Project already initialised.");
        return;
    }

    match std::fs::write("./phpfmt.toml", CONFIG_STUB) {
        Ok(_) => {
            println!("Project initialised!");
        }
        Err(_) => {
            eprintln!("Failed to initialise project.")
        }
    };
}
