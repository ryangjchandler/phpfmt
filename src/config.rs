use std::path::Path;

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    pub paths: Vec<String>,
}

pub enum ConfigError {
    NotFound,
    Invalid(String),
    Unreadable,
}

pub fn load() -> Result<Config, ConfigError> {
    let path = Path::new("./phpfmt.toml");

    if !path.exists() {
        return Err(ConfigError::NotFound);
    }

    let contents = match std::fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(_) => return Err(ConfigError::Unreadable),
    };

    match toml::from_str(&contents) {
        Ok(config) => Ok(config),
        Err(error) => Err(ConfigError::Invalid(error.to_string())),
    }
}
