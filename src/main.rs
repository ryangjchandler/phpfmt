use std::path::PathBuf;

use clap::{Parser, Subcommand};

mod cmd;
mod config;
mod printer;

#[derive(Debug, Parser)]
#[clap(
    about = "An opinionated formatter for PHP.",
    author = "Ryan Chandler <support@ryangjchandler.co.uk>"
)]
struct App {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    #[clap(about = "Initialise a new project.")]
    Init,

    #[clap(about = "Run the formatter.")]
    Run {
        #[clap(help = "An optional file to format.")]
        file: Option<PathBuf>,
    },
}

fn main() {
    let app = App::parse();

    match app.command {
        Command::Init => cmd::init(),
        Command::Run { file } => {
            let config = match config::load() {
                Ok(config) => config,
                Err(error) => {
                    eprintln!(
                        "Failed to load config: {}",
                        match error {
                            config::ConfigError::Invalid(message) => message,
                            config::ConfigError::NotFound => "Config file not found.".into(),
                            config::ConfigError::Unreadable => "Config file is unreadable.".into(),
                        }
                    );
                    return;
                }
            };

            cmd::run(config, file);
        }
    }
}
