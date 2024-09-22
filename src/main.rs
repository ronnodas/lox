mod interpreter;
mod parser;
mod tokenizer;

use std::path::PathBuf;
use std::{fs, io};

use anyhow::Context as _;
use clap::Parser as ArgParser;

use interpreter::Interpreter;

#[derive(ArgParser, Debug)]
#[command(version)]
struct Args {
    script_path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut interpreter = Interpreter;
    if let Some(script_path) = &args.script_path {
        let script = fs::read_to_string(script_path).context("Failed to read file")?;
        interpreter.run_and_print(&script);
    } else {
        interpreter.run_with_prompt(io::stdout(), io::stdin().lines())?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_8_1() {
        let mut interpreter = Interpreter;
        let source = "print \"one\";\nprint true;\nprint 2 + 1;";
        let output = interpreter.collect_output(source).unwrap();

        assert_eq!(output, vec!["\"one\"", "true", "3"]);
    }
}
