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
    let mut interpreter = Interpreter::new();
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
    use super::Interpreter;

    #[test]
    fn test_8_1() {
        let mut interpreter = Interpreter::new();

        let source = "print \"one\";\nprint true;\nprint 2 + 1;";
        let output = interpreter.collect_output(source).unwrap();
        assert_eq!(output, vec!["\"one\"", "true", "3"]);
    }

    #[test]
    fn test_8_2() {
        let mut interpreter = Interpreter::new();

        let source = "var beverage = \"espresso\"; print beverage;";
        let output = interpreter.collect_output(source).unwrap();
        assert_eq!(output, vec!["\"espresso\""]);
    }

    #[test]
    fn test_8_3() {
        let mut interpreter = Interpreter::new();

        let source = "var a = \"before\"; print a; var a = \"after\"; print a;";
        let output = interpreter.collect_output(source).unwrap();
        assert_eq!(output, vec!["\"before\"", "\"after\""]);
    }

    #[test]
    fn test_8_3_1() {
        let mut interpreter = Interpreter::new();

        let source = "var a; print a;";
        let output = interpreter.collect_output(source).unwrap();
        assert_eq!(output, vec!["nil"]);

        let source = "var a = 1; var b = 2; print a + b;";
        let output = interpreter.collect_output(source).unwrap();
        assert_eq!(output, vec!["3"]);
    }

    #[test]
    fn test_8_4_1() {
        let mut interpreter = Interpreter::new();

        let source = "var a = \"before\"; a = \"value\"; print a;";
        let output = interpreter.collect_output(source).unwrap();
        assert_eq!(output, vec!["\"value\""]);
    }
}
