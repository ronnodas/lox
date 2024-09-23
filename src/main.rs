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
        let output = interpreter.run(source).unwrap();
        assert_eq!(output, vec!["\"one\"", "true", "3"]);
    }

    #[test]
    fn test_8_2() {
        let mut interpreter = Interpreter::new();

        let source = "var beverage = \"espresso\"; print beverage;";
        let output = interpreter.run(source).unwrap();
        assert_eq!(output, vec!["\"espresso\""]);
    }

    #[test]
    fn test_8_3() {
        let mut interpreter = Interpreter::new();

        let source = "var a = \"before\"; print a; var a = \"after\"; print a;";
        let output = interpreter.run(source).unwrap();
        assert_eq!(output, vec!["\"before\"", "\"after\""]);
    }

    #[test]
    fn test_8_3_1() {
        let mut interpreter = Interpreter::new();

        let source = "var a; print a;";
        let output = interpreter.run(source).unwrap();
        assert_eq!(output, vec!["nil"]);

        let source = "var a = 1; var b = 2; print a + b;";
        let output = interpreter.run(source).unwrap();
        assert_eq!(output, vec!["3"]);
    }

    #[test]
    fn test_8_4_1() {
        let mut interpreter = Interpreter::new();

        let source = "var a = \"before\"; a = \"value\"; print a;";
        let output = interpreter.run(source).unwrap();
        assert_eq!(output, vec!["\"value\""]);
    }

    #[test]
    fn test_8_4_2() {
        let mut interpreter = Interpreter::new();
        let source = "var a = 1; print a = 2;";
        let output = interpreter.run(source).unwrap();
        assert_eq!(output, vec!["2"]);
    }

    #[test]
    fn test_8_5() {
        use crate::interpreter::{Error, RuntimeError};
        let mut interpreter = Interpreter::new();

        let source = "{  var a = \"in block\"; } print a;";
        let output = interpreter.run(source).unwrap_err();
        assert_eq!(
            output,
            Error::Runtime(RuntimeError::UndeclaredVariable("a".into()))
        );
    }

    #[test]
    fn test_8_5_2() {
        let mut interpreter = Interpreter::new();

        let source = "var a = \"global a\"; var b = \"global b\"; var c = \"global c\"; { var a = \"outer a\"; var b = \"outer b\"; { var a = \"inner a\"; print a; print b; print c; } print a; print b; print c; } print a; print b; print c;";
        let output = interpreter.run(source).unwrap();
        assert_eq!(
            output,
            vec![
                "\"inner a\"",
                "\"outer b\"",
                "\"global c\"",
                "\"outer a\"",
                "\"outer b\"",
                "\"global c\"",
                "\"global a\"",
                "\"global b\"",
                "\"global c\""
            ]
        );
    }
}
