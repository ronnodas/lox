mod interpreter;
mod parser;
mod tokenizer;

use std::path::PathBuf;
use std::{fs, io};

use clap::Parser as ArgParser;
use miette::{Diagnostic, IntoDiagnostic as _};
use parser::ParseError;
use thiserror::Error;

use interpreter::{Interpreter, RuntimeError};
use tokenizer::Error as TokenError;

#[derive(ArgParser, Debug)]
#[command(version)]
struct Args {
    script_path: Option<PathBuf>,
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    let mut interpreter = Interpreter::new();
    if let Some(script_path) = &args.script_path {
        let script = fs::read_to_string(script_path).into_diagnostic()?;
        interpreter.run_and_print(&script);
    } else {
        interpreter.run_with_prompt(io::stdout(), io::stdin().lines())?;
    }
    Ok(())
}

#[derive(Debug, Diagnostic, Error)]
pub(crate) enum LoxError {
    #[error("IO error")]
    Io(
        #[from]
        #[source]
        io::Error,
    ),
    #[error("Tokenizer error")]
    #[diagnostic(transparent)]
    Tokenizing(
        #[from]
        #[source]
        TokenError,
    ),
    #[error("Parsing error")]
    #[diagnostic(transparent)]
    Parsing(
        #[from]
        #[source]
        ParseError,
    ),
    #[error("Runtime error")]
    #[diagnostic(transparent)]
    Runtime(
        #[from]
        #[source]
        RuntimeError,
    ),
}

#[cfg(test)]
mod tests {
    use super::{Interpreter, LoxError};

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
        use crate::interpreter::RuntimeError;
        let mut interpreter = Interpreter::new();

        let source = "{  var a = \"in block\"; } print a;";
        let LoxError::Runtime(error) = interpreter.run(source).unwrap_err() else {
            panic!()
        };

        assert_eq!(error, RuntimeError::UndeclaredVariable("a".into()));
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

    #[test]
    fn test_9_3() {
        let mut interpreter = Interpreter::new();

        let source = "print \"hi\" or 2; print nil or \"yes\";";
        let output = interpreter.run(source).unwrap();
        assert_eq!(output, vec!["\"hi\"", "\"yes\""]);
    }

    #[test]
    fn test_9_5() {
        let mut interpreter = Interpreter::new();

        let source = "var a = 0; var temp; for (var b = 1; a < 10000; b = temp + b) { print a; temp = a; a = b; }";
        let output = interpreter.run(source).unwrap();
        let expected_output = [
            "0", "1", "1", "2", "3", "5", "8", "13", "21", "34", "55", "89", "144", "233", "377",
            "610", "987", "1597", "2584", "4181", "6765",
        ];
        assert_eq!(output, expected_output);
    }
}
