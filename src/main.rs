mod interpreter;
mod parser;

use std::path::PathBuf;
use std::{fs, io};

use clap::Parser as ArgParser;
use miette::{Diagnostic, IntoDiagnostic as _};
use parser::ParseError;
use thiserror::Error;

use interpreter::{Interpreter, RuntimeError};

#[derive(ArgParser, Debug)]
#[command(version)]
struct Args {
    script_path: Option<PathBuf>,
}

//TODO implement semantic analysis pass to:
// * validate that all variables are declared
// * validate that all variables are used
// * validate that returns are not used outside of a function

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
    use miette::Report;

    use super::{Interpreter, LoxError};

    fn verify_output(source: &str, expected_output: &[&str], print_last: bool) {
        let mut interpreter = Interpreter::new();
        match interpreter.run(source, print_last) {
            Err(e) => {
                let e = Report::from(e).with_source_code(source.to_owned());
                eprintln!("failed to parse '{source}': {e:?}");
                panic!()
            }
            Ok(output) => assert_eq!(output, expected_output),
        }
    }

    #[test]
    fn test_8_1() {
        let source = "print \"one\";\nprint true;\nprint 2 + 1;";
        let expected_output = vec!["one", "true", "3"];

        verify_output(source, &expected_output, true);
    }

    #[test]
    fn binary_argument_order() {
        verify_output("print 2 - 1", &["1"], true);
    }

    #[test]
    fn associativity() {
        verify_output("print 1 - 2 - 3", &["-4"], true);
    }

    #[test]
    fn grouping() {
        verify_output("print 2 * (3 + 4)", &["14"], true);
    }

    #[test]
    fn precedence() {
        verify_output("print 2 * 3 + 4", &["10"], true);
    }

    #[test]
    fn test_8_2() {
        let source = "var beverage = \"espresso\"; beverage;";
        let expected_output = ["espresso"];
        verify_output(source, &expected_output, true);
    }

    #[test]
    fn test_8_3() {
        let source = "var a = \"before\"; print a; var a = \"after\"; print a;";
        let expected_output = ["before", "after"];

        verify_output(source, &expected_output, true);
    }

    #[test]
    fn test_8_3_1() {
        verify_output("var a; a;", &["nil"], true);

        verify_output("var a = 1; var b = 2; a + b;", &["3"], true);
    }

    #[test]
    fn test_8_4_1() {
        let source = "var a = \"before\"; a = \"value\"; a;";
        verify_output(source, &["value"], true);
    }

    #[test]
    fn test_8_4_2() {
        verify_output("var a = 1; print a = 2;", &["2"], true);
    }

    #[test]
    fn test_8_5() {
        use crate::interpreter::RuntimeError;
        let mut interpreter = Interpreter::new();

        let source = "{  var a = \"in block\"; } print a;";
        let LoxError::Runtime(error) = interpreter.run(source, true).unwrap_err() else {
            panic!()
        };

        assert_eq!(error, RuntimeError::UndeclaredVariable("a".into()));
    }

    #[test]
    fn test_8_5_2() {
        let source = "var a = \"global a\"; var b = \"global b\"; var c = \"global c\"; { var a = \"outer a\"; var b = \"outer b\"; { var a = \"inner a\"; print a; print b; print c; } print a; print b; print c; } print a; print b; print c;";
        let expected_output = vec![
            "inner a", "outer b", "global c", "outer a", "outer b", "global c", "global a",
            "global b", "global c",
        ];
        verify_output(source, &expected_output, true);
    }

    #[test]
    fn test_9_3() {
        let source = "print \"hi\" or 2; print nil or \"yes\";";
        let expected_output = ["hi", "yes"];
        verify_output(source, &expected_output, true);
    }

    #[test]
    fn test_9_5() {
        let source = "var a = 0; var temp; for (var b = 1; a < 10000; b = temp + b) { print a; temp = a; a = b; }";
        let expected_output = [
            "0", "1", "1", "2", "3", "5", "8", "13", "21", "34", "55", "89", "144", "233", "377",
            "610", "987", "1597", "2584", "4181", "6765",
        ];
        verify_output(source, &expected_output, false);
    }

    #[test]
    fn test_10_4() {
        let source = "fun add(a, b, c) { print a + b + c; } add(1, 2, 3);";
        let expected_output = ["6"];
        verify_output(source, &expected_output, false);

        let source = "fun add(a, b) { print a + b; } print add; ";
        let expected_output = ["<fn add>"];
        verify_output(source, &expected_output, false);
    }

    #[test]
    fn test_10_4_1() {
        let source = "fun sayHi(first, last) { print \"Hi, \" + first + \" \" + last + \"!\";} sayHi(\"Dear\", \"Reader\");";
        let expected_output = ["Hi, Dear Reader!"];
        verify_output(source, &expected_output, false);
    }

    #[test]
    fn test_10_5() {
        let source = "fun procedure() { print \"don't return anything\"; } var result = procedure(); print result;";
        let expected_output = ["don't return anything", "nil"];
        verify_output(source, &expected_output, false);
    }

    #[test]
    fn test_10_5_1() {
        let source = "fun fib(n) { if (n <= 1) return n; return fib(n - 2) + fib(n - 1); } for (var i = 0; i < 20; i = i + 1) { print fib(i); }";
        let expected_output = [
            "0", "1", "1", "2", "3", "5", "8", "13", "21", "34", "55", "89", "144", "233", "377",
            "610", "987", "1597", "2584", "4181",
        ];
        verify_output(source, &expected_output, false);
    }

    #[test]
    fn test_10_6() {
        let source = "fun makeCounter() { var i = 0; fun count() { i = i + 1; print i; } return count; } var counter = makeCounter(); counter(); counter(); ";
        let expected_output = ["1", "2"];
        verify_output(source, &expected_output, false);
    }

    #[test]
    fn test_11_1() {
        let source =
            "var a = \"global\"; { fun showA() { print a; } showA(); var a = \"block\"; showA(); }";
        let expected_output = ["global", "global"];
        verify_output(source, &expected_output, false);
    }
}
