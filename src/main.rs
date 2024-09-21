mod interpreter;
mod parser;
mod tokenizer;

use std::io::Write;
use std::path::PathBuf;
use std::{error, fmt, fs, io};

use anyhow::Context as _;
use clap::Parser as ArgParser;

use interpreter::Interpreter;
use parser::Parser;
use tokenizer::Tokenizer;

#[derive(ArgParser, Debug)]
#[command(version)]
struct Args {
    script_path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut interpreter = Interpreter;
    if let Some(script_path) = &args.script_path {
        run_file(&mut interpreter, script_path)?;
    } else {
        run_prompt(&mut interpreter)?;
    }
    Ok(())
}

fn run_file(interpreter: &mut Interpreter, path: &PathBuf) -> anyhow::Result<()> {
    let script = fs::read_to_string(path).context("Failed to read file")?;
    run(interpreter, &script)?;
    Ok(())
}

#[expect(
    clippy::significant_drop_tightening,
    reason = "https://github.com/rust-lang/rust-clippy/issues/12121"
)]
fn run_prompt(interpreter: &mut Interpreter) -> anyhow::Result<()> {
    let mut input = io::stdin().lines();
    let mut stdout = io::stdout();

    loop {
        stdout.write_all(b"> ")?;
        stdout.flush()?;
        let Some(line) = input.next() else {
            break;
        };
        let line = line.context("Failed to read from stdin")?;

        run(interpreter, &line)?;
    }
    Ok(())
}

fn run(interpreter: &mut Interpreter, source: &str) -> anyhow::Result<()> {
    let tokenizer = Tokenizer::new(source);
    let tokens = tokenizer.into_tokens().context("Lexing error")?;
    let mut parser = Parser::new(&tokens);
    let expression = parser
        .expression()
        .map_err(|e| StringError(e.to_string()))
        .context("Parsing error")?
        .context("No expression")?;

    match interpreter.interpret(&expression) {
        Ok(()) => (),
        Err(e) => println!("Runtime error: {e}"),
    };

    Ok(())
}

struct StringError(String);

impl fmt::Display for StringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for StringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl error::Error for StringError {}
