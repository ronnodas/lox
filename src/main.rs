mod parser;
mod tokenizer;

use std::io::Write;
use std::path::PathBuf;
use std::{fs, io};

use anyhow::Context as _;
use clap::Parser;

use tokenizer::Tokenizer;

#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    script_path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    if let Some(script_path) = &args.script_path {
        run_file(script_path)?;
    } else {
        run_prompt()?;
    }
    Ok(())
}

fn run_file(path: &PathBuf) -> anyhow::Result<()> {
    let script = fs::read_to_string(path).context("Failed to read file")?;
    run(&script)?;
    Ok(())
}

#[expect(
    clippy::significant_drop_tightening,
    reason = "https://github.com/rust-lang/rust-clippy/issues/12121"
)]
fn run_prompt() -> anyhow::Result<()> {
    let mut input = io::stdin().lines();
    let mut stdout = io::stdout();

    loop {
        stdout.write_all(b"> ")?;
        stdout.flush()?;
        let Some(line) = input.next() else {
            break;
        };
        let line = line.context("Failed to read from stdin")?;

        run(&line)?;
    }
    Ok(())
}

fn run(source: &str) -> anyhow::Result<()> {
    let tokenizer = Tokenizer::new(source);
    let tokens = tokenizer.into_tokens().context("Lexing error")?;

    // TODO For now, just print the tokens.
    for token in tokens {
        println!("{token:?}");
    }
    Ok(())
}
