pub mod ast;
pub mod printer;
mod span;
mod tokenizer;

use std::iter::Peekable;

use miette::Diagnostic;
use thiserror::Error;

use ast::{Ast, Atom, Binary, Bp, Grouping, Node, Prefix, Value};
pub use span::Span;
use tokenizer::{
    Error as TokenError, ErrorKind as TokenErrorKind, Token, TokenKind, TokenTag, Tokenizer,
};

type AstResult = Result<Option<Node>, ParseError>;

const EXPRESSION_START: &str = "a new expression";
const STATEMENT_START: &str = "a new statement";
const STATEMENT_END: &str = "a ; or a }";
const IDENTIFIER: &str = "an identifier";

pub const INFIX_DOT_BINDING_POWER: (u8, u8) = (21, 22);
pub const INFIX_CALL_BINDING_POWER: (u8, u8) = (19, 20);
pub const PREFIX_MINUS_NOT_BINDING_POWER: u8 = 17;
pub const INFIX_PRODUCT_BINDING_POWER: (u8, u8) = (15, 16);
pub const INFIX_SUM_BINDING_POWER: (u8, u8) = (13, 14);
pub const INFIX_COMPARISON_BINDING_POWER: (u8, u8) = (11, 12);
pub const INFIX_EQUALITY_BINDING_POWER: (u8, u8) = (9, 10);
pub const INFIX_AND_BINDING_POWER: (u8, u8) = (7, 8);
pub const INFIX_OR_BINDING_POWER: (u8, u8) = (5, 6);
pub const INFIX_ASSIGNMENT_BINDING_POWER: (u8, u8) = (4, 3);
pub const PREFIX_PRINT_BINDING_POWER: u8 = 1;

pub struct Parser<'t> {
    source: &'t str,
    tokens: Peekable<Tokenizer<'t>>,
}

impl<'t> Parser<'t> {
    pub fn new(source: &'t str) -> Self {
        Self {
            source,
            tokens: Tokenizer::new(source).peekable(),
        }
    }

    #[expect(clippy::too_many_lines, reason = "will refactor once complete")]
    fn parse(&mut self, binding_power: Bp, break_on: &[TokenTag]) -> AstResult {
        if self.matches_next(break_on) {
            return Ok(None);
        }
        let Token {
            kind: start,
            mut span,
        } = match self.tokens.next() {
            Some(token) => token?,
            None => return Ok(None),
        };
        let Some(start) = Start::new(start) else {
            return Err(WithSpan::new(ErrorKind::UnexpectedToken(EXPRESSION_START), span).into());
        };
        match start {
            Start::Atom(atom) => {
                let mut node = Ast::Atom(atom);
                loop {
                    let Some(Ok(op)) = self.tokens.peek() else {
                        break;
                    };
                    let Some((op, left_bp, right_bp)) = Infix::new(op.kind) else {
                        break;
                    };
                    if left_bp <= binding_power {
                        break;
                    }
                    _ = self.tokens.next();
                    let Some(right) = self.parse(right_bp, break_on)? else {
                        return Err(ParseError::UnexpectedEndOfInput(EXPRESSION_START));
                    };
                    let right_span = right.span;
                    node = match op {
                        Infix::Op(op) => {
                            Ast::Binary(op, Box::new([Node { ast: node, span }, right]))
                        }
                        Infix::Assignment => {
                            let lvalue = node
                                .lvalue()
                                .ok_or_else(|| ErrorKind::InvalidLValue.with_span(span))?;
                            Ast::Assignment(lvalue, Box::new(right))
                        }
                    };
                    span |= right_span;
                }
                //TODO handle postfix
                Ok(Some(Node { ast: node, span }))
            }
            Start::Prefix(op) => match self.parse(op.right_binding_power(), break_on)? {
                Some(right) => {
                    let right_span = right.span;
                    Ok(Some(Node {
                        ast: Ast::Prefix(op, Box::new(right)),
                        span: span | right_span,
                    }))
                }
                _ => Err(WithSpan::new(ErrorKind::UnexpectedToken(EXPRESSION_START), span).into()),
            },
            Start::Group(Grouping::Brace) => {
                let mut items = Vec::new();
                loop {
                    dbg!(&items);
                    if let Some(node) =
                        self.parse(0, &[TokenTag::Semicolon, TokenTag::RightBrace])?
                    {
                        span |= node.span;
                        items.push(node);
                    }
                    match self.tokens.peek().map(Result::as_ref).transpose()? {
                        Some(Token {
                            kind: TokenKind::Semicolon,
                            ..
                        }) => {
                            _ = self.tokens.next();
                        }
                        Some(Token {
                            kind: TokenKind::RightBrace,
                            span: brace_span,
                        }) => {
                            span |= *brace_span;
                            _ = self.tokens.next();
                            break;
                        }
                        Some(_) => (),
                        None => return Err(ParseError::UnexpectedEndOfInput(STATEMENT_END)),
                    }
                }
                Ok(Some(Node {
                    ast: Ast::Group(Grouping::Brace, items),
                    span,
                }))
            }
            Start::Unary(marker) => {
                let (end, consume) = marker.terminator();
                let one = self.parse(0, end)?;
                if consume {
                    for &terminator in end {
                        span |= self.expect(terminator)?.span;
                    }
                }
                marker.form(one, span).map(Some)
            }
            Start::For => {
                _ = self.expect(TokenTag::LeftParen)?;
                let initialization = self.parse(0, &[TokenTag::Semicolon])?;
                _ = self.expect(TokenTag::Semicolon)?;
                let condition = Box::new(self.parse(0, &[TokenTag::Semicolon])?.unwrap_or(Node {
                    ast: Ast::Atom(Atom::Value(Value::Boolean(true))),
                    span: (0, 0).into(),
                }));
                _ = self.expect(TokenTag::Semicolon)?;
                let increment = self.parse(0, &[TokenTag::RightParen])?.map(Box::new);
                _ = self.expect(TokenTag::RightParen)?;
                let body = Box::new(self.parse(binding_power, break_on)?.ok_or_else(|| {
                    ErrorKind::SyntaxError("for loop must have a body").with_span(span)
                })?);
                span |= body.span;
                let for_node = Node {
                    ast: Ast::For {
                        condition,
                        increment,
                        body,
                    },
                    span,
                };
                let block = match initialization {
                    Some(initialization) => {
                        vec![initialization, for_node]
                    }
                    None => vec![for_node],
                };
                Ok(Some(Node {
                    ast: Ast::Group(Grouping::Brace, block),
                    span,
                }))
            }
            Start::While => {
                _ = self.expect(TokenTag::LeftParen)?;
                let condition =
                    Box::new(self.parse(0, &[TokenTag::Semicolon])?.ok_or_else(|| {
                        ErrorKind::SyntaxError("while loop must have non-empty condition")
                            .with_span(span)
                    })?);
                _ = self.expect(TokenTag::RightParen)?;
                let body = Box::new(self.parse(binding_power, break_on)?.ok_or_else(|| {
                    ErrorKind::SyntaxError("while loop must have a body").with_span(span)
                })?);
                span |= body.span;
                Ok(Some(Node {
                    ast: Ast::For {
                        condition,
                        increment: None,
                        body,
                    },
                    span,
                }))
            }
        }
    }

    fn matches_next(&mut self, tags: &[TokenTag]) -> bool {
        self.tokens
            .peek()
            .and_then(|result| result.as_ref().ok())
            .is_some_and(|token| tags.contains(&TokenTag::from(token.kind)))
    }

    fn expect(&mut self, terminator: TokenTag) -> Result<Token, ParseError> {
        match self.tokens.peek().map(Result::as_ref).transpose()? {
            Some(token) if TokenTag::from(token.kind) == terminator => {
                Ok(self.tokens.next().unwrap_or_else(|| unreachable!())?)
            }
            Some(token) => Err(ErrorKind::UnexpectedToken(terminator.description())
                .with_span(token.span)
                .into()),
            None => Err(ParseError::UnexpectedEndOfInput(terminator.description())),
        }
    }
}

impl<'t> Iterator for Parser<'t> {
    type Item = Result<Node, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO return spans for better errors
        let node = self.parse(0, &[]).transpose()?;
        _ = self.expect(TokenTag::Semicolon);
        Some(node)
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Start {
    Atom(Atom),
    Prefix(Prefix),
    Group(Grouping),
    Unary(FancyUnary),
    For,
    While,
}

impl Start {
    pub fn new(kind: TokenKind) -> Option<Self> {
        Atom::from_token(kind)
            .map(Self::Atom)
            .or_else(|| Prefix::from_token(kind).map(Self::Prefix))
            .or_else(|| match kind {
                TokenKind::LeftBrace => Some(Self::Group(Grouping::Brace)),
                TokenKind::Var => Some(Self::Unary(FancyUnary::Var)),
                TokenKind::LeftParen => Some(Self::Unary(FancyUnary::LeftParen)),
                TokenKind::For => Some(Self::For),
                TokenKind::While => Some(Self::While),
                TokenKind::RightParen
                | TokenKind::RightBrace
                | TokenKind::Comma
                | TokenKind::Dot
                | TokenKind::Minus
                | TokenKind::Plus
                | TokenKind::Semicolon
                | TokenKind::Slash
                | TokenKind::Star
                | TokenKind::Bang
                | TokenKind::BangEqual
                | TokenKind::Equal
                | TokenKind::EqualEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Identifier(_)
                | TokenKind::String(_)
                | TokenKind::Number(_)
                | TokenKind::And
                | TokenKind::Class
                | TokenKind::Else
                | TokenKind::False
                | TokenKind::Fun
                | TokenKind::If
                | TokenKind::Nil
                | TokenKind::Or
                | TokenKind::Print
                | TokenKind::Return
                | TokenKind::Super
                | TokenKind::This
                | TokenKind::True => None,
            })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum FancyUnary {
    Var,
    LeftParen,
}

impl FancyUnary {
    const fn terminator(&self) -> (&[TokenTag], bool) {
        match self {
            Self::Var => (&[TokenTag::Semicolon], false),
            Self::LeftParen => (&[TokenTag::RightParen], true),
        }
    }

    fn form(self, one: Option<Node>, span: Span) -> Result<Node, ParseError> {
        match self {
            Self::Var => {
                let one = one.ok_or_else(|| {
                    ErrorKind::SyntaxError("var should be followed by an identifier")
                        .with_span(span)
                })?;

                let ast = match one.ast.unparen() {
                    Ast::Assignment(left, right) => Ast::Declaration(left, Some(right)),
                    Ast::Atom(Atom::Identifier(identifier)) => Ast::Declaration(identifier, None),
                    Ast::Parenthesized(..) => unreachable!("called unparen()"),
                    _ => {
                        return Err(ErrorKind::SyntaxError(
                            "`var` must be followed by a variable or assignment",
                        )
                        .with_span(span)
                        .into())
                    }
                };
                let ast = ast;
                Ok(Node { ast, span })
            }
            Self::LeftParen => {
                let one = one.ok_or_else(|| {
                    ErrorKind::SyntaxError("empty parentheses are only allowed in a function call")
                        .with_span(span)
                })?;
                Ok(Node {
                    ast: Ast::Parenthesized(Box::new(one)),
                    span,
                })
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Infix {
    Op(Binary),
    Assignment,
}

impl Infix {
    pub fn new(token: TokenKind) -> Option<(Self, Bp, Bp)> {
        Binary::from_token(token).map_or_else(
            || {
                let (left, right) = INFIX_ASSIGNMENT_BINDING_POWER;
                (token == TokenKind::Equal).then_some((Self::Assignment, left, right))
            },
            |op| {
                let (left, right) = op.binding_power();
                Some((Self::Op(op), left, right))
            },
        )
    }
}

#[derive(Clone, Debug, Diagnostic, Eq, Error, PartialEq)]
pub enum ParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    WithSpan(#[from] WithSpan),
    // TODO support batching
    #[error("Reached end of input but expecting {0}")]
    UnexpectedEndOfInput(&'static str),
}

#[derive(Clone, Debug, Diagnostic, Eq, Error, PartialEq)]
#[error("{}", self.kind)]
#[diagnostic()]
pub struct WithSpan {
    kind: ErrorKind,
    #[label("{}", self.kind.label())]
    span: Span,
}

impl WithSpan {
    pub const fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

//TODO add expected token to UnexpectedToken, probably want to pull in strum
#[derive(Clone, Debug, Diagnostic, Eq, Error, PartialEq)]
pub enum ErrorKind {
    #[error(transparent)]
    TokenizingError(#[from] TokenErrorKind),
    #[error("Unexpected token while parsing, expecting {0}")]
    UnexpectedToken(&'static str),
    #[error("Unmatched parenthesis")]
    UnmatchedParen,
    #[error("Unterminated print statement")]
    UnterminatedPrint,
    #[error("Syntax error: {0}")]
    SyntaxError(&'static str),
    #[error("Invalid l-value")]
    InvalidLValue,
}

impl ErrorKind {
    const fn label(&self) -> &'static str {
        match self {
            Self::UnexpectedToken(_) => "this token",
            Self::UnmatchedParen => "this open parenthesis",
            Self::UnterminatedPrint => "print starts here",
            Self::InvalidLValue => "this expression cannot be assigned to",
            Self::SyntaxError(_) => "in this expression",
            Self::TokenizingError(_) => "",
        }
    }

    const fn with_span(self, span: Span) -> WithSpan {
        WithSpan::new(self, span)
    }
}

impl From<TokenError> for ParseError {
    fn from(error: TokenError) -> Self {
        WithSpan::new(error.kind.into(), error.span).into()
    }
}

impl From<&TokenError> for ParseError {
    fn from(error: &TokenError) -> Self {
        WithSpan::new(error.kind.clone().into(), error.span).into()
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools as _;

    use super::*;

    fn test_parse(source: &str) -> miette::Result<()> {
        let ast: Vec<Node> = Parser::new(source)
            .try_collect()
            .map_err(|e| miette::Report::from(e).with_source_code(source.to_owned()))?;
        assert_eq!(ast.len(), 1);
        let ast = ast.into_iter().next().unwrap();
        assert_eq!(source, ast.to_string(), "{ast:?}");
        Ok(())
    }

    #[test]
    fn literals() {
        for source in [
            "1",
            "1.23",
            "\"\"",
            "\"\\\"\"",
            "\"hello\"",
            "true",
            "false",
            "nil",
        ] {
            if let Err(e) = test_parse(source) {
                eprintln!("failed to parse '{source}': {e:?}");
                panic!();
            }
        }
    }

    #[test]
    fn plus_minus() {
        for source in [
            "1 + 2",
            "\"abc\" + \"def\"",
            "1 + 2 + 3 + 4",
            "1 - 2 + 3 - 4",
        ] {
            if let Err(e) = test_parse(source) {
                eprintln!("failed to parse '{source}': {e:?}");
                panic!();
            }
        }
    }

    #[test]
    fn times_divide() {
        for source in [
            "1 * 2",
            "\"abc\" * \"def\"",
            "1 * 2 * 3 / 4",
            "1 / 2 + 3 / 4",
        ] {
            if let Err(e) = test_parse(source) {
                eprintln!("failed to parse '{source}': {e:?}");
                panic!();
            }
        }
    }
}
