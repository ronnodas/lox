pub mod ast;
pub mod printer;
mod span;
mod tokenizer;

use std::iter::Peekable;

use itertools::Itertools;
use miette::Diagnostic;
use thiserror::Error;

use ast::{
    Atom, Binary, Bp, Expression, ExpressionNode, Identifier, Literal, Prefix, Statement,
    StatementNode,
};
pub use span::Span;
use tokenizer::{
    Error as TokenError, ErrorKind as TokenErrorKind, Token, TokenKind, TokenTag, Tokenizer,
};

const EXPRESSION_START: &str = "a new expression";
const STATEMENT_END: &str = "a ; or a }";

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

pub struct Parser<'t> {
    tokens: Peekable<Tokenizer<'t>>,
}

impl<'t> Parser<'t> {
    pub fn new(source: &'t str) -> Self {
        Self {
            tokens: Tokenizer::new(source).peekable(),
        }
    }

    #[expect(clippy::too_many_lines, reason = "will refactor once complete")]
    fn parse_statement(
        &mut self,
        break_on: &[TokenTag],
    ) -> Result<Option<StatementNode>, ParseError> {
        if self.matches_next(break_on) {
            return Ok(None);
        }
        let Token {
            kind: start,
            mut span,
        } = match self.tokens.peek() {
            Some(token) => token.as_ref()?,
            None => return Ok(None),
        };
        let Some(start) = StatementStart::new(start) else {
            let expression = self.parse_expression(0, break_on)?;
            let Some(expression) = expression else {
                _ = self.expect(TokenTag::Semicolon)?;
                return Ok(None);
            };
            let span = expression.span;
            return Ok(Some(StatementNode {
                statement: Statement::Expression(expression),
                span,
            }));
        };
        _ = self.tokens.next();
        match start {
            StatementStart::Block => {
                let mut items = Vec::new();
                loop {
                    if let Some(node) =
                        self.parse_statement(&[TokenTag::Semicolon, TokenTag::RightBrace])?
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
                Ok(Some(StatementNode {
                    statement: Statement::Block(items),
                    span,
                }))
            }
            StatementStart::Prefix(marker) => {
                let one = self.parse_expression(0, &[])?;
                marker.form(one, span).map(Some)
            }
            StatementStart::For => {
                _ = self.expect(TokenTag::LeftParen)?;
                let initialization = self.parse_statement(&[TokenTag::Semicolon])?;
                _ = self.expect(TokenTag::Semicolon)?;
                let condition = self.parse_expression(0, &[TokenTag::Semicolon])?;
                _ = self.expect(TokenTag::Semicolon)?;
                let increment = self.parse_expression(0, &[TokenTag::RightParen])?;
                _ = self.expect(TokenTag::RightParen)?;
                let body = self.parse_statement(break_on)?.ok_or_else(|| {
                    ErrorKind::SyntaxError("for loop must have a body").with_span(span)
                })?;
                span |= body.span;
                Ok(Some(StatementNode {
                    statement: Self::desugar_for(initialization, condition, increment, body)?,
                    span,
                }))
            }
            StatementStart::While => {
                _ = self.expect(TokenTag::LeftParen)?;
                let condition = self
                    .parse_expression(0, &[TokenTag::RightParen])?
                    .ok_or_else(|| {
                        ErrorKind::SyntaxError("while loop must have non-empty condition")
                            .with_span(span)
                    })?;
                _ = self.expect(TokenTag::RightParen)?;
                let body = Box::new(self.parse_statement(break_on)?.ok_or_else(|| {
                    ErrorKind::SyntaxError("while loop must have a body").with_span(span)
                })?);
                span |= body.span;
                Ok(Some(StatementNode {
                    statement: Statement::While { condition, body },
                    span,
                }))
            }
            StatementStart::Fun => {
                let name = match self.parse_expression(0, &[TokenTag::LeftParen])? {
                    Some(ExpressionNode {
                        expression: Expression::Atom(Atom::Identifier(name)),
                        ..
                    }) => name,
                    expression => {
                        let span = expression.map_or(span, |expr| expr.span);
                        return Err(
                            ErrorKind::SyntaxError("function name must be an identifier")
                                .with_span(span)
                                .into(),
                        );
                    }
                };
                _ = self.expect(TokenTag::LeftParen)?;
                let (params, _) = self.parse_args(span)?;
                #[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
                let params: Vec<Identifier> = params
                    .into_iter()
                    .map(|expression| match expression.expression {
                        Expression::Atom(Atom::Identifier(name)) => Ok(name),
                        _ => Err(
                            ErrorKind::SyntaxError("function parameters must be identifiers")
                                .with_span(expression.span),
                        ),
                    })
                    .try_collect()?;
                let body = Box::new(self.parse_statement(break_on)?.ok_or_else(|| {
                    ErrorKind::SyntaxError("function must have a body").with_span(span)
                })?);
                span |= body.span;
                Ok(Some(StatementNode {
                    statement: Statement::FunctionDeclaration(name, params, body),
                    span,
                }))
            }
            StatementStart::If => {
                _ = self.expect(TokenTag::LeftParen)?;
                let condition = self
                    .parse_expression(0, &[TokenTag::RightParen])?
                    .ok_or_else(|| {
                        ErrorKind::SyntaxError("if statement must have non-empty condition")
                            .with_span(span)
                    })?;
                _ = self.expect(TokenTag::RightParen)?;
                let then_body = Box::new(self.parse_statement(break_on)?.ok_or_else(|| {
                    ErrorKind::SyntaxError("while loop must have a body").with_span(span)
                })?);
                span |= then_body.span;
                let else_body = if self.expect(TokenTag::Else).is_ok() {
                    let else_body = self.parse_statement(break_on)?.ok_or_else(|| {
                        ErrorKind::SyntaxError("while loop must have a body").with_span(span)
                    })?;
                    span |= else_body.span;
                    Some(Box::new(else_body))
                } else {
                    None
                };
                Ok(Some(StatementNode {
                    statement: Statement::If(condition, then_body, else_body),
                    span,
                }))
            }
        }
    }

    // #[expect(clippy::too_many_lines, reason = "will refactor once complete")]
    fn parse_expression(
        &mut self,
        binding_power: Bp,
        break_on: &[TokenTag],
    ) -> Result<Option<ExpressionNode>, ParseError> {
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
        let Some(start) = ExpressionStart::new(start) else {
            return Err(WithSpan::new(ErrorKind::UnexpectedToken(EXPRESSION_START), span).into());
        };
        match start {
            ExpressionStart::Atom(atom) => {
                let mut expression = Expression::Atom(atom);
                while let Some(Ok(op)) = self.tokens.peek() {
                    if break_on.contains(&op.kind.into()) {
                        break;
                    }
                    let Some((op, left_bp, right_bp)) = Infix::new(op.kind) else {
                        break;
                    };
                    if left_bp <= binding_power {
                        break;
                    }
                    let right_span = self
                        .tokens
                        .next()
                        .transpose()?
                        .unwrap_or_else(|| unreachable!())
                        .span;
                    if op == Infix::Call {
                        let (args, right_span) = self.parse_args(right_span)?;
                        expression =
                            Expression::Call(Box::new(ExpressionNode { expression, span }), args);
                        span |= right_span;
                    } else {
                        let Some(right) = self.parse_expression(right_bp, break_on)? else {
                            return Err(ParseError::UnexpectedEndOfInput(EXPRESSION_START));
                        };
                        expression = match op {
                            Infix::Op(op) => Expression::Binary(
                                op,
                                Box::new([ExpressionNode { expression, span }, right]),
                            ),
                            Infix::Assignment => {
                                let lvalue = expression
                                    .lvalue()
                                    .ok_or_else(|| ErrorKind::InvalidLValue.with_span(span))?;
                                Expression::Assignment(lvalue, Box::new(right))
                            }
                            Infix::Call => unreachable!("outer if-else"),
                        };
                        span |= right_span;
                    }
                }
                //TODO handle postfix
                Ok(Some(ExpressionNode { expression, span }))
            }
            ExpressionStart::Prefix(op) => match self
                .parse_expression(op.right_binding_power(), break_on)?
            {
                Some(right) => {
                    let right_span = right.span;
                    Ok(Some(ExpressionNode {
                        expression: Expression::Prefix(op, Box::new(right)),
                        span: span | right_span,
                    }))
                }
                _ => Err(WithSpan::new(ErrorKind::UnexpectedToken(EXPRESSION_START), span).into()),
            },
            ExpressionStart::Paren => {
                let one = self.parse_expression(0, &[(TokenTag::RightParen)])?;
                span |= self.expect(TokenTag::RightParen)?.span;

                let one = one.ok_or_else(|| {
                    ErrorKind::SyntaxError("empty parentheses are only allowed in a function call")
                        .with_span(span)
                })?;
                Ok(Some(ExpressionNode {
                    expression: Expression::Parenthesized(Box::new(one)),
                    span,
                }))
            }
        }
    }

    fn parse_args(&mut self, mut span: Span) -> Result<(Vec<ExpressionNode>, Span), ParseError> {
        let mut args = Vec::new();
        loop {
            if let Some(node) =
                self.parse_expression(0, &[TokenTag::Comma, TokenTag::RightParen])?
            {
                args.push(node);
            };
            match self.tokens.peek().map(Result::as_ref).transpose()? {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {
                    _ = self.tokens.next();
                }
                Some(
                    paren @ Token {
                        kind: TokenKind::RightParen,
                        ..
                    },
                ) => {
                    span |= paren.span;
                    _ = self.tokens.next();
                    break;
                }
                Some(token) => {
                    return Err(ErrorKind::UnexpectedToken("a , or a )")
                        .with_span(token.span)
                        .into())
                }
                None => {
                    return Err(ParseError::UnexpectedEndOfInput("a , or a )"));
                }
            }
        }
        Ok((args, span))
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

    fn desugar_for(
        initialization: Option<StatementNode>,
        condition: Option<ExpressionNode>,
        increment: Option<ExpressionNode>,
        body: StatementNode,
    ) -> Result<Statement, ParseError> {
        let body = match increment {
            Some(increment) => {
                let span = increment.span;
                let increment = StatementNode {
                    statement: Statement::Expression(increment),
                    span,
                };
                let span = body.span | increment.span;
                StatementNode {
                    statement: Statement::Block(vec![body, increment]),
                    span,
                }
            }
            None => body,
        };
        let condition = condition.unwrap_or(ExpressionNode {
            expression: Expression::Atom(Atom::Literal(Literal::Boolean(true))),
            span: (0, 0).into(),
        });
        let span = body.span | condition.span;
        let while_statement = StatementNode {
            statement: Statement::While {
                condition,
                body: Box::new(body),
            },
            span,
        };
        Ok(match initialization {
            Some(
                initialization @ StatementNode {
                    statement:
                        Statement::Declaration(..)
                        | Statement::Expression(ExpressionNode {
                            expression: Expression::Assignment(..),
                            ..
                        }),
                    ..
                },
            ) => Statement::Block(vec![initialization, while_statement]),
            Some(initialization) => {
                return Err(ErrorKind::SyntaxError(
                    "initializer in for loop must be either a declaration or assignment",
                )
                .with_span(initialization.span)
                .into())
            }
            None => while_statement.statement,
        })
    }
}

impl<'t> Iterator for Parser<'t> {
    type Item = Result<StatementNode, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO return spans for better errors
        let node = self.parse_statement(&[]).transpose()?;
        _ = self.expect(TokenTag::Semicolon);
        Some(node)
    }
}

#[derive(Clone, Debug, PartialEq)]
enum StatementStart {
    Prefix(ExpressionPrefix),
    If,
    While,
    For,
    Block,
    Fun,
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl StatementStart {
    pub const fn new(kind: &TokenKind) -> Option<Self> {
        match kind {
            TokenKind::LeftBrace => Some(Self::Block),
            TokenKind::Var => Some(Self::Prefix(ExpressionPrefix::Var)),
            TokenKind::Print => Some(Self::Prefix(ExpressionPrefix::Print)),
            TokenKind::Return => Some(Self::Prefix(ExpressionPrefix::Return)),
            TokenKind::If => Some(Self::If),
            TokenKind::While => Some(Self::While),
            TokenKind::For => Some(Self::For),
            TokenKind::Fun => Some(Self::Fun),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ExpressionPrefix {
    Var,
    Print,
    Return,
}

impl ExpressionPrefix {
    fn form(self, arg: Option<ExpressionNode>, span: Span) -> Result<StatementNode, ParseError> {
        match self {
            Self::Var => {
                let arg = arg.ok_or_else(|| {
                    ErrorKind::SyntaxError("var should be followed by an identifier")
                        .with_span(span)
                })?;

                let statement = match arg.expression {
                    Expression::Assignment(left, right) => {
                        Statement::Declaration(left, Some(*right))
                    }
                    Expression::Atom(Atom::Identifier(identifier)) => {
                        Statement::Declaration(identifier, None)
                    }
                    Expression::Atom(_)
                    | Expression::Prefix(..)
                    | Expression::Binary(..)
                    | Expression::Parenthesized(_)
                    | Expression::Call(..) => {
                        return Err(ErrorKind::SyntaxError(
                            "`var` must be followed by a variable or assignment",
                        )
                        .with_span(span)
                        .into())
                    }
                };
                Ok(StatementNode { statement, span })
            }
            Self::Print => {
                let arg = arg.ok_or_else(|| {
                    ErrorKind::SyntaxError("print requires an argument").with_span(span)
                })?;
                Ok(StatementNode {
                    statement: Statement::Print(arg),
                    span,
                })
            }
            Self::Return => {
                let arg = arg.unwrap_or(ExpressionNode {
                    expression: Expression::Atom(Atom::Literal(Literal::Nil)),
                    span,
                });
                Ok(StatementNode {
                    statement: Statement::Return(arg),
                    span,
                })
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ExpressionStart {
    Atom(Atom),
    Prefix(Prefix),
    Paren,
}

impl ExpressionStart {
    pub fn new(kind: TokenKind) -> Option<Self> {
        Atom::from_token(kind)
            .map(Self::Atom)
            .or_else(|| Prefix::from_token(kind).map(Self::Prefix))
            .or_else(|| (kind == TokenKind::LeftParen).then_some(Self::Paren))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Infix {
    Op(Binary),
    Assignment,
    Call,
}

impl Infix {
    #[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
    pub fn new(token: TokenKind) -> Option<(Self, Bp, Bp)> {
        Binary::from_token(token).map_or_else(
            || match token {
                TokenKind::Equal => {
                    let (left, right) = INFIX_ASSIGNMENT_BINDING_POWER;
                    Some((Self::Assignment, left, right))
                }
                TokenKind::LeftParen => {
                    let (left, right) = INFIX_CALL_BINDING_POWER;
                    Some((Self::Call, left, right))
                }
                _ => None,
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

#[derive(Clone, Debug, Diagnostic, Eq, Error, PartialEq)]
pub enum ErrorKind {
    #[error(transparent)]
    TokenizingError(#[from] TokenErrorKind),
    #[error("Unexpected token while parsing, expecting {0}")]
    UnexpectedToken(&'static str),
    #[error("Syntax error: {0}")]
    SyntaxError(&'static str),
    #[error("Invalid l-value")]
    InvalidLValue,
}

impl ErrorKind {
    const fn label(&self) -> &'static str {
        match self {
            Self::UnexpectedToken(_) => "this token",
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

    use super::*;

    fn test_expression_parse(source: &str) -> miette::Result<()> {
        let expression = Parser::new(source)
            .parse_expression(0, &[])
            .map_err(|e| miette::Report::from(e).with_source_code(source.to_owned()))?
            .unwrap();
        assert_eq!(source, expression.to_string(), "{expression:?}");
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
            if let Err(e) = test_expression_parse(source) {
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
            if let Err(e) = test_expression_parse(source) {
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
            if let Err(e) = test_expression_parse(source) {
                eprintln!("failed to parse '{source}': {e:?}");
                panic!();
            }
        }
    }
}
