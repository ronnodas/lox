use std::sync::Arc;

use thiserror::Error;

use super::span::Span;
use super::tokenizer::TokenKind;
use super::{
    INFIX_AND_BINDING_POWER, INFIX_COMPARISON_BINDING_POWER, INFIX_EQUALITY_BINDING_POWER,
    INFIX_OR_BINDING_POWER, INFIX_PRODUCT_BINDING_POWER, INFIX_SUM_BINDING_POWER,
    PREFIX_MINUS_NOT_BINDING_POWER,
};

pub type Identifier = Arc<str>;
pub type Str = Arc<str>;
pub type LValue = Identifier;
pub type Bp = u8;

pub struct StatementNode {
    pub statement: Statement,
    pub span: Span,
}

impl StatementNode {
    pub fn host<V: StatementVisitor>(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit(self)
    }

    pub fn implicit_print(self) -> Self {
        let statement = match self.statement {
            Statement::Expression(expression_node) => Statement::Print(expression_node),
            statement @ (Statement::Print(_)
            | Statement::Declaration(..)
            | Statement::While { .. }
            | Statement::Block(_)) => statement,
        };
        Self {
            statement,
            span: self.span,
        }
    }
}

pub enum Statement {
    Expression(ExpressionNode),
    Print(ExpressionNode),
    Declaration(Identifier, Option<ExpressionNode>),
    While {
        condition: ExpressionNode,
        body: Box<StatementNode>,
    },
    Block(Vec<StatementNode>),
}

#[derive(Debug)]
pub struct ExpressionNode {
    pub expression: Expression,
    pub span: Span,
}

impl ExpressionNode {
    pub fn host<V: ExpressionVisitor>(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit(self)
    }
}

#[derive(Debug)]
pub enum Expression {
    Atom(Atom),
    Prefix(Prefix, Box<ExpressionNode>),
    Binary(Binary, Box<[ExpressionNode; 2]>),
    Parenthesized(Box<ExpressionNode>),
    Assignment(LValue, Box<ExpressionNode>),
}

impl Expression {
    pub fn lvalue(&self) -> Option<LValue> {
        match &self {
            Self::Atom(Atom::Identifier(identifier)) => Some(Identifier::clone(identifier)),
            Self::Atom(_)
            | Self::Prefix(..)
            | Self::Binary(..)
            | Self::Parenthesized(_)
            | Self::Assignment(..) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Value(Value),
    Identifier(Identifier),
}
impl Atom {
    pub fn from_token(kind: TokenKind) -> Option<Self> {
        Value::from_token(kind).map(Self::Value).or_else(|| {
            if let TokenKind::Identifier(identifier) = kind {
                Some(Self::Identifier(identifier.into()))
            } else {
                None
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    String(Str),
    Boolean(bool),
    Nil,
}

impl Value {
    fn from_token(token: TokenKind) -> Option<Self> {
        let value = match token {
            TokenKind::Number(number) => Self::Number(number),
            TokenKind::False => Self::Boolean(false),
            TokenKind::True => Self::Boolean(true),
            TokenKind::Nil => Self::Nil,
            TokenKind::String(string) => Self::string(string),
            TokenKind::LeftParen
            | TokenKind::RightParen
            | TokenKind::LeftBrace
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
            | TokenKind::And
            | TokenKind::Class
            | TokenKind::Else
            | TokenKind::For
            | TokenKind::Fun
            | TokenKind::If
            | TokenKind::Or
            | TokenKind::Print
            | TokenKind::Return
            | TokenKind::Super
            | TokenKind::This
            | TokenKind::Var
            | TokenKind::While => return None,
        };
        Some(value)
    }

    fn string(escaped: &str) -> Self {
        let mut seen_slash = false;
        let string: String = escaped
            .chars()
            .filter_map(|c| match c {
                c if seen_slash => {
                    seen_slash = false;
                    Some(c)
                }
                '\\' => {
                    seen_slash = true;
                    None
                }
                c => Some(c),
            })
            .collect();

        Self::String(string.into())
    }

    pub const fn float(&self) -> Option<f64> {
        match self {
            &Self::Number(number) => Some(number),
            &Self::Boolean(boolean) => Some(if boolean { 1.0 } else { 0.0 }),
            Self::String(_) | Self::Nil => None,
        }
    }

    pub const fn is_truthy(&self) -> bool {
        match self {
            &Self::Boolean(boolean) => boolean,
            Self::Nil => false,
            Self::Number(_) | Self::String(_) => true,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Prefix {
    Minus,
    Not,
}

impl Prefix {
    pub const fn right_binding_power(self) -> u8 {
        match self {
            Self::Minus | Self::Not => PREFIX_MINUS_NOT_BINDING_POWER,
        }
    }

    pub const fn from_token(token: TokenKind) -> Option<Self> {
        let op = match token {
            TokenKind::Minus => Self::Minus,
            TokenKind::Bang => Self::Not,

            TokenKind::LeftParen
            | TokenKind::RightParen
            | TokenKind::LeftBrace
            | TokenKind::RightBrace
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::Plus
            | TokenKind::Semicolon
            | TokenKind::Slash
            | TokenKind::Star
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
            | TokenKind::For
            | TokenKind::Fun
            | TokenKind::If
            | TokenKind::Nil
            | TokenKind::Or
            | TokenKind::Print
            | TokenKind::Return
            | TokenKind::Super
            | TokenKind::This
            | TokenKind::True
            | TokenKind::Var
            | TokenKind::While => return None,
        };
        Some(op)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Binary {
    Arithmetic(Arithmetic),
    Comparison(Comparison),
    Equality(Equality),
    Logical(Logical),
}

impl Binary {
    pub fn evaluate(self, left: Value, right: Value) -> Result<Value, TypeError> {
        match self {
            Self::Arithmetic(op) => op.evaluate(left, right),
            Self::Comparison(rel) => rel.evaluate(left, right).map(Value::Boolean),
            Self::Equality(rel) => Ok(Value::Boolean(rel.evaluate(&left, &right))),
            Self::Logical(op) => Ok(op.evaluate(left, right)),
        }
    }

    pub const fn binding_power(self) -> (u8, u8) {
        match self {
            Self::Arithmetic(op) => op.binding_power(),
            Self::Comparison(_) => INFIX_COMPARISON_BINDING_POWER,
            Self::Equality(_) => INFIX_EQUALITY_BINDING_POWER,
            Self::Logical(op) => op.binding_power(),
        }
    }

    pub const fn from_token(token: TokenKind) -> Option<Self> {
        let op = match token {
            TokenKind::Plus => Self::Arithmetic(Arithmetic::Plus),
            TokenKind::Minus => Self::Arithmetic(Arithmetic::Minus),
            TokenKind::Star => Self::Arithmetic(Arithmetic::Times),
            TokenKind::Slash => Self::Arithmetic(Arithmetic::Divide),
            TokenKind::Less => Self::Comparison(Comparison::Less),
            TokenKind::LessEqual => Self::Comparison(Comparison::LessEqual),
            TokenKind::Greater => Self::Comparison(Comparison::Greater),
            TokenKind::GreaterEqual => Self::Comparison(Comparison::GreaterEqual),
            TokenKind::BangEqual => Self::Equality(Equality::NotEqual),
            TokenKind::EqualEqual => Self::Equality(Equality::Equal),
            TokenKind::And => Self::Logical(Logical::And),
            TokenKind::Or => Self::Logical(Logical::Or),
            TokenKind::LeftParen
            | TokenKind::RightParen
            | TokenKind::LeftBrace
            | TokenKind::RightBrace
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::Semicolon
            | TokenKind::Bang
            | TokenKind::Equal
            | TokenKind::Identifier(_)
            | TokenKind::String(_)
            | TokenKind::Number(_)
            | TokenKind::Class
            | TokenKind::Else
            | TokenKind::False
            | TokenKind::For
            | TokenKind::Fun
            | TokenKind::If
            | TokenKind::Nil
            | TokenKind::Print
            | TokenKind::Return
            | TokenKind::Super
            | TokenKind::This
            | TokenKind::True
            | TokenKind::Var
            | TokenKind::While => return None,
        };

        Some(op)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arithmetic {
    Plus,
    Minus,
    Times,
    Divide,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison {
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Equality {
    Equal,
    NotEqual,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Logical {
    And,
    Or,
}

impl Arithmetic {
    const fn binding_power(self) -> (u8, u8) {
        match self {
            Self::Plus | Self::Minus => INFIX_SUM_BINDING_POWER,
            Self::Times | Self::Divide => INFIX_PRODUCT_BINDING_POWER,
        }
    }

    pub fn evaluate(self, left: Value, right: Value) -> Result<Value, TypeError> {
        if self == Self::Plus {
            match (&left, &right) {
                (Value::String(left), Value::String(right)) => {
                    return Ok(Value::String((left.as_ref().to_owned() + right).into()))
                }
                (Value::String(_), _) => return Err(TypeError::SumString(right)),
                (_, Value::String(_)) => return Err(TypeError::SumString(left)),
                _ => (),
            }
        }

        let left = self.cast(left)?;
        let right = self.cast(right)?;

        let value = match self {
            Self::Plus => left + right,
            Self::Minus => left - right,
            Self::Times => left * right,
            Self::Divide => left / right,
        };

        Ok(Value::Number(value))
    }

    fn cast(self, value: Value) -> Result<f64, TypeError> {
        value.float().ok_or(TypeError::Arithmetic(self, value))
    }
}

impl Logical {
    const fn binding_power(self) -> (u8, u8) {
        match self {
            Self::And => INFIX_AND_BINDING_POWER,
            Self::Or => INFIX_OR_BINDING_POWER,
        }
    }

    pub fn evaluate(self, left: Value, right: Value) -> Value {
        match (self, left.is_truthy()) {
            (Self::And, false) | (Self::Or, true) => left,
            (Self::And, true) | (Self::Or, false) => right,
        }
    }

    pub const fn short_circuit(self, left: &Value) -> bool {
        match self {
            Self::And => !left.is_truthy(),
            Self::Or => left.is_truthy(),
        }
    }
}

impl Comparison {
    pub fn evaluate(self, left: Value, right: Value) -> Result<bool, TypeError> {
        let left = self.cast(left)?;
        let right = self.cast(right)?;

        let value = match self {
            Self::Greater => left > right,
            Self::GreaterEqual => left >= right,
            Self::Less => left < right,
            Self::LessEqual => left <= right,
        };
        Ok(value)
    }

    fn cast(self, value: Value) -> Result<f64, TypeError> {
        value.float().ok_or(TypeError::Comparison(self, value))
    }
}

impl Equality {
    pub fn evaluate(self, left: &Value, right: &Value) -> bool {
        match self {
            Self::Equal => left == right,
            Self::NotEqual => left != right,
        }
    }
}

pub trait ExpressionVisitor {
    type Output;
    type Error;

    fn visit_atom(&mut self, atom: &Atom, span: Span) -> Result<Self::Output, Self::Error>;
    fn visit_prefix(
        &mut self,
        op: Prefix,
        arg: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error>;
    fn visit_binary(
        &mut self,
        op: Binary,
        args: &[ExpressionNode; 2],
    ) -> Result<Self::Output, Self::Error>;
    fn visit_assignment(
        &mut self,
        left: &LValue,
        right: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error>;

    fn visit(&mut self, node: &ExpressionNode) -> Result<Self::Output, Self::Error> {
        match &node.expression {
            Expression::Atom(atom) => self.visit_atom(atom, node.span),
            Expression::Prefix(op, arg) => self.visit_prefix(*op, arg),
            Expression::Binary(op, args) => self.visit_binary(*op, args),
            Expression::Assignment(left, right) => self.visit_assignment(left, right),
            Expression::Parenthesized(inner) => self.visit_parenthesized(inner),
        }
    }

    fn visit_parenthesized(&mut self, node: &ExpressionNode) -> Result<Self::Output, Self::Error> {
        self.visit(node)
    }
}

pub trait StatementVisitor {
    type Output;
    type Error;

    fn visit_expression(
        &mut self,
        expression: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error>;
    fn visit_print(&mut self, expression: &ExpressionNode) -> Result<Self::Output, Self::Error>;
    fn visit_block(&mut self, items: &[StatementNode]) -> Result<Self::Output, Self::Error>;
    fn visit_declaration(
        &mut self,
        identifier: &Identifier,
        initializer: Option<&ExpressionNode>,
    ) -> Result<Self::Output, Self::Error>;
    fn visit_while(
        &mut self,
        condition: &ExpressionNode,
        body: &StatementNode,
    ) -> Result<Self::Output, Self::Error>;

    fn visit(&mut self, node: &StatementNode) -> Result<Self::Output, Self::Error> {
        match &node.statement {
            Statement::Expression(expression) => self.visit_expression(expression),
            Statement::Print(expression) => self.visit_print(expression),
            Statement::Declaration(identifier, initializer) => {
                self.visit_declaration(identifier, initializer.as_ref())
            }
            Statement::While { condition, body } => self.visit_while(condition, body),
            Statement::Block(statements) => self.visit_block(statements),
        }
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum TypeError {
    #[error("cannot compare {1} using {0}, not a number")]
    Comparison(Comparison, Value),
    #[error("cannot add {0} to a string")]
    SumString(Value),
    #[error("cannot apply {0} to {1}, not a number")]
    Arithmetic(Arithmetic, Value),
    #[error("cannot negate {0}")]
    UnaryMinus(Value),
}
