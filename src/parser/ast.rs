use std::sync::Arc;

use super::span::Span;
use super::tokenizer::TokenKind;
use super::{
    INFIX_AND_BINDING_POWER, INFIX_COMPARISON_BINDING_POWER, INFIX_DOT_BINDING_POWER,
    INFIX_EQUALITY_BINDING_POWER, INFIX_OR_BINDING_POWER, INFIX_PRODUCT_BINDING_POWER,
    INFIX_SUM_BINDING_POWER, PREFIX_MINUS_NOT_BINDING_POWER,
};

pub type Identifier = Arc<str>;
pub type LValue = Identifier;
pub type Bp = u8;

#[derive(Clone, Debug, PartialEq)]
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
            | Statement::If(..)
            | Statement::While { .. }
            | Statement::Block(_)
            | Statement::FunctionDeclaration(..)
            | Statement::Return(_)) => statement,
        };
        Self {
            statement,
            span: self.span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Expression(ExpressionNode),
    Print(ExpressionNode),
    Declaration(Identifier, Option<ExpressionNode>),
    If(
        ExpressionNode,
        Box<StatementNode>,
        Option<Box<StatementNode>>,
    ),
    While {
        condition: ExpressionNode,
        body: Box<StatementNode>,
    },
    Block(Vec<StatementNode>),
    FunctionDeclaration(Identifier, Vec<Identifier>, Box<StatementNode>),
    Return(ExpressionNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionNode {
    pub expression: Expression,
    pub span: Span,
}

impl ExpressionNode {
    pub fn host<V: ExpressionVisitor>(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atom(Atom),
    Prefix(Prefix, Box<ExpressionNode>),
    Binary(Binary, Box<[ExpressionNode; 2]>),
    Parenthesized(Box<ExpressionNode>),
    Assignment(LValue, Box<ExpressionNode>),
    Call(Box<ExpressionNode>, Vec<ExpressionNode>),
}

impl Expression {
    pub fn lvalue(&self) -> Option<LValue> {
        match &self {
            Self::Atom(Atom::Identifier(identifier)) => Some(Identifier::clone(identifier)),
            Self::Binary(Binary::Field, ..) => todo!(),
            Self::Atom(_)
            | Self::Prefix(..)
            | Self::Binary(..)
            | Self::Parenthesized(_)
            | Self::Assignment(..)
            | Self::Call(..) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Literal(Literal),
    Identifier(Identifier),
}

impl Atom {
    pub fn from_token(kind: TokenKind) -> Option<Self> {
        Literal::from_token(kind).map(Self::Literal).or_else(|| {
            if let TokenKind::Identifier(identifier) = kind {
                Some(Self::Identifier(identifier.into()))
            } else {
                None
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Literal {
    #[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
    pub(crate) fn from_token(token: TokenKind<'_>) -> Option<Self> {
        let literal = match token {
            TokenKind::Number(number) => Self::Number(number),
            TokenKind::False => Self::Boolean(false),
            TokenKind::True => Self::Boolean(true),
            TokenKind::Nil => Self::Nil,
            TokenKind::String(string) => Self::string(string),
            _ => return None,
        };
        Some(literal)
    }

    pub(crate) fn string(escaped: &str) -> Self {
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
        Self::String(string)
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

    #[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
    pub const fn from_token(token: TokenKind) -> Option<Self> {
        let op = match token {
            TokenKind::Minus => Self::Minus,
            TokenKind::Bang => Self::Not,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Binary {
    Field,
    Arithmetic(Arithmetic),
    Comparison(Comparison),
    Equality(Equality),
    Logical(Logical),
}

impl Binary {
    pub const fn binding_power(self) -> (u8, u8) {
        match self {
            Self::Field => INFIX_DOT_BINDING_POWER,
            Self::Arithmetic(op) => op.binding_power(),
            Self::Comparison(_) => INFIX_COMPARISON_BINDING_POWER,
            Self::Equality(_) => INFIX_EQUALITY_BINDING_POWER,
            Self::Logical(op) => op.binding_power(),
        }
    }

    #[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
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
            TokenKind::Dot => Self::Field,
            _ => return None,
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
}

impl Logical {
    const fn binding_power(self) -> (u8, u8) {
        match self {
            Self::And => INFIX_AND_BINDING_POWER,
            Self::Or => INFIX_OR_BINDING_POWER,
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
    fn visit_call(
        &mut self,
        callee: &ExpressionNode,
        args: &[ExpressionNode],
    ) -> Result<Self::Output, Self::Error>;

    fn visit(&mut self, node: &ExpressionNode) -> Result<Self::Output, Self::Error> {
        match &node.expression {
            Expression::Atom(atom) => self.visit_atom(atom, node.span),
            Expression::Prefix(op, arg) => self.visit_prefix(*op, arg),
            Expression::Binary(op, args) => self.visit_binary(*op, args),
            Expression::Assignment(left, right) => self.visit_assignment(left, right),
            Expression::Parenthesized(inner) => self.visit_parenthesized(inner),
            Expression::Call(callee, args) => self.visit_call(callee, args),
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
    fn visit_if(
        &mut self,
        condition: &ExpressionNode,
        then_body: &StatementNode,
        else_body: Option<&StatementNode>,
    ) -> Result<Self::Output, Self::Error>;
    fn visit_while(
        &mut self,
        condition: &ExpressionNode,
        body: &StatementNode,
    ) -> Result<Self::Output, Self::Error>;
    fn visit_function_declaration(
        &mut self,
        name: &Identifier,
        args: &[Identifier],
        body: &StatementNode,
    ) -> Result<Self::Output, Self::Error>;
    fn visit_return(&mut self, expression: &ExpressionNode) -> Result<Self::Output, Self::Error>;

    fn visit(&mut self, node: &StatementNode) -> Result<Self::Output, Self::Error> {
        match &node.statement {
            Statement::Expression(expression) => self.visit_expression(expression),
            Statement::Print(expression) => self.visit_print(expression),
            Statement::Declaration(identifier, initializer) => {
                self.visit_declaration(identifier, initializer.as_ref())
            }
            Statement::If(condition, then_body, else_body) => {
                self.visit_if(condition, then_body, else_body.as_ref().map(Box::as_ref))
            }
            Statement::While { condition, body } => self.visit_while(condition, body),
            Statement::Block(statements) => self.visit_block(statements),
            Statement::FunctionDeclaration(name, args, body) => {
                self.visit_function_declaration(name, args, body)
            }
            Statement::Return(expression) => self.visit_return(expression),
        }
    }
}
