use core::fmt;
use std::error;
use std::rc::Rc;

pub type Identifier = Rc<str>;
pub type LValue = Identifier;

#[derive(Debug)]
pub enum Declaration {
    VariableDeclaration {
        identifier: Identifier,
        initializer: Option<Expression>,
    },
    Statement(Statement),
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Print(Expression),
    Block(Vec<Declaration>),
}

#[derive(Debug)]
pub enum Expression {
    Assignment(Assignment),
    Or(LogicalOr),
}

#[derive(Debug)]
pub struct Assignment {
    pub lvalue: LValue,
    pub expression: Box<Expression>,
}

// TODO: could make the two operands different types, is that useful anywhere?
#[derive(Debug)]
pub struct Fold<T, Op> {
    pub start: T,
    pub more: Vec<(Op, T)>,
}

pub type LogicalOr = Fold<LogicalAnd, OrOperator>;
pub type LogicalAnd = Fold<Equality, AndOperator>;
pub type Equality = Fold<Comparison, EqualityOperator>;
pub type Comparison = Fold<Sum, ComparisonOperator>;
pub type Sum = Fold<Factor, SumOperator>;
pub type Factor = Fold<Unary, FactorOperator>;

#[derive(Debug)]
pub enum Unary {
    Unary(UnaryOperator, Box<Self>),
    Primary(Primary),
}

#[derive(Debug)]
pub enum Primary {
    Literal(Value),
    Grouping(Box<Expression>),
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(Identifier),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub struct OrOperator;
#[derive(Debug, Clone, Copy)]
pub struct AndOperator;

#[derive(Clone, Copy, Debug)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOperator {
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl ComparisonOperator {
    pub fn cast(self, value: Value) -> Result<f64, TypeError> {
        value.float().ok_or(TypeError::Comparison(self, value))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SumOperator {
    Minus,
    Plus,
}

impl SumOperator {
    pub fn cast(self, lhs: &Value) -> Result<f64, TypeError> {
        lhs.float().ok_or_else(|| TypeError::Sum(self, lhs.clone()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FactorOperator {
    Divide,
    Multiply,
}

impl FactorOperator {
    pub fn cast(self, lhs: Value) -> Result<f64, TypeError> {
        lhs.float().ok_or(TypeError::Factor(self, lhs))
    }
}

impl From<Primary> for Unary {
    fn from(v: Primary) -> Self {
        Self::Primary(v)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOperator {
    Minus,
    Not,
}

impl UnaryOperator {
    pub(crate) fn evaluate(self, value: Value) -> Result<Value, TypeError> {
        match self {
            Self::Minus => {
                let value = value.float().ok_or(TypeError::UnaryMinus(value))?;
                Ok(Value::Number(-value))
            }
            Self::Not => Ok(Value::Boolean(!value.is_truthy())),
        }
    }
}

impl From<Value> for Primary {
    fn from(v: Value) -> Self {
        Self::Literal(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Number(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl Value {
    const fn float(&self) -> Option<f64> {
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

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Comparison(operator, value) => {
                write!(
                    f,
                    "cannot cast {value} to float to compare using {operator}"
                )
            }
            Self::Sum(operator, value) => {
                write!(f, "cannot cast {value} to float to evaluate {operator}")
            }
            Self::Factor(operator, value) => {
                write!(f, "cannot cast {value} to float to evaluate {operator}")
            }
            Self::UnaryMinus(value) => write!(f, "cannot cast {value} to float to negate"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    Comparison(ComparisonOperator, Value),
    Sum(SumOperator, Value),
    Factor(FactorOperator, Value),
    UnaryMinus(Value),
}

impl error::Error for TypeError {}

impl Primary {
    pub(crate) fn group(expression: Expression) -> Self {
        Self::Grouping(Box::new(expression))
    }
}

impl From<LogicalOr> for Expression {
    fn from(v: LogicalOr) -> Self {
        Self::Or(v)
    }
}

pub trait ExpressionVisitor {
    type Output;
    type Error;

    fn visit_expression(&mut self, expression: &Expression) -> Result<Self::Output, Self::Error>;
    fn visit_assignment(&mut self, assignment: &Assignment) -> Result<Self::Output, Self::Error>;
    fn visit_unary(&mut self, unary: &Unary) -> Result<Self::Output, Self::Error>;
    fn visit_primary(&mut self, primary: &Primary) -> Result<Self::Output, Self::Error>;
    fn visit_fold<T: ExpressionHost<Self>, Op: ExpressionVisitCombiner<Self, T>>(
        &mut self,
        fold: &Fold<T, Op>,
    ) -> Result<Self::Output, Self::Error> {
        let start = fold.start.host(self)?;
        fold.more
            .iter()
            .try_fold(start, |lhs, (op, rhs)| {
                op.combine(lhs, self, rhs).map(Op::Output::into)
            })
            .map_err(Op::Error::into)
    }
}

pub trait ExpressionHost<V: ExpressionVisitor + ?Sized> {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error>;
}

pub trait ExpressionVisitCombiner<V: ExpressionVisitor + ?Sized, T: ExpressionHost<V>> {
    type Output: Into<V::Output>;
    type Error: Into<V::Error>;

    fn combine(
        &self,
        lhs: V::Output,
        visitor: &mut V,
        rhs: &T,
    ) -> Result<Self::Output, Self::Error>;
}

pub trait StatementVisitor {
    type Output;
    type Error;

    fn visit_print(&mut self, print: &Expression) -> Result<Self::Output, Self::Error>;

    fn visit_expression_statement(
        &mut self,
        expression: &Expression,
    ) -> Result<Self::Output, Self::Error>;

    fn visit_variable_declaration(
        &mut self,
        identifier: &str,
        initializer: Option<&Expression>,
    ) -> Result<Self::Output, Self::Error>;

    fn visit_if(
        &mut self,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> Result<Self::Output, Self::Error>;

    fn visit_block(&mut self, block: &[Declaration]) -> Result<Self::Output, Self::Error>;

    fn visit_statement(&mut self, statement: &Statement) -> Result<Self::Output, Self::Error> {
        match statement {
            Statement::Expression(expression) => self.visit_expression_statement(expression),
            Statement::Print(expression) => self.visit_print(expression),
            Statement::Block(block) => self.visit_block(block),
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => self.visit_if(condition, then_branch, else_branch.as_deref()),
        }
    }

    fn visit_declaration(
        &mut self,
        declaration: &Declaration,
    ) -> Result<Self::Output, Self::Error> {
        match declaration {
            Declaration::VariableDeclaration {
                identifier,
                initializer,
            } => self.visit_variable_declaration(identifier, initializer.as_ref()),
            Declaration::Statement(statement) => self.visit_statement(statement),
        }
    }
}

pub trait StatementHost<V: StatementVisitor> {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error>;
}

impl<V: StatementVisitor> StatementHost<V> for Declaration {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_declaration(self)
    }
}

impl<V: StatementVisitor> StatementHost<V> for Statement {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_statement(self)
    }
}

impl<V: ExpressionVisitor> ExpressionHost<V> for Assignment {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_assignment(self)
    }
}

impl<V: ExpressionVisitor> ExpressionHost<V> for Expression {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_expression(self)
    }
}

impl<V: ExpressionVisitor> ExpressionHost<V> for Unary {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_unary(self)
    }
}

impl<V: ExpressionVisitor> ExpressionHost<V> for Primary {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_primary(self)
    }
}

impl<V: ExpressionVisitor, T: ExpressionHost<V>, Op: ExpressionVisitCombiner<V, T>>
    ExpressionHost<V> for Fold<T, Op>
{
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_fold(self)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(number) => {
                let string = number.to_string();
                if let Some(stripped) = string.strip_suffix(".0") {
                    write!(f, "{stripped}")
                } else {
                    write!(f, "{string}")
                }
            }
            Self::String(string) => write!(f, "\"{string}\""),
            Self::Boolean(boolean) => write!(f, "{boolean}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

pub trait IntoLValue {
    fn lvalue(&self) -> Option<LValue>;
}

impl IntoLValue for Expression {
    fn lvalue(&self) -> Option<LValue> {
        match self {
            Self::Assignment(_) => None,
            Self::Or(or) => or.lvalue(),
        }
    }
}

impl<T: IntoLValue, Op> IntoLValue for Fold<T, Op> {
    fn lvalue(&self) -> Option<LValue> {
        self.more.is_empty().then(|| self.start.lvalue()).flatten()
    }
}

impl IntoLValue for Unary {
    fn lvalue(&self) -> Option<LValue> {
        match self {
            Self::Unary(..) => None,
            Self::Primary(primary) => primary.lvalue(),
        }
    }
}

impl IntoLValue for Primary {
    fn lvalue(&self) -> Option<LValue> {
        match self {
            Self::Identifier(identifier) => Some(Identifier::clone(identifier)),
            Self::Grouping(expression) => expression.lvalue(),
            Self::Literal(_) => None,
        }
    }
}

impl fmt::Display for OrOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "or")
    }
}

impl fmt::Display for AndOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "and")
    }
}

impl fmt::Display for EqualityOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
        }
    }
}

impl fmt::Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
        }
    }
}

impl fmt::Display for SumOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
        }
    }
}

impl fmt::Display for FactorOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Divide => write!(f, "/"),
            Self::Multiply => write!(f, "*"),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}
