use std::fmt;

pub enum Expression<'a> {
    Binary {
        left: Box<Self>,
        operator: BinaryOperator,
        right: Box<Self>,
    },
    Grouping(Box<Self>),
    Literal(Literal<'a>),
    Unary {
        operator: UnaryOperator,
        right: Box<Self>,
    },
}

impl<'a> From<Literal<'a>> for Expression<'a> {
    fn from(v: Literal<'a>) -> Self {
        Self::Literal(v)
    }
}

pub enum BinaryOperator {
    Equals,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Times,
    Divide,
}

pub enum Literal<'a> {
    Number(f64),
    String(&'a str),
    Boolean(bool),
    Nil,
}

pub enum UnaryOperator {
    Minus,
    Not,
}

impl<'a> Expression<'a> {
    fn group(self) -> Self {
        Self::Grouping(Box::new(self))
    }
}

impl<'a> fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Binary {
                left,
                operator,
                right,
            } => write!(f, "({operator} {left} {right})"),
            Expression::Grouping(expression) => write!(f, "(group {expression})"),
            Expression::Literal(literal) => write!(f, "{literal}"),
            Expression::Unary { operator, right } => write!(f, "({operator} {right})"),
        }
    }
}

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(number) => write!(f, "{number}"),
            Self::String(string) => write!(f, "{string:?}"),
            Self::Boolean(boolean) => write!(f, "{boolean}"),
            Self::Nil => write!(f, "nil"),
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

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Equals => write!(f, "="),
            Self::NotEqual => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Times => write!(f, "*"),
            Self::Divide => write!(f, "/"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{BinaryOperator, Expression, Literal, UnaryOperator};

    #[test]
    fn display() {
        let expression = Expression::Binary {
            left: Expression::Unary {
                operator: UnaryOperator::Minus,
                right: Box::new(Literal::Number(123.0).into()),
            }
            .into(),
            operator: BinaryOperator::Times,
            right: Box::new(Expression::Literal(Literal::Number(45.67)).group()),
        };

        assert_eq!(format!("{expression}"), "(* (- 123) (group 45.67))");
    }
}
