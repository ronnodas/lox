use std::fmt::{self, Display};

use super::ast::{
    Assignment, ComparisonOperator, EqualityOperator, Expression, ExpressionHost,
    ExpressionVisitor, FactorOperator, Fold, Primary, SumOperator, Unary, UnaryOperator,
};

struct Printer<'a, 'f> {
    formatter: &'a mut fmt::Formatter<'f>,
}

impl<'a, 'f> ExpressionVisitor for Printer<'a, 'f> {
    type Output = ();
    type Error = fmt::Error;

    fn visit_expression(&mut self, expression: &Expression) -> fmt::Result {
        match expression {
            Expression::Assignment(assignment) => assignment.host(self),
            Expression::Equality(equality) => equality.fmt(self.formatter),
        }
    }

    fn visit_assignment(&mut self, assignment: &Assignment) -> fmt::Result {
        write!(self.formatter, "{} = ", assignment.lvalue)?;
        self.visit_expression(&assignment.expression)
    }

    fn visit_unary(&mut self, unary: &Unary) -> fmt::Result {
        match unary {
            Unary::Unary(operator, unary) => {
                write!(self.formatter, "{operator} {unary}")
            }
            Unary::Primary(primary) => primary.host(self),
        }
    }

    fn visit_primary(&mut self, primary: &Primary) -> fmt::Result {
        match primary {
            Primary::Literal(value) => write!(self.formatter, "{value}"),
            Primary::Grouping(expression) => expression.host(self),
            Primary::Identifier(identifier) => write!(self.formatter, "{identifier}"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.host(&mut (Printer { formatter: f }))
    }
}

impl<T: Display, Op: Display> Display for Fold<T, Op> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.start.fmt(f)?;
        self.more
            .iter()
            .try_for_each(|(op, right)| write!(f, " {op} {right}"))
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.host(&mut (Printer { formatter: f }))
    }
}

impl Display for EqualityOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
        }
    }
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
        }
    }
}

impl Display for SumOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
        }
    }
}

impl Display for FactorOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Divide => write!(f, "/"),
            Self::Multiply => write!(f, "*"),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}
