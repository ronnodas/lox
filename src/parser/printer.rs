use std::fmt::{self, Display};

use super::ast::{
    Assignment, Expression, ExpressionHost, ExpressionVisitCombiner, ExpressionVisitor, Fold,
    Primary, Unary,
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
            Expression::Or(or) => or.fmt(self.formatter),
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

impl<T, Op> Display for Fold<T, Op>
where
    for<'a, 'f> T: Display + ExpressionHost<Printer<'a, 'f>>,
    for<'a, 'f> Op: Display + ExpressionVisitCombiner<Printer<'a, 'f>, T>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.host(&mut (Printer { formatter: f }))
    }
}

impl<'a, 'f, T: Display + ExpressionHost<Printer<'a, 'f>>, Op: Display>
    ExpressionVisitCombiner<Printer<'a, 'f>, T> for Op
{
    type Output = ();
    type Error = fmt::Error;

    fn combine(&self, (): (), visitor: &mut Printer, rhs: &T) -> fmt::Result {
        write!(visitor.formatter, " {self} {rhs}")
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.host(&mut (Printer { formatter: f }))
    }
}
