use std::fmt::{self, Display};

use itertools::Itertools as _;

use super::ast::{
    Arithmetic, Atom, Binary, Comparison, Equality, ExpressionNode, ExpressionVisitor, Literal,
    Logical, Prefix,
};
use super::Span;

struct Printer<'a, 'f> {
    formatter: &'a mut fmt::Formatter<'f>,
}

impl<'a, 'f> ExpressionVisitor for Printer<'a, 'f> {
    type Output = ();
    type Error = fmt::Error;

    fn visit_atom(&mut self, atom: &Atom, _span: Span) -> Result<Self::Output, Self::Error> {
        if let Atom::Literal(Literal::String(string)) = atom {
            write!(self.formatter, "{string:?}")
        } else {
            write!(self.formatter, "{atom}")
        }
    }

    fn visit_prefix(
        &mut self,
        op: Prefix,
        arg: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "{op}{arg}")
    }

    fn visit_binary(
        &mut self,
        op: Binary,
        [left, right]: &[ExpressionNode; 2],
    ) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "{left} {op} {right}")
    }

    fn visit_assignment(
        &mut self,
        left: &super::ast::LValue,
        right: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "{left} = {right}")
    }

    fn visit_parenthesized(&mut self, node: &ExpressionNode) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "({node})")
    }

    fn visit_call(
        &mut self,
        callee: &ExpressionNode,
        args: &[ExpressionNode],
    ) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "{callee}(")?;
        args.iter().format(", ").fmt(self.formatter)?;
        write!(self.formatter, ")")
    }
}

impl Display for ExpressionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.host(&mut (Printer { formatter: f }))
    }
}

impl Display for Literal {
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
            Self::String(string) => write!(f, "{string}"),
            Self::Boolean(boolean) => write!(f, "{boolean}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(value) => write!(f, "{value}"),
            Self::Identifier(identifier) => write!(f, "{identifier}"),
        }
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Self::Minus => "-",
            Self::Not => "!",
        };
        write!(f, "{string}")
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Field => write!(f, "."),
            Self::Arithmetic(arithmetic) => write!(f, "{arithmetic}"),
            Self::Comparison(comparison) => write!(f, "{comparison}"),
            Self::Equality(equality) => write!(f, "{equality}"),
            Self::Logical(logical) => write!(f, "{logical}"),
        }
    }
}

impl Display for Arithmetic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Divide => "/",
        };
        write!(f, "{string}")
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",
        };
        write!(f, "{string}")
    }
}

impl Display for Equality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Self::Equal => "==",
            Self::NotEqual => "!=",
        };
        write!(f, "{string}")
    }
}

impl Display for Logical {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Self::And => "and",
            Self::Or => "or",
        };
        write!(f, "{string}")
    }
}
