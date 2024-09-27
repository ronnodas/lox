use std::fmt::{self, Display};

use super::ast::{
    Arithmetic, Atom, Binary, Comparison, Equality, Grouping, Logical, Node, Prefix, Value, Visitor,
};
use super::Span;

struct Printer<'a, 'f> {
    formatter: &'a mut fmt::Formatter<'f>,
}

impl<'a, 'f> Visitor for Printer<'a, 'f> {
    type Output = ();
    type Error = fmt::Error;

    fn visit_atom(&mut self, atom: &Atom, _span: Span) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "{atom}")
    }

    fn visit_prefix(
        &mut self,
        op: Prefix,
        arg: &Node,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "{op}{arg}")
    }

    fn visit_binary(
        &mut self,
        op: Binary,
        [left, right]: &[Node; 2],
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "{left} {op} {right}")
    }

    fn visit_group(
        &mut self,
        grouping: Grouping,
        items: &[Node],
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        match grouping {
            Grouping::Brace => {
                write!(self.formatter, "{{")?;
                items
                    .iter()
                    .try_for_each(|arg| write!(self.formatter, "{arg}; "))?;
                write!(self.formatter, "}}")
            }
        }
    }

    fn visit_assignment(
        &mut self,
        left: &super::ast::LValue,
        right: &Node,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "{left} = {right}")
    }

    fn visit_declaration(
        &mut self,
        identifier: &super::ast::Identifier,
        initializer: Option<&Node>,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        match initializer {
            Some(initializer) => write!(self.formatter, "var {identifier} = {initializer};"),
            None => write!(self.formatter, "var {identifier};"),
        }
    }

    fn visit_parenthesized(
        &mut self,
        node: &Node,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        write!(self.formatter, "({node})")
    }

    fn visit_for(
        &mut self,
        condition: &Node,
        body: &Node,
        increment: Option<&Node>,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        match increment {
            Some(increment) => {
                write!(self.formatter, "for (;{condition}; {increment}) {body}")
            }
            None => {
                write!(self.formatter, "for (; {condition};) {body}")
            }
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.host(&mut (Printer { formatter: f }))
    }
}

impl Display for Value {
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
            Self::String(string) => write!(f, "{string:?}"),
            Self::Boolean(boolean) => write!(f, "{boolean}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(value) => write!(f, "{value}"),
            Self::Identifier(identifier) => write!(f, "{identifier}"),
        }
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Self::Minus => "-",
            Self::Not => "!",
            Self::Print => "print ",
        };
        write!(f, "{string}")
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
