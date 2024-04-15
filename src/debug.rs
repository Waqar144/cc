use crate::node::*;

// BEGIN tracing

#[allow(unused_macros)]
macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);

        // Find and cut the rest of the path
        match &name[..name.len() - 3].rfind(':') {
            Some(pos) => &name[pos + 1..name.len() - 3],
            None => &name[..name.len() - 3],
        }
    }};
}
#[allow(unused_imports)]
pub(crate) use function;

macro_rules! add_tracing {
    () => {
        struct TraceRaii {}

        impl TraceRaii {
            fn new() -> TraceRaii {
                unsafe {
                    TRACE_DEPTH += 1;
                };
                TraceRaii {}
            }
        }

        impl Drop for TraceRaii {
            fn drop(&mut self) {
                unsafe {
                    TRACE_DEPTH -= 1;
                };
            }
        }
    };
}
pub(crate) use add_tracing;

macro_rules! trace {
    ($($arg:tt)*) => {
        #[cfg(feature = "tracing")]
        unsafe {
            println!(
                "\x1b[1;32m{:>depth$} {}\x1b[0m",
                "==",
                format_args!($($arg)*),
                depth = TRACE_DEPTH
            );
        }
    };
}
pub(crate) use trace;

// END tracing

pub fn node_name(node: &Node) -> &'static str {
    match node {
        Node::Block(_) => "Block",
        Node::Variable(_) => "Variable",
        Node::StmtExpr(_) => "StmtExpr",
        Node::Numeric(_) => "Numeric",
        Node::Cast(_) => "Cast",
        Node::AddressOf(_) => "Address",
        Node::Dereference(_) => "Dereference",
        Node::Neg(_) => "Neg",
        Node::Add(_) => "Add",
        Node::Mul(_) => "Mul",
        Node::Sub(_) => "Sub",
        Node::Div(_) => "Div",
        Node::LessThan(_) => "LessThan",
        Node::LessThanEq(_) => "LessThanEq",
        Node::Eq(_) => "Eq",
        Node::NotEq(_) => "NotEq",
        Node::Assign(_) => "Assign",
        Node::ExprStmt(_) => "ExprStmt",
        Node::Comma(_) => "Comma",
        Node::While(_) => "While",
        Node::For(_) => "For",
        Node::If(_) => "If",
        Node::Return(_) => "Return",
        Node::StructMember(_) => "StructMember",
        Node::FunctionCall(_) => "FunctionCall",
        Node::Invalid => "Invalid",
    }
}
