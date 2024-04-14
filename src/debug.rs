use crate::node::*;

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
