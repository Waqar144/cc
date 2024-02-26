use crate::parser::Object;
use crate::token::Token;
use crate::ty::*;

pub struct Block {
    pub token: Token,
    pub block_body: Vec<Node>,
}

pub struct Variable {
    pub var: Object,
}

pub struct StmtExpr {
    pub block_body: Vec<Node>,
}

pub struct Cast {
    pub lhs: Box<Node>,
    pub ty: Type,
}

pub struct Numeric {
    pub val: usize,
    pub ty: Type,
}

pub struct AddressOf {
    pub lhs: Box<Node>,
    pub ty: Type,
}

pub struct Dereference {
    pub lhs: Box<Node>,
    pub ty: Type,
}

pub struct Neg {
    pub lhs: Box<Node>,
    pub ty: Type,
}

pub struct BinaryNode {
    pub ty: Type,
    pub lhs: Box<Node>,
    pub rhs: Box<Node>,
}

pub enum Node {
    Block(Block),
    Variable(Variable),
    StmtExpr(StmtExpr),
    Numeric(Numeric),
    Cast(Cast),
    AddressOf(AddressOf),
    Dereference(Dereference),
    Neg(Neg),
    Add(BinaryNode),
    Mul(BinaryNode),
    Sub(BinaryNode),
    Div(BinaryNode),
    LessThan(BinaryNode),
    LessThanEq(BinaryNode),
    Invalid,
}

impl Node {
    pub fn add_type(&mut self) {
        match self {
            Node::Block(_) => (),
            Node::Variable(_) => (), // TODO,
            Node::StmtExpr(_) => (),
            Node::Numeric(n) => {
                if i32::try_from(n.val).is_ok() {
                    n.ty = Type::int_type();
                } else {
                    n.ty = Type::long_type();
                }
            }
            Node::Cast(_) => (),
            Node::Invalid => panic!(),
            Node::AddressOf(_) => todo!(),
            Node::Dereference(_) => todo!(),
            Node::Neg(_) => todo!(),
            Node::Add(_) => todo!(),
            Node::Mul(_) => todo!(),
            Node::Sub(_) => todo!(),
            Node::Div(_) => todo!(),
            Node::LessThan(_) => todo!(),
            Node::LessThanEq(_) => todo!(),
        }
    }

    pub fn ty(&self) -> &Type {
        match self {
            Node::Block(_) => todo!(),
            Node::Variable(_) => todo!(),
            Node::StmtExpr(_) => todo!(),
            Node::Numeric(n) => &n.ty,
            Node::Cast(_) => todo!(),
            Node::Invalid => todo!(),
            Node::AddressOf(_) => todo!(),
            Node::Dereference(_) => todo!(),
            Node::Neg(_) => todo!(),
            Node::Add(_) => todo!(),
            Node::Mul(_) => todo!(),
            Node::Sub(_) => todo!(),
            Node::Div(_) => todo!(),
            Node::LessThan(_) => todo!(),
            Node::LessThanEq(_) => todo!(),
        }
    }
}
