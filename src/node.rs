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

pub enum Node {
    Block(Block),
    Variable(Variable),
    StmtExpr(StmtExpr),
    Numeric(Numeric),
    Cast(Cast),
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
        }
    }
}
