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

pub enum Node {
    Block(Block),
    Variable(Variable),
    StmtExpr(StmtExpr),
    Numeric(usize),
    Cast(Cast),
    Invalid,
}
