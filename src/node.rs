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

pub struct ExprStmt {
    pub lhs: Box<Node>,
}

pub struct While {
    pub cond: Box<Node>,
    pub then: Box<Node>,
}

pub struct For {
    pub init: Box<Node>,
    pub cond: Option<Box<Node>>,
    pub inc: Option<Box<Node>>,
    pub then: Box<Node>,
}

pub struct If {
    pub cond: Box<Node>,
    pub then: Box<Node>,
    pub els: Option<Box<Node>>,
}

pub struct Return {
    pub lhs: Box<Node>,
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
    Eq(BinaryNode),
    NotEq(BinaryNode),
    Assign(BinaryNode),
    ExprStmt(ExprStmt),
    Comma(BinaryNode),
    While(While),
    For(For),
    If(If),
    Return(Return),
    Invalid,
}

impl Node {
    fn common_type(t1: &Type, t2: &Type) -> Type {
        if let Some(base) = t1.base_ty() {
            return Type::pointer_to(*base.clone());
        }
        if t1.size() == 8 && t2.size() == 8 {
            return Type::long_type();
        }
        return Type::int_type();
    }

    fn cast_into(node: &mut Node, ty: Type) {
        // take out the prev value
        let old_value = std::mem::replace(node, Node::Invalid);
        // replace prev node with Cast
        let _ = std::mem::replace(
            node,
            Node::Cast(Cast {
                lhs: Box::new(old_value), // old value moved here
                ty: ty.clone(),
            }),
        );
    }

    fn usual_arithmetic_conversion(lhs: &mut Node, rhs: &mut Node) {
        let ty = Self::common_type(lhs.ty(), rhs.ty());
        Self::cast_into(lhs, ty.clone());
        Self::cast_into(rhs, ty);
    }

    pub fn add_type(&mut self) {
        match self {
            Node::Block(block) => {
                for n in block.block_body.iter_mut() {
                    n.add_type();
                }
            }
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
            Node::Dereference(d) => {
                if !d.lhs.ty().is_ptr() {
                    eprintln!("Invalid Dereference!");
                    panic!();
                }

                if matches!(d.lhs.ty(), Type::Void { .. }) {
                    eprintln!("Cannot Dereference a void ptr");
                    panic!();
                }

                d.lhs.add_type();
                d.ty = *d.lhs.ty().base_ty().unwrap().clone();
            }
            Node::Neg(n) => {
                let ty = Self::common_type(&Type::int_type(), n.lhs.ty());
                Self::cast_into(&mut *n.lhs, ty.clone());
                n.ty = ty;
            }
            Node::Add(n) | Node::Mul(n) | Node::Sub(n) | Node::Div(n) => {
                Self::usual_arithmetic_conversion(&mut *n.lhs, &mut *n.rhs);
                n.ty = n.lhs.ty().clone();
            }
            Node::LessThan(n) | Node::LessThanEq(n) | Node::Eq(n) | Node::NotEq(n) => {
                Self::usual_arithmetic_conversion(&mut *n.lhs, &mut *n.rhs);
                n.ty = Type::int_type();
            }
            Node::Assign(n) => {
                let lhs_ty = n.lhs.ty();
                if lhs_ty.is_array() {
                    eprintln!("Not an lvalue");
                    panic!();
                } else if !matches!(lhs_ty, Type::Struct { .. }) {
                    Self::cast_into(&mut *n.rhs, lhs_ty.clone());
                }
                n.ty = lhs_ty.clone();
            }
            Node::ExprStmt(e) => {
                e.lhs.add_type();
            }
            Node::Comma(c) => {
                c.ty = c.rhs.ty().clone();
            }
            Node::While(w) => {
                w.cond.add_type();
                w.then.add_type();
            }
            Node::For(f) => {
                f.init.add_type();
                f.then.add_type();
                f.cond.as_mut().map(|c| c.add_type());
                f.inc.as_mut().map(|i| i.add_type());
            }
            Node::If(i) => {
                i.cond.add_type();
                i.then.add_type();
                i.els.as_mut().map(|i| i.add_type());
            }
            Node::Return(r) => {
                r.lhs.add_type();
            }
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
            Node::Eq(_) => todo!(),
            Node::NotEq(_) => todo!(),
            Node::Assign(_) => todo!(),
            Node::ExprStmt(_) => todo!(),
            Node::Comma(_) => todo!(),
            Node::While(_) => todo!(),
            Node::For(_) => todo!(),
            Node::If(_) => todo!(),
            Node::Return(_) => todo!(),
        }
    }
}
