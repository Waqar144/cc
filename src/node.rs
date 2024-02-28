use crate::token::Token;
use crate::ty::*;

pub struct Block {
    pub token: Token,
    pub block_body: Vec<Node>,
}

pub struct Variable {
    pub idx: usize,
    pub is_local: bool,
    pub ty: Type,
}

pub struct StmtExpr {
    pub block_body: Vec<Node>,
    pub ty: Type,
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

pub struct StructMembr {
    pub lhs: Box<Node>,
    pub member: StructMember,
}

pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Node>,
    pub ty: Type,
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
    StructMember(StructMembr),
    FunctionCall(FunctionCall),
    Invalid,
}

impl Node {
    pub fn binary_node(&self) -> Option<&BinaryNode> {
        match self {
            Node::Add(b)
            | Node::Mul(b)
            | Node::Sub(b)
            | Node::Div(b)
            | Node::LessThan(b)
            | Node::LessThanEq(b)
            | Node::Eq(b)
            | Node::NotEq(b)
            | Node::Assign(b)
            | Node::Comma(b) => Some(b),
            _ => None,
        }
    }

    fn common_type(t1: &Type, t2: &Type) -> Type {
        if let Some(base) = t1.base_ty() {
            return Type::pointer_to(*base.clone());
        }
        if t1.size() == 8 || t2.size() == 8 {
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
            Node::Variable(_) => (),
            Node::StmtExpr(se) => {
                if !se.block_body.is_empty() {
                    if let Node::ExprStmt(e) = se.block_body.last().unwrap() {
                        se.ty = e.lhs.ty().clone();
                    }
                }
            }
            Node::Numeric(n) => {
                if i32::try_from(n.val).is_ok() {
                    n.ty = Type::int_type();
                } else {
                    n.ty = Type::long_type();
                }
            }
            Node::Cast(_) => (),
            Node::Invalid => panic!(),
            Node::AddressOf(a) => {
                if a.lhs.ty().is_array() {
                    a.ty = Type::pointer_to(*a.lhs.ty().base_ty().unwrap().clone());
                } else {
                    a.ty = Type::pointer_to(a.lhs.ty().clone());
                }
            }
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
                n.lhs.add_type();
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
                n.lhs.add_type();
                n.rhs.add_type();

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
            Node::StructMember(_) => (),
            Node::FunctionCall(f) => f.ty = Type::long_type(),
        }
    }

    pub fn ty(&self) -> &Type {
        match self {
            Node::Block(_) => &Type::NoType,
            Node::Variable(v) => &v.ty,
            Node::StmtExpr(se) => &se.ty,
            Node::Numeric(n) => &n.ty,
            Node::Cast(c) => &c.ty,
            Node::Invalid => panic!(),
            Node::AddressOf(a) => &a.ty,
            Node::Dereference(d) => &d.ty,
            Node::Neg(n) => n.lhs.ty(),
            Node::Add(n) | Node::Mul(n) | Node::Sub(n) | Node::Div(n) => &n.ty,
            Node::LessThan(n) | Node::LessThanEq(n) | Node::Eq(n) | Node::NotEq(n) => &n.ty,
            Node::Assign(a) => a.lhs.ty(),
            Node::ExprStmt(_) => &Type::NoType,
            Node::Comma(c) => c.rhs.ty(),
            Node::While(_) => &Type::NoType,
            Node::For(_) => &Type::NoType,
            Node::If(_) => &Type::NoType,
            Node::Return(r) => r.lhs.ty(),
            Node::StructMember(s) => &s.member.ty,
            Node::FunctionCall(f) => &f.ty,
        }
    }
}
