use crate::{debug::node_name, node::Node, parser::Object};

#[allow(dead_code)]
pub fn dump_ast(program: &Vec<Object>) {
    for global in program {
        let Object::Function(f) = global else {
            continue;
        };
        let depth = 0;
        for node in f.body.iter() {
            dump(node, depth);
        }
    }
}

fn indent(depth: usize) {
    for _ in 0..depth {
        print!(" ");
    }
}

fn dump(node: &Node, depth: usize) {
    indent(depth);
    println!("- {}", node_name(node));
    match node {
        Node::Block(b) => {
            for n in b.block_body.iter() {
                dump(n, depth + 1);
            }
        }
        Node::Variable(_) => (),
        Node::StmtExpr(s) => {
            for n in s.block_body.iter() {
                dump(n, depth + 1);
            }
        }
        Node::Numeric(_) => (),
        Node::Cast(c)
        | Node::AddressOf(c)
        | Node::Dereference(c)
        | Node::Neg(c)
        | Node::Not(c)
        | Node::BitNot(c) => dump(&c.lhs, depth + 1),
        Node::Div(n)
        | Node::Add(n)
        | Node::Mul(n)
        | Node::Sub(n)
        | Node::LessThan(n)
        | Node::LessThanEq(n)
        | Node::Eq(n)
        | Node::NotEq(n)
        | Node::Assign(n)
        | Node::Comma(n)
        | Node::Modulus(n) => {
            dump(&n.lhs, depth + 1);
            dump(&n.rhs, depth + 1);
        }
        Node::ExprStmt(n) => dump(&n.lhs, depth + 1),
        Node::While(n) => {
            dump(&n.cond, depth + 1);
            dump(&n.then, depth + 1);
        }
        Node::For(n) => {
            dump(&n.init, depth + 1);
            if let Some(cond) = &n.cond {
                dump(cond, depth + 1);
            }
            if let Some(inc) = &n.inc {
                dump(inc, depth + 1);
            }
            dump(&n.then, depth + 1);
        }
        Node::If(n) => {
            dump(&n.cond, depth + 1);
            if let Some(els) = &n.els {
                dump(els, depth + 1);
            }
            dump(&n.then, depth + 1);
        }
        Node::Return(n) => dump(&n.lhs, depth + 1),
        Node::StructMember(n) => dump(&n.lhs, depth + 1),
        Node::FunctionCall(_) => (),
        Node::Invalid => panic!(),
    }
}
