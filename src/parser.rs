#![allow(dead_code)]
#![allow(unused)]

use std::cell::Cell;

use crate::node::*;
use crate::token::{Token, TokenKind};
use crate::ty::*;

pub struct FunctionObject {
    pub name: String,
    pub locals: Vec<VarObject>,
    pub params: Vec<Object>,
    pub is_func_def: bool,
    pub body: Vec<Node>,
    pub ty: Type,
}

#[derive(Clone)]
pub struct VarObject {
    pub name: String,
    pub ty: Type,
    pub offset: Cell<i32>,
    pub init_data: String,
    pub is_local: bool,
}

pub enum Object {
    FunctionObject(FunctionObject),
    VarObject(VarObject),
    Invalid,
}

impl Object {
    fn as_var_object_mut(&mut self) -> &mut VarObject {
        if let Self::VarObject(v) = self {
            v
        } else {
            panic!()
        }
    }

    pub fn as_var_object(&self) -> &VarObject {
        if let Self::VarObject(v) = self {
            v
        } else {
            panic!()
        }
    }

    pub fn ty(&self) -> &Type {
        match self {
            Object::FunctionObject(f) => &f.ty,
            Object::VarObject(v) => &v.ty,
            Object::Invalid => panic!(),
        }
    }

    pub fn as_function_object(self) -> FunctionObject {
        if let Self::FunctionObject(v) = self {
            v
        } else {
            panic!()
        }
    }
}

struct TagScope {
    name: String,
    ty: Type,
}

enum Scope {
    Object {
        name: String,
        is_global: bool,
        idx: usize,
    },
    TypeDef {
        name: String,
        typedef: Type,
    },
}

#[derive(Default)]
struct VarAttr {
    is_typedef: bool,
}

// Block level scope
#[derive(Default)]
struct Scopes {
    var_scopes: Vec<Scope>,
    tag_scopes: Vec<TagScope>,
}

pub struct Parser<'a> {
    source: &'a str,
    pub globals: Vec<Object>,
    scopes: Vec<Scopes>,
    locals: Cell<Vec<VarObject>>,
    tokens: Cell<std::iter::Peekable<std::slice::Iter<'a, Token>>>,
    current_fn_return_ty: Type,
    str_literal_counter: usize,
}

impl Parser<'_> {
    pub fn new<'a>(source: &'a str, tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser {
            source,
            globals: Vec::new(),
            scopes: vec![Scopes::default()],
            locals: Cell::new(Vec::new()),
            tokens: tokens.iter().peekable().into(),
            str_literal_counter: 1,
            current_fn_return_ty: Type::NoType,
        }
    }

    pub fn parse(&mut self) {
        loop {
            if let Some(tok) = self.peek() {
                if tok.kind == TokenKind::TOKEOF {
                    break;
                }
            }

            let mut attr = VarAttr::default();
            let base_type = self.declspec(&mut attr);

            if attr.is_typedef {
                self.parse_typedef(base_type);
                continue;
            }

            // copy the iterator, is_function will perform a look ahead
            // and we will need the copy to rewind back
            let tokens_copy = self.tokens.get_mut().clone();
            if self.is_function() {
                // reset the iterator
                self.tokens.set(tokens_copy);
                let func = self.function(base_type);
                self.globals.push(func);
                continue;
            }
        }
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokens.get_mut().peek().cloned().cloned()
    }

    fn skip(&mut self, s: &str) {
        if !self.next_token_equals(s) {
            eprintln!("Expected {s}");
            panic!();
        }
        self.tokens.get_mut().next();
    }

    fn find_tag_scope(&self, tag: &str) -> Option<&TagScope> {
        for scope in self.scopes.iter().rev() {
            for tagscope in scope.tag_scopes.iter().rev() {
                if tagscope.name == tag {
                    return Some(tagscope);
                }
            }
        }
        None
    }

    fn find_typedef(&mut self) -> Option<Type> {
        if self.next_token_kind_is(TokenKind::Identifier) {
            let name = self.next_token_text().to_string();
            return self.find_var_scope(&name).and_then(|ts| {
                if let Scope::TypeDef { typedef, .. } = ts {
                    return Some(typedef.clone());
                }
                None
            });
        }
        None
    }

    fn parse_struct_members(&mut self) -> Vec<StructMember> {
        let mut members = Vec::new();
        while !self.next_token_equals("}") {
            let mut attr = VarAttr::default();
            let mut mem_base_ty = self.declspec(&mut attr);
            let mut first = true;
            // find semicolon
            while !self.consume(";") {
                if !first {
                    self.skip(",");
                }
                first = false;

                let (mem_ty, ident) = self.declarator(&mut mem_base_ty);
                members.push(StructMember {
                    name: ident,
                    ty: mem_ty,
                    offset: 0,
                });
            }
        }
        self.skip("}");
        members
    }

    fn struct_or_union_decl(&mut self, is_union: bool) -> Type {
        // is there a tag
        let mut tag_name = String::new();
        if self.next_token_kind_is(TokenKind::Identifier) {
            tag_name = self.next_token_text().into();
            self.tokens.get_mut().next();
        }

        // struct ref
        if !tag_name.is_empty() && !self.next_token_equals("{") {
            let tagscope = self.find_tag_scope(&tag_name);
            if let Some(ts) = tagscope {
                return ts.ty.clone();
            }
            eprintln!("Unknown struct {tag_name}");
            panic!();
        }

        self.skip("{");

        let members = self.parse_struct_members();
        let st = if is_union {
            Type::Union {
                size: 0,
                alignment: 1,
                members,
            }
        } else {
            Type::Struct {
                size: 0,
                alignment: 1,
                members,
            }
        };

        if !tag_name.is_empty() {
            self.push_tag_scope(&tag_name, st.clone());
        }

        st
    }

    fn align_to(n: usize, align: usize) -> usize {
        return ((n + align - 1) / align) * align;
    }

    fn struct_decl(&mut self) -> Type {
        let mut st = self.struct_or_union_decl(false);
        let mut offset = 0;

        if let Type::Struct {
            size,
            alignment,
            members,
        } = &mut st
        {
            for m in members.iter_mut() {
                offset = Self::align_to(offset, m.ty.alignment());
                m.offset = offset;
                offset += m.ty.size();

                if *alignment < m.ty.alignment() {
                    *alignment = m.ty.alignment();
                }
            }
            *size = Self::align_to(offset, *alignment);
        } else {
            eprintln!("Expected struct decl");
            panic!()
        }
        st
    }

    fn union_decl(&mut self) -> Type {
        let mut st = self.struct_or_union_decl(true);
        let mut offset = 0;

        if let Type::Union {
            size,
            alignment,
            members,
        } = &mut st
        {
            // The union is as big as necessary to hold its largest member
            for m in members.iter_mut() {
                m.offset = 0;
                if *alignment < m.ty.alignment() {
                    *alignment = m.ty.alignment();
                }
                if *size < m.ty.size() {
                    *size = m.ty.size();
                }
            }
            *size = Self::align_to(*size, *alignment);
        } else {
            eprintln!("Expected union decl");
            panic!()
        }
        st
    }

    fn declspec(&mut self, var_attr: &mut VarAttr) -> Type {
        enum CTypes {
            VOID = 1 << 0,
            CHAR = 1 << 2,
            SHORT = 1 << 4,
            INT = 1 << 6,
            LONG = 1 << 8,
            OTHER = 1 << 10,
        }
        let mut counter: usize = 0;
        let mut ty = Type::NoType;

        while let Some(token) = self.peek() {
            if !self.next_token_is_typename() {
                break;
            }

            if let Some(ty2) = self.find_typedef() {
                ty = ty2;
                self.tokens.get_mut().next(); // advance
                continue;
            }

            counter += match self.next_token_text() {
                "void" => CTypes::VOID as usize,
                "char" => CTypes::CHAR as usize,
                "short" => CTypes::SHORT as usize,
                "int" => CTypes::INT as usize,
                "long" => CTypes::LONG as usize,
                "struct" => {
                    ty = self.struct_decl();
                    counter += CTypes::OTHER as usize;
                    self.tokens.get_mut().next(); // advance
                    continue;
                }
                "union" => {
                    ty = self.union_decl();
                    counter += CTypes::OTHER as usize;
                    self.tokens.get_mut().next(); // advance
                    continue;
                }
                _ => {
                    eprintln!("unexpected type {}", self.next_token_text());
                    panic!();
                }
            };

            ty = match counter {
                x if x == CTypes::VOID as usize => Type::void_type(),
                x if x == CTypes::CHAR as usize => Type::char_type(),
                x if x == CTypes::SHORT as usize
                    || x == (CTypes::SHORT as usize) + (CTypes::INT as usize) =>
                {
                    Type::int_type()
                }
                x if x == CTypes::INT as usize => Type::int_type(),
                x if x == CTypes::LONG as usize
                    || x == (CTypes::LONG as usize) + (CTypes::INT as usize)
                    || x == (CTypes::LONG as usize) + (CTypes::LONG as usize) =>
                {
                    Type::long_type()
                }
                _ => {
                    eprintln!("invalid type");
                    Type::NoType
                }
            };

            self.tokens.get_mut().next(); // advance
        }

        ty
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scopes::default())
    }

    fn leave_scope(&mut self) {
        self.scopes.pop();
    }

    fn push_var_scope<'a>(&mut self, name: String, is_global: bool, index: usize) {
        assert!(!self.scopes.is_empty());
        self.scopes
            .last_mut()
            .unwrap()
            .var_scopes
            .push(Scope::Object {
                name,
                is_global,
                idx: index,
            });
    }

    fn push_typedef_scope<'a>(&mut self, name: String, ty: Type) {
        assert!(!self.scopes.is_empty());
        self.scopes
            .last_mut()
            .unwrap()
            .var_scopes
            .push(Scope::TypeDef { name, typedef: ty });
    }

    fn push_tag_scope(&mut self, name: &str, ty: Type) {
        assert!(!self.scopes.is_empty());
        self.scopes.last_mut().unwrap().tag_scopes.push(TagScope {
            name: name.to_string(),
            ty,
        });
    }

    fn new_variable(&mut self, name: &str, ty: Type, is_global: bool) -> Object {
        let obj = Object::VarObject(VarObject {
            name: name.to_string(),
            ty,
            offset: 0.into(),
            init_data: String::new(),
            is_local: !is_global,
        });

        let idx = if is_global {
            self.globals.len()
        } else {
            self.locals.get_mut().len()
        };

        self.push_var_scope(name.to_string(), is_global, idx);
        obj
    }

    fn create_param_local_vars(&mut self, param_types: &Vec<FuncParamType>) -> Vec<Object> {
        let mut locals: Vec<Object> = Vec::new();
        let mut fn_locals = Vec::new();
        for ty in param_types {
            locals.push(self.new_variable(&ty.name, ty.ty.clone(), false));
            // Copy into function locals, dont use new_variable because we dont' want to duplicate varscope
            let obj = VarObject {
                name: ty.name.clone(),
                ty: ty.ty.clone(),
                offset: 0.into(),
                init_data: String::new(),
                is_local: true,
            };
            fn_locals.push(obj);
        }
        self.locals.set(fn_locals);
        locals
    }

    fn function(&mut self, base_ty: Type) -> Object {
        let mut base_ty = base_ty;
        let (ty, ident) = self.declarator(&mut base_ty);

        let mut function = Object::FunctionObject(FunctionObject {
            name: ident.clone(),
            locals: Vec::new(),
            params: Vec::new(),
            is_func_def: false,
            body: Vec::new(),
            ty: ty.clone(),
        });

        self.push_var_scope(ident, true, self.globals.len());

        // clear
        self.locals.take();
        // enter scope
        self.enter_scope();

        let is_func_def = !self.consume(";");
        if !is_func_def {
            return function;
        }

        if let Type::Func(func) = ty {
            let params = self.create_param_local_vars(&func.params);
            self.consume("{");

            self.current_fn_return_ty = *func.return_type.clone();

            println!("compound_stmt...");
            let body = self.compound_stmt();

            self.leave_scope();
            let mut f = function.as_function_object();
            f.params = params;
            f.is_func_def = true;
            f.body = vec![body];
            f.locals = self.locals.take();
            return Object::FunctionObject(f);
        }

        eprintln!("Expected function type!");
        Object::Invalid
    }

    fn compound_stmt(&mut self) -> Node {
        self.enter_scope();
        let mut body: Vec<Node> = Vec::new();
        loop {
            if self.next_token_equals("}") {
                break;
            }

            let is_typename = self.is_typename();

            let mut node = if (is_typename) {
                let mut attr = VarAttr::default();
                let base_ty = self.declspec(&mut attr);
                if (attr.is_typedef) {
                    // parse_typedef
                    self.parse_typedef(base_ty);
                    continue;
                }
                self.declaration(base_ty)
            } else {
                // stmt
                self.stmt()
            };

            node.add_type();
            body.push(node);
        }

        let node = Node::Block(Block {
            token: self.peek().unwrap(),
            block_body: body,
        });

        self.leave_scope();
        self.skip("}");

        node
    }

    fn parse_typedef(&mut self, base_ty: Type) {
        let mut first = true;
        while (!self.consume(";")) {
            if !first {
                self.skip(",");
            }

            first = false;
            let mut base_ty = base_ty.clone();
            let (ty, name) = self.declarator(&mut base_ty);
            self.push_typedef_scope(name, ty);
        }
    }

    fn stmt(&mut self) -> Node {
        if self.next_token_equals("while") {
            self.skip("while");
            self.skip("(");
            let cond = self.expr();
            self.skip(")");
            let then = self.stmt();
            return Node::While(While {
                cond: Box::new(cond),
                then: Box::new(then),
            });
        }

        if self.next_token_equals("for") {
            self.skip("for");
            self.skip("(");
            let init = self.expr_stmt();

            let mut cond: Option<Box<Node>> = None;
            if !self.next_token_equals(";") {
                cond = Some(Box::new(self.expr()));
            }
            self.skip(";");

            let mut inc: Option<Box<Node>> = None;
            if !self.next_token_equals(")") {
                inc = Some(Box::new(self.expr()));
            }
            self.skip(")");
            let then = self.stmt();
            return Node::For(For {
                init: init.into(),
                cond,
                inc,
                then: then.into(),
            });
        }

        if self.next_token_equals("if") {
            self.skip("if");
            self.skip("(");
            let cond = self.expr();
            self.skip(")");
            let then = self.stmt();

            let mut els: Option<Box<Node>> = None;
            if self.next_token_equals("else") {
                self.skip("else");
                els = Some(Box::new(self.stmt()));
            }
            return Node::If(If {
                cond: cond.into(),
                then: then.into(),
                els,
            });
        }

        if self.next_token_equals("return") {
            self.skip("return");
            let node = Node::Cast(Cast {
                lhs: Box::new(self.expr()),
                ty: self.current_fn_return_ty.clone(),
            });
            self.skip(";");
            return Node::Return(Return {
                lhs: Box::new(node),
            });
        }

        if self.next_token_equals("{") {
            self.skip("{");
            return self.compound_stmt();
        }

        self.expr_stmt()
    }

    // expr_stmt = expr? ";"
    fn expr_stmt(&mut self) -> Node {
        if self.next_token_equals(";") {
            let tok = self.peek().clone().unwrap();
            self.skip(";");
            return Node::Block(Block {
                token: tok,
                block_body: Vec::new(),
            });
        }

        let node = Node::ExprStmt(ExprStmt {
            lhs: Box::new(self.expr()),
        });
        if !self.consume(";") {
            eprintln!("Expected ;");
            panic!();
        }
        node
    }

    // declaration = typespec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self, base_ty: Type) -> Node {
        let mut first = true;
        let mut nodes = Vec::new();
        while (!self.next_token_equals(";")) {
            if !first {
                self.skip(",");
            }

            let mut base_ty = base_ty.clone();
            let (ty, name) = self.declarator(&mut base_ty);
            if let Type::Void { size, alignment } = ty {
                eprintln!("unexpected void type"); // TODO proper error reporting
            }

            let var = self.new_variable(&name, ty.clone(), false);
            let mut locals = self.locals.take();
            // duplicate for now
            locals.push(VarObject {
                name,
                ty,
                offset: 0.into(),
                init_data: String::new(),
                is_local: true,
            });
            self.locals.set(locals);

            if !self.next_token_equals("=") {
                continue;
            }
            self.skip("=");

            let lhs = Node::Variable(Variable { var });
            let rhs = self.assign();
            let node = Node::Assign(BinaryNode {
                ty: Type::NoType,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
            let cur = Node::ExprStmt(ExprStmt { lhs: node.into() });
            nodes.push(cur);
        }

        let token = self.peek().clone().unwrap();
        let block_node = Node::Block(Block {
            token,
            block_body: nodes,
        });
        self.tokens.get_mut().next();
        block_node
    }

    fn assign(&mut self) -> Node {
        let mut node = self.equality();
        if self.consume("=") {
            node = Node::Assign(BinaryNode {
                ty: Type::NoType,
                lhs: Box::new(node),
                rhs: Box::new(self.assign()),
            });
        }
        node
    }

    fn equality(&mut self) -> Node {
        let mut node = self.relational();
        loop {
            if self.next_token_equals("==") {
                self.skip("==");
                node = Node::Eq(BinaryNode {
                    ty: Type::NoType,
                    lhs: Box::new(node),
                    rhs: Box::new(self.relational()),
                });
                continue;
            }

            if self.next_token_equals("!=") {
                self.skip("!=");
                node = Node::NotEq(BinaryNode {
                    ty: Type::NoType,
                    lhs: Box::new(node),
                    rhs: Box::new(self.relational()),
                });
                continue;
            }
            break;
        }
        node
    }

    fn relational(&mut self) -> Node {
        let mut node = self.add();
        loop {
            if self.next_token_equals("<") {
                self.skip("<");
                node = Node::LessThan(BinaryNode {
                    ty: Type::NoType,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                });
                continue;
            }

            if self.next_token_equals("<=") {
                self.skip("<=");
                node = Node::LessThanEq(BinaryNode {
                    ty: Type::NoType,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                });
                continue;
            }

            if self.next_token_equals(">") {
                self.skip(">");
                node = Node::LessThan(BinaryNode {
                    ty: Type::NoType,
                    lhs: Box::new(self.add()),
                    rhs: Box::new(node),
                });
                continue;
            }

            if self.next_token_equals(">=") {
                self.skip(">=");
                node = Node::LessThanEq(BinaryNode {
                    ty: Type::NoType,
                    lhs: Box::new(self.add()),
                    rhs: Box::new(node),
                });
                continue;
            }
            break;
        }
        node
    }

    fn add(&mut self) -> Node {
        let mut node = self.mul();
        loop {
            if self.next_token_equals("+") {
                self.skip("+");
                let right = self.mul();
                node = self.new_add(node, right);
                continue;
            }

            if self.next_token_equals("-") {
                self.skip("-");
                let right = self.mul();
                node = self.new_sub(node, right);
                continue;
            }

            break;
        }
        node
    }

    fn mul(&mut self) -> Node {
        let mut node = self.cast();
        loop {
            if self.next_token_equals("*") {
                self.skip("*");
                node = Node::Mul(BinaryNode {
                    ty: Type::NoType,
                    lhs: Box::new(node),
                    rhs: Box::new(self.cast()),
                });
                continue;
            }

            if self.next_token_equals("/") {
                self.skip("/");
                node = Node::Div(BinaryNode {
                    ty: Type::NoType,
                    lhs: Box::new(node),
                    rhs: Box::new(self.cast()),
                });
                continue;
            }

            break;
        }
        node
    }

    // cast = "(" type-name ")" cast | unary
    fn cast(&mut self) -> Node {
        let toks = self.tokens.get_mut().clone();
        if self.consume("(") && self.next_token_is_typename() {
            let t = self.typename();
            self.skip(")");

            let mut lhs = self.cast();
            lhs.add_type();
            let node = Cast {
                lhs: Box::new(lhs),
                ty: t,
            };
            return Node::Cast(node);
        } else {
            self.tokens.set(toks);
        }
        self.unary()
    }

    fn unary(&mut self) -> Node {
        if self.next_token_equals("&") {
            self.tokens.get_mut().next();
            return Node::AddressOf(AddressOf {
                lhs: Box::new(self.cast()),
                ty: Type::NoType,
            });
        }

        if self.next_token_equals("*") {
            self.tokens.get_mut().next();
            return Node::Dereference(Dereference {
                lhs: Box::new(self.cast()),
                ty: Type::NoType,
            });
        }

        if self.next_token_equals("-") {
            self.tokens.get_mut().next();
            return Node::Neg(Neg {
                lhs: Box::new(self.cast()),
                ty: Type::NoType,
            });
        }

        if self.next_token_equals("+") {
            self.tokens.get_mut().next();
            return self.cast();
        }

        self.postfix()
    }

    fn new_add(&mut self, mut left: Node, mut right: Node) -> Node {
        left.add_type();
        right.add_type();

        if left.ty().is_int() && right.ty().is_int() {
            return Node::Add(BinaryNode {
                ty: Type::NoType,
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
        }

        if left.ty().is_ptr() && right.ty().is_ptr() {
            eprintln!("new_add: Invalid operands");
            panic!();
        }

        if !left.ty().is_ptr() && right.ty().is_ptr() {
            std::mem::swap(&mut left, &mut right);
        }

        right = Node::Mul(BinaryNode {
            ty: Type::NoType,
            lhs: Box::new(right),
            rhs: Box::new(Node::Numeric(Numeric {
                // unwrap should be safe here, we made sure above that left is a ptr
                val: left.ty().base_ty().unwrap().size(),
                ty: Type::long_type(),
            })),
        });

        Node::Add(BinaryNode {
            ty: Type::NoType,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    fn new_sub(&mut self, mut left: Node, mut right: Node) -> Node {
        left.add_type();
        right.add_type();

        if left.ty().is_int() && right.ty().is_int() {
            return Node::Sub(BinaryNode {
                ty: Type::NoType,
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
        }

        if left.ty().is_ptr() && right.ty().is_int() {
            right = Node::Mul(BinaryNode {
                ty: Type::NoType,
                lhs: Box::new(right),
                rhs: Box::new(Node::Numeric(Numeric {
                    // unwrap should be safe here, we made sure above that left is a ptr
                    val: left.ty().base_ty().unwrap().size(),
                    ty: Type::long_type(),
                })),
            });
            right.add_type();
            return Node::Sub(BinaryNode {
                ty: left.ty().clone(),
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
        }

        if left.ty().is_ptr() && right.ty().is_ptr() {
            let size = left.ty().base_ty().unwrap().size();
            let node = Node::Sub(BinaryNode {
                ty: Type::int_type(),
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
            return Node::Div(BinaryNode {
                ty: Type::NoType,
                lhs: Box::new(node),
                rhs: Node::Numeric(Numeric {
                    val: size,
                    ty: Type::long_type(),
                })
                .into(),
            });
        }

        eprintln!("new_sub: Invalid operands");
        panic!();
    }

    fn get_struct_member(members: &Vec<StructMember>, name: &str) -> StructMember {
        for member in members {
            if member.name == name {
                return member.clone(); // TODO do better, return index into vec?
            }
        }

        eprintln!("Unknown struct member {name}");
        panic!();
    }

    fn struct_ref(&mut self, mut left: Node) -> Node {
        left.add_type();

        match left.ty() {
            Type::Struct { members, .. } | Type::Union { members, .. } => {
                self.tokens.get_mut().next();
                let member = Self::get_struct_member(members, self.next_token_text());
                return Node::StructMember(StructMembr {
                    lhs: Box::new(left),
                    member,
                });
            }
            _ => {
                eprintln!("Expected a struct or union");
                panic!();
            }
        };
    }

    // postfix = primary ("[" expr "]")* | "." ident)*
    fn postfix(&mut self) -> Node {
        let mut node = self.primary();

        loop {
            if self.consume("[") {
                let idx_node = self.expr();
                node = Node::Dereference(Dereference {
                    lhs: Box::new(self.new_add(node, idx_node)),
                    ty: Type::NoType,
                });
                continue;
            }

            if self.consume(".") {
                node = self.struct_ref(node);
                continue;
            }

            if self.consume("->") {
                // x->y is short for (*x).y
                node = Node::Dereference(Dereference {
                    lhs: Box::new(node),
                    ty: Type::NoType,
                });
                node = self.struct_ref(node);
                continue;
            }

            break;
        }
        node
    }

    fn expr(&mut self) -> Node {
        let mut node = self.assign();
        if self.consume(",") {
            node = Node::Comma(BinaryNode {
                ty: Type::NoType,
                lhs: Box::new(node),
                rhs: Box::new(self.expr()),
            });
        }
        node
    }

    // primary = "(" "{" stmt+ "}" ")"
    //         | "(" expr ")"
    //         | "sizeof" unary
    //         | ident func-args?
    //         | str
    //         | num
    fn primary(&mut self) -> Node {
        let tokens_copy = self.tokens.get_mut().clone();
        // GNU Expr Stmt
        if (self.consume("(") && self.consume("{")) {
            let body = self.compound_stmt();
            let node = Node::StmtExpr(StmtExpr {
                block_body: vec![body],
                ty: Type::NoType,
            });
            self.skip(")");
            return node;
        }
        // reset
        self.tokens.set(tokens_copy);

        if self.consume("(") {
            let node = self.expr();
            if !self.consume(")") {
                eprintln!("Expected )");
                panic!();
            }
            return node;
        }

        if self.consume("sizeof") {
            let tokens_copy = self.tokens.get_mut().clone();
            if self.consume("(") && self.next_token_is_typename() {
                let t = self.typename();
                return Node::Numeric(Numeric {
                    val: t.size(),
                    ty: Type::NoType,
                });
            }
            // reset
            self.tokens.set(tokens_copy);
            let mut node = self.unary();
            node.add_type();
            return Node::Numeric(Numeric {
                val: node.ty().size(),
                ty: Type::NoType,
            });
        }

        if self.next_token_kind_is(TokenKind::Identifier) {
            let is_function_call: bool = {
                let mut tokens_copy = self.tokens.get_mut().clone();
                tokens_copy.next(); // skip ident
                let res = tokens_copy
                    .next()
                    .map(|t| self.text(&t) == "(")
                    .unwrap_or(false);
                res
            };

            if is_function_call {
                return self.function_call();
            }

            let Some(obj) = self.find_var() else {
                eprintln!("Unknown variable");
                panic!();
            };
            self.tokens.get_mut().next(); // skip var
            return Node::Variable(Variable {
                var: Object::VarObject(obj),
            });
        }

        if self.next_token_kind_is(TokenKind::StringLiteral) {
            let tok = self.peek().unwrap();
            let v = self.str_literal_counter;
            self.str_literal_counter += 1;
            let name = format!(".L..{}", v);
            let ty = Type::array_of(Type::char_type(), tok.len);
            let mut var = self.new_variable(&name, ty.clone(), true);
            var.as_var_object_mut().init_data = tok.string_literal.unwrap();
            self.globals.push(var);
            // create a copy for now
            let obj = Object::VarObject(VarObject {
                name: name.to_string(),
                ty,
                offset: 0.into(),
                init_data: String::new(), // initdata not needed
                is_local: false,
            });
            self.tokens.get_mut().next(); // skip str literal
            return Node::Variable(Variable { var: obj });
        }

        if self.next_token_kind_is(TokenKind::Numeric) {
            let tok = self.peek().unwrap();
            let node = Node::Numeric(Numeric {
                val: tok.val.unwrap(),
                ty: Type::NoType,
            });
            self.tokens.get_mut().next();
            return node;
        }

        eprintln!("Invalid stmt");
        panic!();
    }

    fn function_call(&mut self) -> Node {
        let func_ret_ty = {
            let func = self.find_func_var();
            if let None = func {
                eprintln!("Unknown function");
                panic!();
            }
            let func = func.unwrap();
            if let None = func.ty.as_func() {
                eprintln!("Not a function");
                panic!();
            } else {
                *func.ty.as_func().unwrap().return_type.clone()
            }
        };
        let name = self.next_token_text().to_string();

        self.tokens.get_mut().next();
        self.tokens.get_mut().next();

        let mut args: Vec<Node> = Vec::new();
        let mut first = true;
        while !self.next_token_equals(")") {
            if !first {
                self.skip(",");
            }
            first = false;
            let mut assign = self.assign();
            assign.add_type();
            args.push(assign);
        }
        self.skip(")");

        Node::FunctionCall(FunctionCall {
            name,
            args,
            ty: func_ret_ty,
        })
    }

    fn find_var_scope(&self, var_name: &str) -> Option<&Scope> {
        for scope in self.scopes.iter().rev() {
            for varscope in scope.var_scopes.iter().rev() {
                if let Scope::Object { name, .. } = varscope {
                    if var_name == name {
                        return Some(varscope);
                    }
                }
            }
        }
        None
    }

    fn abstract_declarator(&mut self, ty: &mut Type) -> Type {
        while self.consume("*") {
            *ty = Type::pointer_to((*ty).clone())
        }

        if self.consume("(") {
            let mut dummy = Type::void_type();
            self.abstract_declarator(&mut dummy);
            self.skip(")");
            let mut ty = self.type_suffix(ty.clone());
            self.tokens.get_mut().next();
            return self.abstract_declarator(&mut ty);
        }

        self.type_suffix(ty.clone())
    }

    fn typename(&mut self) -> Type {
        let mut attr = VarAttr::default();
        let mut t = self.declspec(&mut attr);
        self.abstract_declarator(&mut t)
    }

    fn find_var(&mut self) -> Option<VarObject> {
        let token = self.peek().unwrap();
        let text = self.text(&token);
        if let Some(Scope::Object {
            name,
            is_global,
            idx,
        }) = self.find_var_scope(text)
        {
            if *is_global {
                if let Object::VarObject(vo) = &self.globals[*idx] {
                    return Some(vo.clone());
                }
            } else {
                let locals = self.locals.take();
                let var = locals[*idx].clone();
                self.locals.set(locals);
                return Some(var);
            }
        }
        None
    }

    fn find_func_var(&mut self) -> Option<&FunctionObject> {
        let token = self.peek().unwrap();
        let text = self.text(&token);
        if let Some(Scope::Object {
            name,
            is_global,
            idx,
        }) = self.find_var_scope(text)
        {
            if *is_global {
                if let Object::FunctionObject(f) = &self.globals[*idx] {
                    return Some(f);
                }
            }
        }
        None
    }

    fn is_typename(&mut self) -> bool {
        {
            let type_keywords = [
                "char", "int", "struct", "union", "long", "short", "void", "typedef",
            ];

            let text = self.next_token_text();
            for k in type_keywords {
                if text == k {
                    return true;
                }
            }
        }
        self.find_typedef().is_some()
    }

    fn is_function(&mut self) -> bool {
        if self.next_token_equals(";") {
            false
        } else {
            let mut ty = Type::NoType;
            let (ty, x) = self.declarator(&mut ty);
            match ty {
                Type::Func(_) => true,
                _ => false,
            }
        }
    }

    fn consume(&mut self, text: &str) -> bool {
        if let Some(token) = self.peek() {
            if self.text(&token) == text {
                self.tokens.get_mut().next();
                return true;
            }
        }
        false
    }

    // declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
    fn declarator(&mut self, ty: &mut Type) -> (Type, String) {
        while self.consume("*") {
            *ty = Type::pointer_to((*ty).clone())
        }

        if self.next_token_equals("(") {
            let tokens = self.tokens.get_mut().clone();
            self.skip("(");
            // skip identifer, use a dummy type
            let mut dummy = Type::NoType;
            self.declarator(&mut dummy);
            self.skip(")");

            // get the type suffix
            *ty = self.type_suffix((*ty).clone());
            // reset
            self.tokens.set(tokens);
            // get the ident
            return self.declarator(ty);
        }

        if !self.next_token_kind_is(TokenKind::Identifier) {
            eprintln!("Expected an identifier!");
        }
        let name_token = self.peek().unwrap();
        let name = self.text(&name_token).to_string();
        self.tokens.get_mut().next();
        let ty = self.type_suffix((*ty).clone());
        (ty, name)
    }

    fn func_params(&mut self, return_ty: Type) -> Type {
        let mut params: Vec<FuncParamType> = Vec::new();
        while !self.next_token_equals(")") {
            if self.next_token_equals(",") {
                self.skip(",");
            }
            let mut attr = VarAttr::default();
            let mut base_type = self.declspec(&mut attr);
            let (ty, name) = self.declarator(&mut base_type);
            params.push(FuncParamType {
                name: name.to_string(),
                ty,
            });
        }
        let ty = Type::func_type(return_ty, params);
        self.tokens.get_mut().next();
        ty
    }

    fn type_suffix(&mut self, ty: Type) -> Type {
        if self.next_token_equals("(") {
            self.skip("(");
            self.func_params(ty)
        } else if self.next_token_equals("[") {
            self.skip("[");
            let tok = self.peek();
            if !self.next_token_kind_is(TokenKind::Numeric) {
                eprintln!("Expected a number!");
            }
            let size = tok.unwrap().val.unwrap();
            self.tokens.get_mut().next(); // skip number
            self.skip("]");

            let ty = self.type_suffix(ty);
            Type::array_of(ty, size)
        } else {
            ty
        }
    }

    fn text<'a>(&'a self, token: &'a Token) -> &str {
        token.text(self.source)
    }

    fn next_token_equals(&mut self, s: &str) -> bool {
        self.next_token_text() == s
    }

    fn next_token_kind_is(&mut self, kind: TokenKind) -> bool {
        if let Some(tok) = self.peek() {
            tok.kind == kind
        } else {
            false
        }
    }

    fn next_token_text(&mut self) -> &str {
        if let Some(tok) = self.tokens.get_mut().peek() {
            tok.text(&self.source)
        } else {
            panic!()
        }
    }

    fn next_token_is_typename(&mut self) -> bool {
        self.is_typename()
    }
}
