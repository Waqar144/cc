use std::cell::Cell;

use crate::debug::*;
use crate::node::*;
use crate::token::{Token, TokenKind};
use crate::ty::*;

static mut TRACE_DEPTH: usize = 1;
add_tracing!();

#[derive(Debug)]
pub struct FunctionObject {
    pub name: String,
    pub locals: Vec<Object>,
    // params are just a list of idxes into self.locals
    pub params: Vec<usize>,
    pub is_func_def: bool,
    pub body: Vec<Node>,
    pub ty: Type,
    pub is_static: bool,
}

#[derive(Clone, Debug)]
pub struct VarObject {
    pub name: String,
    pub ty: Type,
    pub offset: Cell<i32>,
    pub init_data: String,
    pub is_local: bool,
}

#[derive(Debug)]
pub enum Object {
    Function(FunctionObject),
    Var(VarObject),
    Invalid,
}

impl Object {
    fn as_var_object_mut(&mut self) -> &mut VarObject {
        if let Self::Var(v) = self {
            v
        } else {
            panic!()
        }
    }

    pub fn ty(&self) -> &Type {
        match self {
            Object::Function(f) => &f.ty,
            Object::Var(v) => &v.ty,
            Object::Invalid => panic!(),
        }
    }

    pub fn into_function_object(self) -> FunctionObject {
        if let Self::Function(v) = self {
            v
        } else {
            panic!()
        }
    }

    pub fn as_function_object_ref(&self) -> &FunctionObject {
        if let Self::Function(v) = &self {
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
    Enumerator {
        name: String,
        val: usize,
        ty: Type,
    },
}

#[derive(Default)]
struct VarAttr {
    is_typedef: bool,
    is_static: bool,
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
    locals: Cell<Vec<Object>>,
    tokens: Cell<Option<std::iter::Peekable<std::slice::Iter<'a, Token>>>>,
    current_fn_return_ty: Type,
    str_literal_counter: usize,
    // the current function being parsed
    current_fn: Option<FunctionObject>,
}

impl Parser<'_> {
    pub fn new<'a>(source: &'a str, tokens: &'a [Token]) -> Parser<'a> {
        Parser {
            source,
            globals: Vec::new(),
            scopes: vec![Scopes::default()],
            locals: Cell::new(Vec::new()),
            tokens: Some(tokens.iter().peekable()).into(),
            str_literal_counter: 1,
            current_fn_return_ty: Type::None,
            current_fn: None,
        }
    }

    pub fn parse(&mut self) {
        trace!("=>> start parse");
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
                let func = self.function(base_type, attr);
                self.globals.push(func);
                continue;
            } else {
                self.tokens.set(tokens_copy);
            }

            self.parse_global_variables(base_type);
        }
        trace!("=>> end parse");
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

    fn find_typedef(&self) -> Option<Type> {
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
            self.advance();
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
        ((n + align - 1) / align) * align
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

        if let Type::Union {
            size,
            alignment,
            members,
        } = &mut st
        {
            // The union is as big as necessary to hold its largest member
            for m in members.iter_mut() {
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

    // enum-specifier = ident? "{" enum-list? "}"
    //                | ident ("{" enum-list? "}")?
    //
    // enum-list      = ident ("=" num)? ("," ident ("=" num)?)*
    fn enum_specifier(&mut self) -> Type {
        let ty = Type::enum_type();

        // read tag
        let mut tag_name = String::new();
        if self.next_token_kind_is(TokenKind::Identifier) {
            tag_name = self.next_token_text().into();
            self.advance();
        }

        // struct ref
        if !tag_name.is_empty() && !self.next_token_equals("{") {
            let tagscope = self.find_tag_scope(&tag_name);
            if tagscope.is_none() {
                eprintln!("Unknown enum {tag_name}");
                panic!();
            }
            let ts = tagscope.unwrap();
            if !matches!(ts.ty, Type::Enum { .. }) {
                eprintln!("Not an enum tag");
                panic!();
            }
            return ts.ty.clone();
        }

        self.skip("{");

        let mut first = true;
        let mut val = 0;
        while !self.consume("}") {
            if !first {
                self.skip(",");
            }
            first = false;

            let ident = self.next_token_text().to_string();
            self.advance();

            if self.consume("=") {
                val = self.peek().unwrap().val.unwrap();
                self.advance();
            }

            self.push_enum_scope(ident, ty.clone(), val);
            val += 1;
        }

        if !tag_name.is_empty() {
            self.push_tag_scope(&tag_name, ty.clone());
        }

        ty
    }

    fn declspec(&mut self, var_attr: &mut VarAttr) -> Type {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        enum CTypes {
            VOID = 1 << 0,
            BOOL = 1 << 2,
            CHAR = 1 << 4,
            SHORT = 1 << 6,
            INT = 1 << 8,
            LONG = 1 << 10,
            OTHER = 1 << 12,
        }
        let mut counter: usize = 0;
        let mut ty = Type::int_type();

        loop {
            if self.consume("typedef") {
                var_attr.is_typedef = true;
                continue;
            } else if self.consume("static") {
                var_attr.is_static = true;
                continue;
            }

            if let (true, true) = (var_attr.is_typedef, var_attr.is_static) {
                eprintln!("Typedef and static can't be used together");
                panic!();
            }

            if !self.next_token_is_typename() {
                break;
            }

            if let Some(ty2) = self.find_typedef() {
                if counter > 0 {
                    break;
                }
                ty = ty2;
                self.advance(); // advance
                counter += CTypes::OTHER as usize;
                continue;
            }

            counter += match self.next_token_text() {
                "void" => CTypes::VOID as usize,
                "_Bool" => CTypes::BOOL as usize,
                "char" => CTypes::CHAR as usize,
                "short" => CTypes::SHORT as usize,
                "int" => CTypes::INT as usize,
                "long" => CTypes::LONG as usize,
                "struct" => {
                    if counter > 0 {
                        break;
                    }
                    self.advance(); // skip "struct"
                    ty = self.struct_decl();
                    counter += CTypes::OTHER as usize;
                    continue;
                }
                "union" => {
                    if counter > 0 {
                        break;
                    }
                    self.advance(); // skip "union"
                    ty = self.union_decl();
                    counter += CTypes::OTHER as usize;
                    continue;
                }
                "enum" => {
                    if counter > 0 {
                        break;
                    }
                    self.advance(); // skip "enum"
                    ty = self.enum_specifier();
                    counter += CTypes::OTHER as usize;
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
                x if x == CTypes::BOOL as usize => Type::Bool {
                    size: 1,
                    alignment: 1,
                },
                x if x == CTypes::SHORT as usize
                    || x == (CTypes::SHORT as usize) + (CTypes::INT as usize) =>
                {
                    Type::Short {
                        size: 2,
                        alignment: 2,
                    }
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
                    Type::None
                }
            };

            self.advance(); // advance
        }

        ty
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scopes::default())
    }

    fn leave_scope(&mut self) {
        self.scopes.pop();
    }

    fn push_enum_scope(&mut self, name: String, ty: Type, val: usize) {
        assert!(!self.scopes.is_empty());
        self.scopes
            .last_mut()
            .unwrap()
            .var_scopes
            .push(Scope::Enumerator { name, val, ty });
    }

    fn push_var_scope(&mut self, name: String, is_global: bool, index: usize) {
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

    fn push_typedef_scope(&mut self, name: String, ty: Type) {
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
        let obj = Object::Var(VarObject {
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

    fn create_param_local_vars(&mut self, param_types: &Vec<FuncParamType>) -> Vec<usize> {
        let mut locals_idxes: Vec<usize> = Vec::new();
        for ty in param_types {
            let var = self.new_variable(&ty.name, ty.ty.clone(), false);
            let idx = self.locals.get_mut().len();
            self.locals.get_mut().push(var);
            locals_idxes.push(idx);
        }
        locals_idxes
    }

    fn parse_global_variables(&mut self, base_ty: Type) {
        let mut first = true;
        while !self.consume(";") {
            if !first {
                self.skip(",");
            }
            first = false;

            let mut base_ty = base_ty.clone();
            let (ty, ident) = self.declarator(&mut base_ty);
            let obj = self.new_variable(&ident, ty, true);
            self.globals.push(obj);
        }
    }

    fn function(&mut self, base_ty: Type, var_attr: VarAttr) -> Object {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut base_ty = base_ty;
        let (ty, ident) = self.declarator(&mut base_ty);

        let function = Object::Function(FunctionObject {
            name: ident.clone(),
            locals: Vec::new(),
            params: Vec::new(),
            is_func_def: false,
            body: Vec::new(),
            ty: ty.clone(),
            is_static: var_attr.is_static,
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
            self.current_fn = Some(function.into_function_object());

            let body = self.compound_stmt().into_block().unwrap();

            self.leave_scope();
            let mut f = self.current_fn.take().unwrap();
            f.params = params;
            f.is_func_def = true;
            f.body = body.block_body;
            f.locals = self.locals.take();
            return Object::Function(f);
        }

        eprintln!("Expected function type!");
        Object::Invalid
    }

    fn compound_stmt(&mut self) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        self.enter_scope();
        let mut body: Vec<Node> = Vec::new();
        loop {
            if self.next_token_equals("}") {
                break;
            }

            let is_typename = self.is_typename();

            let mut node = if is_typename {
                let mut attr = VarAttr::default();
                let base_ty = self.declspec(&mut attr);
                if attr.is_typedef {
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
            token: self.peek().cloned().unwrap(),
            block_body: body,
        });

        self.leave_scope();
        self.skip("}");

        node
    }

    fn parse_typedef(&mut self, base_ty: Type) {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut first = true;
        while !self.consume(";") {
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
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

            self.enter_scope();

            let init = if self.next_token_is_typename() {
                let mut dummy = VarAttr::default();
                let base_ty = self.declspec(&mut dummy);
                self.declaration(base_ty)
            } else {
                self.expr_stmt()
            };

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

            self.leave_scope();

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
            let mut lhs = self.expr();
            lhs.add_type();
            let node = Node::Cast(Unary {
                lhs: Box::new(lhs),
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        if self.next_token_equals(";") {
            let tok = self.peek().cloned().unwrap();
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut first = true;
        let mut nodes = Vec::new();
        while !self.next_token_equals(";") {
            if !first {
                self.skip(",");
            }
            first = false;

            let mut base_ty = base_ty.clone();
            let (ty, name) = self.declarator(&mut base_ty);
            if let Type::Void { .. } = ty {
                eprintln!("unexpected void type"); // TODO proper error reporting
            }

            let var = self.new_variable(&name, ty.clone(), false);
            let idx = self.locals.get_mut().len();
            let ty = var.ty().clone();
            self.locals.get_mut().push(var);

            if !self.next_token_equals("=") {
                continue;
            }
            self.skip("=");

            let lhs = Node::Variable(Variable {
                idx,
                is_local: true,
                ty,
            });
            let rhs = self.assign();
            let node = Node::Assign(BinaryNode {
                ty: Type::None,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
            let cur = Node::ExprStmt(ExprStmt { lhs: node.into() });
            nodes.push(cur);
        }

        let token = self.peek().cloned().unwrap();
        let block_node = Node::Block(Block {
            token,
            block_body: nodes,
        });
        self.advance();
        block_node
    }

    fn to_assign(&mut self, binary: Node, op: char) -> Node {
        let binary = binary.binary_node_mut().unwrap();
        let (mut lhs, mut rhs) = (binary.lhs, binary.rhs);
        lhs.add_type();
        rhs.add_type();

        let var = self.new_variable("", Type::pointer_to(lhs.ty().clone()), false);
        let var_ty = var.ty().clone();
        self.locals.get_mut().push(var);
        let var = Variable {
            idx: self.locals.get_mut().len() - 1,
            is_local: true,
            ty: var_ty,
        };

        let expr1 = Node::Assign(BinaryNode {
            ty: Type::None,
            lhs: Node::Variable(var.clone()).into(),
            rhs: Node::AddressOf(Unary {
                lhs,
                ty: Type::None,
            })
            .into(),
        });

        let expr2_rhs = BinaryNode {
            ty: Type::None,
            lhs: Node::Dereference(Unary {
                lhs: Node::Variable(var.clone()).into(),
                ty: Type::None,
            })
            .into(),
            rhs,
        };
        let expr2 = Node::Assign(BinaryNode {
            ty: Type::None,
            lhs: Node::Dereference(Unary {
                lhs: Box::new(Node::Variable(var)),
                ty: Type::None,
            })
            .into(),
            rhs: match op {
                '+' => Node::Add(expr2_rhs).into(),
                '-' => Node::Sub(expr2_rhs).into(),
                '*' => Node::Mul(expr2_rhs).into(),
                '/' => Node::Div(expr2_rhs).into(),
                _ => {
                    eprintln!("unexpected op {op}");
                    panic!();
                }
            },
        });

        Node::Comma(BinaryNode {
            ty: Type::None,
            lhs: Box::new(expr1),
            rhs: Box::new(expr2),
        })
    }

    // assign    = equality (assign-op assign)?
    // assign-op = "=" | "+=" | "-=" | "*=" | "/="
    fn assign(&mut self) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut node = self.equality();

        if self.consume("=") {
            node = Node::Assign(BinaryNode {
                ty: Type::None,
                lhs: Box::new(node),
                rhs: Box::new(self.assign()),
            });
        }

        if self.consume("+=") {
            let assign = self.assign();
            let add = self.new_add(node, assign);
            node = self.to_assign(add, '+');
        }

        if self.consume("-=") {
            let assign = self.assign();
            let sub = self.new_sub(node, assign);
            node = self.to_assign(sub, '-');
        }

        if self.consume("*=") {
            let assign = self.assign();
            node = self.to_assign(
                Node::Mul(BinaryNode {
                    ty: Type::None,
                    lhs: node.into(),
                    rhs: assign.into(),
                }),
                '*',
            );
        }

        if self.consume("/=") {
            let assign = self.assign();
            node = self.to_assign(
                Node::Div(BinaryNode {
                    ty: Type::None,
                    lhs: node.into(),
                    rhs: assign.into(),
                }),
                '/',
            );
        }

        node
    }

    fn equality(&mut self) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut node = self.relational();
        loop {
            if self.next_token_equals("==") {
                self.skip("==");
                node = Node::Eq(BinaryNode {
                    ty: Type::None,
                    lhs: Box::new(node),
                    rhs: Box::new(self.relational()),
                });
                continue;
            }

            if self.next_token_equals("!=") {
                self.skip("!=");
                node = Node::NotEq(BinaryNode {
                    ty: Type::None,
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut node = self.add();
        loop {
            if self.next_token_equals("<") {
                self.skip("<");
                node = Node::LessThan(BinaryNode {
                    ty: Type::None,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                });
                continue;
            }

            if self.next_token_equals("<=") {
                self.skip("<=");
                node = Node::LessThanEq(BinaryNode {
                    ty: Type::None,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                });
                continue;
            }

            if self.next_token_equals(">") {
                self.skip(">");
                node = Node::LessThan(BinaryNode {
                    ty: Type::None,
                    lhs: Box::new(self.add()),
                    rhs: Box::new(node),
                });
                continue;
            }

            if self.next_token_equals(">=") {
                self.skip(">=");
                node = Node::LessThanEq(BinaryNode {
                    ty: Type::None,
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut node = self.cast();
        loop {
            if self.next_token_equals("*") {
                self.skip("*");
                node = Node::Mul(BinaryNode {
                    ty: Type::None,
                    lhs: Box::new(node),
                    rhs: Box::new(self.cast()),
                });
                continue;
            }

            if self.next_token_equals("/") {
                self.skip("/");
                node = Node::Div(BinaryNode {
                    ty: Type::None,
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let toks = self.tokens.get_mut().clone();
        if self.consume("(") && self.next_token_is_typename() {
            let t = self.typename();
            self.skip(")");

            let mut lhs = self.cast();
            lhs.add_type();
            let node = Unary {
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        if self.next_token_equals("&") {
            self.advance();
            return Node::AddressOf(Unary {
                lhs: Box::new(self.cast()),
                ty: Type::None,
            });
        }

        if self.next_token_equals("*") {
            self.advance();
            return Node::Dereference(Unary {
                lhs: Box::new(self.cast()),
                ty: Type::None,
            });
        }

        if self.next_token_equals("!") {
            self.advance();
            return Node::Not(Unary {
                lhs: Box::new(self.cast()),
                ty: Type::None,
            });
        }

        if self.next_token_equals("-") {
            self.advance();
            return Node::Neg(Unary {
                lhs: Box::new(self.cast()),
                ty: Type::None,
            });
        }

        if self.next_token_equals("+") {
            self.advance();
            return self.cast();
        }

        if self.consume("++") {
            let unary = self.unary();
            let add = self.new_add(
                unary,
                Node::Numeric(Numeric {
                    val: 1,
                    ty: Type::int_type(),
                }),
            );
            return self.to_assign(add, '+');
        }

        if self.consume("--") {
            let unary = self.unary();
            let add = self.new_sub(
                unary,
                Node::Numeric(Numeric {
                    val: 1,
                    ty: Type::int_type(),
                }),
            );
            return self.to_assign(add, '-');
        }

        self.postfix()
    }

    fn new_add(&mut self, mut left: Node, mut right: Node) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        left.add_type();
        right.add_type();

        if left.ty().is_number() && right.ty().is_number() {
            return Node::Add(BinaryNode {
                ty: Type::None,
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
        }

        if left.ty().base_ty().is_some() && right.ty().base_ty().is_some() {
            eprintln!("new_add: Invalid operands");
            panic!();
        }

        if left.ty().base_ty().is_none() && right.ty().base_ty().is_some() {
            std::mem::swap(&mut left, &mut right);
        }

        right = Node::Mul(BinaryNode {
            ty: Type::None,
            lhs: Box::new(right),
            rhs: Box::new(Node::Numeric(Numeric {
                // unwrap should be safe here, we made sure above that left is a ptr
                val: left.ty().base_ty().unwrap().size(),
                ty: Type::long_type(),
            })),
        });

        Node::Add(BinaryNode {
            ty: Type::None,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    fn new_sub(&mut self, mut left: Node, mut right: Node) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        left.add_type();
        right.add_type();

        if left.ty().is_number() && right.ty().is_number() {
            trace!("{}: {}", function!(), "Sub");
            return Node::Sub(BinaryNode {
                ty: Type::None,
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
        }

        if left.ty().base_ty().is_some() && right.ty().is_number() {
            right = Node::Mul(BinaryNode {
                ty: Type::None,
                lhs: Box::new(right),
                rhs: Box::new(Node::Numeric(Numeric {
                    // unwrap should be safe here, we made sure above that left is a ptr
                    val: left.ty().base_ty().unwrap().size(),
                    ty: Type::long_type(),
                })),
            });
            right.add_type();
            trace!("{}: {}", function!(), "Sub");
            return Node::Sub(BinaryNode {
                ty: left.ty().clone(),
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
        }

        if left.ty().base_ty().is_some() && right.ty().base_ty().is_some() {
            let size = left.ty().base_ty().unwrap().size();
            let node = Node::Sub(BinaryNode {
                ty: Type::int_type(),
                lhs: Box::new(left),
                rhs: Box::new(right),
            });
            trace!("{}: {}", function!(), "Div");
            return Node::Div(BinaryNode {
                ty: Type::None,
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), "");
        for member in members {
            if member.name == name {
                return member.clone(); // TODO do better, return index into vec?
            }
        }

        eprintln!("Unknown struct member {name}");
        panic!();
    }

    fn struct_ref(&mut self, mut left: Node) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        left.add_type();

        match left.ty() {
            Type::Struct { members, .. } | Type::Union { members, .. } => {
                let member = Self::get_struct_member(members, self.next_token_text());
                Node::StructMember(StructMembr {
                    lhs: Box::new(left),
                    member,
                })
            }
            _ => {
                eprintln!("Expected a struct or union");
                panic!();
            }
        }
    }

    fn inc_dec(&mut self, mut node: Node, op: char) -> Node {
        node.add_type();

        fn add_one(_self: &mut Parser<'_>, node: Node) -> Node {
            _self.new_add(
                node,
                Node::Numeric(Numeric {
                    val: 1,
                    ty: Type::int_type(),
                }),
            )
        }
        fn sub_one(_self: &mut Parser<'_>, node: Node) -> Node {
            _self.new_sub(
                node,
                Node::Numeric(Numeric {
                    val: 1,
                    ty: Type::int_type(),
                }),
            )
        }

        let add_or_sub = if op == '+' {
            add_one(self, node)
        } else {
            sub_one(self, node)
        };

        let assign = self.to_assign(add_or_sub, op);

        let mut add_or_sub = if op == '+' {
            sub_one(self, assign)
        } else {
            add_one(self, assign)
        };
        add_or_sub.add_type();
        let ty = add_or_sub.ty().clone();

        Node::Cast(Unary {
            lhs: add_or_sub.into(),
            ty,
        })
    }

    // postfix = primary ("[" expr "]")* | "." ident)*
    fn postfix(&mut self) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut node = self.primary();

        loop {
            if self.consume("[") {
                let idx_node = self.expr();
                self.skip("]");
                node = Node::Dereference(Unary {
                    lhs: Box::new(self.new_add(node, idx_node)),
                    ty: Type::None,
                });
                continue;
            }

            if self.consume(".") {
                node = self.struct_ref(node);
                self.advance();
                continue;
            }

            if self.consume("->") {
                // x->y is short for (*x).y
                node = Node::Dereference(Unary {
                    lhs: Box::new(node),
                    ty: Type::None,
                });
                node = self.struct_ref(node);
                self.advance();
                continue;
            }

            if self.consume("++") {
                node = self.inc_dec(node, '+');
                continue;
            }

            if self.consume("--") {
                node = self.inc_dec(node, '-');
                continue;
            }
            break;
        }
        node
    }

    fn expr(&mut self) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut node = self.assign();
        if self.consume(",") {
            node = Node::Comma(BinaryNode {
                ty: Type::None,
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let tokens_copy = self.tokens.get_mut().clone();
        // GNU Expr Stmt
        if self.consume("(") && self.consume("{") {
            let body = self.compound_stmt();
            let node = Node::StmtExpr(StmtExpr {
                block_body: vec![body],
                ty: Type::None,
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
                self.skip(")");
                return Node::Numeric(Numeric {
                    val: t.size(),
                    ty: Type::None,
                });
            }
            // reset
            self.tokens.set(tokens_copy);
            let mut node = self.unary();
            node.add_type();
            return Node::Numeric(Numeric {
                val: node.ty().size(),
                ty: Type::None,
            });
        }

        if self.next_token_kind_is(TokenKind::Identifier) {
            let is_function_call: bool = {
                let tokens = self.tokens.take().unwrap();
                let mut tokens_copy = tokens.clone();
                self.tokens.set(Some(tokens));
                tokens_copy.next(); // skip ident
                let res = tokens_copy
                    .next()
                    .map(|t| self.text(t) == "(")
                    .unwrap_or(false);
                res
            };

            if is_function_call {
                return self.function_call();
            }

            let name = self.next_token_text();
            let var = self.find_var(name);
            if let Some(Scope::Object { is_global, idx, .. }) = var {
                let (idx, is_global, ty) = self.get_var(*is_global, *idx);
                self.advance(); // skip ident
                return Node::Variable(Variable {
                    idx,
                    is_local: !is_global,
                    ty,
                });
            } else if let Some(Scope::Enumerator { val, ty, .. }) = var {
                let val = *val;
                let ty = ty.clone();
                self.advance(); // skip ident
                return Node::Numeric(Numeric { val, ty });
            } else {
                eprintln!("Unknown variable {}", self.next_token_text());
                panic!();
            }
        }

        if self.next_token_kind_is(TokenKind::StringLiteral) {
            let tok = self.peek().cloned().unwrap();
            let v = self.str_literal_counter;
            self.str_literal_counter += 1;
            let name = format!(".L..{}", v);
            let quotes_len = 2;
            let ty = Type::array_of(Type::char_type(), tok.len - quotes_len + 1);
            let mut var = self.new_variable(&name, ty.clone(), true);
            var.as_var_object_mut().init_data = tok.string_literal.unwrap();
            let idx = self.globals.len();
            self.globals.push(var);

            self.advance(); // skip str literal
            return Node::Variable(Variable {
                idx,
                is_local: false,
                ty,
            });
        }

        if self.next_token_kind_is(TokenKind::Numeric) {
            let tok = self.peek().unwrap();
            let node = Node::Numeric(Numeric {
                val: tok.val.unwrap(),
                ty: Type::None,
            });
            self.advance();
            return node;
        }

        eprintln!("Invalid stmt {}", self.next_token_text());
        panic!();
    }

    fn function_call(&mut self) -> Node {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let idx = self.find_func_var();
        if idx.is_none() {
            eprintln!("Unknown function: {}", self.next_token_text());
            panic!();
        }
        let fn_idx = idx.unwrap();
        let name = self.next_token_text().to_string();

        self.advance();
        self.advance();

        let mut args: Vec<Node> = Vec::new();
        let mut first = true;
        let mut param_idx = 0;
        while !self.next_token_equals(")") {
            if !first {
                self.skip(",");
            }
            first = false;
            let mut assign = self.assign();
            assign.add_type();

            let f = self.get_function(fn_idx);
            let func_params_ty = &f.ty.as_func().unwrap().params;

            if param_idx < func_params_ty.len() {
                assign = Node::Cast(Unary {
                    lhs: Box::new(assign),
                    ty: func_params_ty[param_idx].ty.clone(),
                });
            }

            args.push(assign);
            param_idx += 1;
        }
        self.skip(")");

        let f = self.get_function(fn_idx);

        Node::FunctionCall(FunctionCall {
            name,
            args,
            ty: *f.ty.as_func().unwrap().return_type.clone(),
        })
    }

    fn find_var_scope(&self, var_name: &str) -> Option<&Scope> {
        for scope in self.scopes.iter().rev() {
            for varscope in scope.var_scopes.iter().rev() {
                match varscope {
                    Scope::Object { name, .. }
                    | Scope::TypeDef { name, .. }
                    | Scope::Enumerator { name, .. } => {
                        if var_name == name {
                            return Some(varscope);
                        }
                    }
                }
            }
        }
        None
    }

    fn abstract_declarator(&mut self, ty: &mut Type) -> Type {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        while self.consume("*") {
            *ty = Type::pointer_to((*ty).clone())
        }

        if self.consume("(") {
            let tokens_copy = self.tokens.get_mut().clone();
            let mut dummy = Type::void_type();
            self.abstract_declarator(&mut dummy);
            self.skip(")");
            let mut ty = self.type_suffix(ty.clone());
            // save advanced position
            let pos = self.tokens.get_mut().clone();
            self.tokens.set(tokens_copy); // reset
            let ty = self.abstract_declarator(&mut ty);
            self.tokens.set(pos); // restore
            return ty;
        }

        self.type_suffix(ty.clone())
    }

    fn typename(&mut self) -> Type {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        let mut attr = VarAttr::default();
        let mut t = self.declspec(&mut attr);
        self.abstract_declarator(&mut t)
    }

    // returns the variable for given scope
    fn get_var(&self, is_global: bool, idx: usize) -> (usize, bool, Type) {
        if is_global {
            if let Object::Var(vo) = &self.globals[idx] {
                return (idx, true, vo.ty.clone());
            }
        } else {
            let locals = self.locals.take();
            let ty = locals[idx].ty().clone();
            self.locals.set(locals);
            return (idx, false, ty);
        }
        eprintln!("Failed to get var for given scope!");
        panic!();
    }

    fn find_var(&self, name: &str) -> Option<&Scope> {
        self.find_var_scope(name)
    }

    fn find_func_var(&self) -> Option<usize> {
        let token = self.peek().unwrap();
        let text = self.text(&token);
        if let Some(Scope::Object {
            name,
            is_global,
            idx,
        }) = self.find_var_scope(text)
        {
            if *is_global {
                // if idx is greater/eq
                if *idx >= self.globals.len() {
                    // its recursion?
                    return self.current_fn.as_ref().and_then(|f| {
                        if f.name == *name {
                            Some(usize::max_value()) // recursion
                        } else {
                            None
                        }
                    });
                }

                if let Object::Function(_) = &self.globals[*idx] {
                    return Some(*idx);
                }
            }
        }
        None
    }

    fn get_function(&self, idx: usize) -> &FunctionObject {
        let f = if idx == usize::max_value() {
            // recursion
            self.current_fn.as_ref().unwrap()
        } else {
            self.globals[idx].as_function_object_ref()
        };
        f
    }

    fn is_typename(&self) -> bool {
        {
            let type_keywords = [
                "char", "int", "struct", "union", "long", "short", "void", "typedef", "_Bool",
                "enum", "static",
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
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        if self.next_token_equals(";") {
            false
        } else {
            let mut ty = Type::int_type();
            let (ty, _) = self.declarator(&mut ty);
            matches!(ty, Type::Func(_))
        }
    }

    fn consume(&self, text: &str) -> bool {
        if self.next_token_equals(text) {
            self.advance();
            true
        } else {
            false
        }
    }

    // declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
    fn declarator(&mut self, ty: &mut Type) -> (Type, String) {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        while self.consume("*") {
            *ty = Type::pointer_to((*ty).clone())
        }

        if self.next_token_equals("(") {
            self.skip("(");
            let tokens = self.tokens.get_mut().clone();
            // skip identifer, use a dummy type
            let mut dummy = Type::int_type();
            self.declarator(&mut dummy);
            self.skip(")");

            // get the type suffix
            *ty = self.type_suffix((*ty).clone());
            // save advanced position
            let moved = self.tokens.get_mut().clone();
            // get the ident, start from prev saved position
            self.tokens.set(tokens);
            let t = self.declarator(ty);
            self.tokens.set(moved); // restore
            return t;
        }

        if !self.next_token_kind_is(TokenKind::Identifier) {
            eprintln!("Expected an identifier! got '{}'", self.next_token_text());
            panic!();
        }
        let name_token = self.peek().unwrap();
        let name = self.text(&name_token).to_string();
        self.advance();
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
        self.advance();
        ty
    }

    fn type_suffix(&mut self, ty: Type) -> Type {
        let _t = TraceRaii::new();
        trace!("{}: {}", function!(), self.next_token_text());
        if self.next_token_equals("(") {
            self.skip("(");
            self.func_params(ty)
        } else if self.next_token_equals("[") {
            self.skip("[");
            if !self.next_token_kind_is(TokenKind::Numeric) {
                eprintln!("Expected a number!");
            }
            let tok = self.peek().cloned();
            let size = tok.unwrap().val.unwrap();
            self.advance(); // skip number
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

    fn next_token_equals(&self, s: &str) -> bool {
        self.next_token_text() == s
    }

    fn peek(&self) -> Option<&Token> {
        let tokens = self.tokens.take();
        if let Some(mut tokens) = tokens {
            let token = tokens.peek().cloned();
            self.tokens.set(Some(tokens));
            token
        } else {
            panic!();
        }
    }

    fn skip(&self, s: &str) {
        if !self.next_token_equals(s) {
            eprintln!("Expected {s}, got '{}'", self.next_token_text());
            panic!();
        }
        self.advance();
    }

    fn next_token_kind_is(&self, kind: TokenKind) -> bool {
        if let Some(tok) = self.peek() {
            tok.kind == kind
        } else {
            false
        }
    }

    fn advance(&self) {
        let tokens = self.tokens.take();
        if let Some(mut tokens) = tokens {
            tokens.next();
            self.tokens.set(Some(tokens));
        } else {
            panic!();
        }
    }

    fn next_token_text(&self) -> &str {
        if let Some(tok) = self.peek() {
            tok.text(self.source)
        } else {
            panic!()
        }
    }

    fn next_token_is_typename(&self) -> bool {
        self.is_typename()
    }
}
