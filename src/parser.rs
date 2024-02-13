#![allow(dead_code)]
#![allow(unused)]

use std::cell::Cell;

use crate::token::{Token, TokenKind};
use crate::ty::*;

struct Block {
    token: Token,
    block_body: Vec<Node>,
}

struct Variable {
    var: Object,
}

enum Node {
    Block(Block),
    Variable(Variable),
}

enum NodeType {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Eq,
    NotEq,
    LessThan,
    LessEq,
    ExprStmt,
    StmtExpr, // GNU Statement Expression
    Assign,
    Comma,
    Variable,
    Return,
    Block,
    If,
    For,
    While,
    AddressOf,
    Dereference,
    FunctionCall,
    Number,
    StructMembr,
    Cast,
}

#[derive(Clone, Debug)]
pub struct StructMember {
    name: String,
    ty: Box<Type>, // TODO
    offset: usize,
}

struct FunctionObject {
    name: String,
    locals: Vec<VarObject>,
    params: Vec<Object>,
    is_func_def: bool,
    body: Vec<Node>,
}

#[derive(Clone)]
struct VarObject {
    name: String,
    ty: Type,
}

enum Object {
    FunctionObject(FunctionObject),
    VarObject(VarObject),
    Invalid,
}

impl Object {
    fn as_var_object(self) -> VarObject {
        if let Self::VarObject(v) = self {
            v
        } else {
            panic!()
        }
    }
}

struct VarScope<'a> {
    name: &'a str,
    var: &'a Object,
    typedef: Option<Type>,
}

struct TagScope<'a> {
    name: &'a str,
    ty: Type,
}

enum Scope {
    Object { name: String },
    TypeDef { name: String, typedef: Type },
}

#[derive(Default)]
struct VarAttr {
    is_typedef: bool,
}

// Block level scope
#[derive(Default)]
struct Scopes<'a> {
    var_scopes: Vec<Scope>,
    tag_scopes: Vec<TagScope<'a>>,
}

pub struct Parser<'a> {
    source: &'a str,
    globals: Vec<Object>,
    scopes: Vec<Scopes<'a>>,
    locals: Cell<Vec<VarObject>>,
    tokens: Cell<std::iter::Peekable<std::slice::Iter<'a, Token>>>,
}

impl Parser<'_> {
    pub fn new<'a>(source: &'a str, tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser {
            source,
            globals: Vec::new(),
            scopes: Vec::new(),
            locals: Cell::new(Vec::new()),
            tokens: tokens.iter().peekable().into(),
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
            return;
        }
        self.tokens.get_mut().next();
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
            // let text = self.text_mut(token);
            let text = self.text(&token);

            if !self.is_typename(text) {
                break;
            }

            if text == "struct" || text == "union" {
                // TODO
            }

            if text == "void" {
                counter += CTypes::VOID as usize;
            } else if text == "char" {
                counter += CTypes::CHAR as usize;
            } else if text == "short" {
                counter += CTypes::SHORT as usize;
            } else if text == "int" {
                counter += CTypes::INT as usize;
            } else if text == "long" {
                counter += CTypes::LONG as usize;
            } else {
                // PRINT_ERROR(toks->start, "unexpected type, bug..");
            }

            ty = match counter {
                x if x == CTypes::VOID as usize => void_type(),
                x if x == CTypes::CHAR as usize => char_type(),
                x if x == CTypes::SHORT as usize
                    || x == (CTypes::SHORT as usize) + (CTypes::INT as usize) =>
                {
                    int_type()
                }
                x if x == CTypes::INT as usize => int_type(),
                x if x == CTypes::LONG as usize
                    || x == (CTypes::LONG as usize) + (CTypes::INT as usize)
                    || x == (CTypes::LONG as usize) + (CTypes::LONG as usize) =>
                {
                    long_type()
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

    fn push_var_scope<'a>(&mut self, name: String) {
        assert!(!self.scopes.is_empty());
        self.scopes
            .last_mut()
            .unwrap()
            .var_scopes
            .push(Scope::Object { name });
    }

    fn push_typedef_scope<'a>(&mut self, name: String, ty: Type) {
        assert!(!self.scopes.is_empty());
        self.scopes
            .last_mut()
            .unwrap()
            .var_scopes
            .push(Scope::TypeDef { name, typedef: ty });
    }

    fn new_variable(&mut self, name: &str, ty: Type) -> Object {
        let obj = Object::VarObject(VarObject {
            name: name.to_string(),
            ty,
        });
        self.push_var_scope(name.to_string());
        obj
    }

    fn create_param_local_vars(&mut self, param_types: &Vec<FuncParamType>) -> Vec<Object> {
        let mut locals: Vec<Object> = Vec::new();
        let mut fn_locals = Vec::new();
        for ty in param_types {
            locals.push(self.new_variable(&ty.name, ty.ty.clone()));
            // Copy into function locals, dont use new_variable because we dont' want to duplicate varscope
            let obj = VarObject {
                name: ty.name.clone(),
                ty: ty.ty.clone(),
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
        });

        self.push_var_scope(ident.clone());

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
            if self.consume("{") {
                self.tokens.get_mut().next();
            }

            println!("compound_stmt...");
            let body = self.compound_stmt();

            self.leave_scope();
            return Object::FunctionObject(FunctionObject {
                name: ident.to_string(),
                locals: self.locals.take(),
                params,
                is_func_def: false,
                body: vec![body],
            });
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

            let is_typename = self
                .peek()
                .map(|t| self.is_typename(self.text(&t)))
                .unwrap_or(false);
            if (is_typename) {
                let mut attr = VarAttr::default();
                let base_ty = self.declspec(&mut attr);
                if (attr.is_typedef) {
                    // parse_typedef
                    self.parse_typedef(base_ty);
                    continue;
                }
                self.declaration(base_ty);
            } else {
                // stmt
            }
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

    fn declaration(&mut self, base_ty: Type) {
        let mut first = true;
        while (!self.next_token_equals(";")) {
            if !first {
                self.skip(",");
            }

            let mut base_ty = base_ty.clone();
            let (ty, name) = self.declarator(&mut base_ty);
            if let Type::Void { size, alignment } = ty {
                eprintln!("unexpected void type"); // TODO proper error reporting
            }

            let var = self.new_variable(&name, ty.clone());
            let mut locals = self.locals.take();
            locals.push(var.as_var_object());

            if !self.next_token_equals("-") {
                continue;
            }

            let lhs = Node::Variable(Variable {
                var: self.new_variable(&name, ty),
            });
        }
    }

    fn is_typename(&self, token_text: &str) -> bool {
        let type_keywords = [
            "char", "int", "struct", "union", "long", "short", "void", "typedef",
        ];

        for k in type_keywords {
            if token_text == k {
                return true;
            }
        }

        false
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
            *ty = pointer_to((*ty).clone())
        }

        if self.next_token_equals("(") {
            self.skip("(");
            // skip identifer, use a dummy type
            let mut dummy = Type::NoType;
            self.declarator(&mut dummy);

            // TODO

            self.skip(")");
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
        let ty = func_type(return_ty, params);
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
            array_of(ty, size)
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
}
