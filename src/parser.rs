#![allow(dead_code)]
#![allow(unused)]
use crate::token::{Token, TokenKind};
use crate::ty::*;

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

#[derive(Clone)]
pub struct StructMember {
    name: String,
    ty: Box<Type>, // TODO
    offset: usize,
    // Type* type;
    // int offset;
}

struct Node {
    // using NodePtr = std::unique_ptr<Node>;
    // using TokenIterator = std::vector<Token>::const_iterator;
    // TokenIterator token;
    /* Position in source */
    token: Token,

    /* Type of node */
    // NodeType nodeType;
    node_type: NodeType,

    /* Value if node is a 'Number' node */
    // int64_t val = -1;
    val: isize,

    left: Box<Node>,
    right: Box<Node>,
    // NodePtr left = nullptr;
    // NodePtr right = nullptr;

    // variable
    // Object* var = nullptr; // shared between multiple nodes, TODO: revisit this (maybe use shared_ptr<>)
    var: Box<Object>,

    // if stmt
    cond: Box<Node>,
    then: Box<Node>,
    els: Box<Node>,
    // NodePtr cond = nullptr;
    // NodePtr then = nullptr;
    // NodePtr els = nullptr; // else
    init: Box<Node>,
    inc: Box<Node>,
    // NodePtr init = nullptr;
    // NodePtr inc = nullptr;
    ty: Box<Type>,
    // Type* type = nullptr;
    member: StructMember,
    // StructMember member;
    block_body: Vec<Box<Node>>,
    // std::vector<NodePtr> blockBody;
    func_name: String,
    // std::string_view funcName;
    args: Vec<Box<Node>>,
    // std::vector<NodePtr> args;
}

struct FunctionObject {
    name: String,
    locals: Vec<Object>,
    params: Vec<Object>,
    is_func_def: bool,
    body: Vec<Box<Node>>,
}

struct VarObject {
    name: String,
    ty: Type,
}

enum Object {
    FunctionObject(FunctionObject),
    VarObject(VarObject),
    Invalid,
}

// struct Object {
//     name: String,
//     offset: i32,
//     ty: Box<Type>, // TODO
//     is_local: bool,
//     is_func: bool,
//     is_func_def: bool,
//
//     // function
//     locals: Vec<Box<Object>>,
//     params: Vec<Box<Object>>,
//     body: Vec<Box<Node>>, // TODO
//     // std::vector<NodePtr> body;
//     stack_size: usize,
//     init_data: String,
//     // Type* type = nullptr;
// }

struct VarScope<'a> {
    name: &'a str,
    var: Box<Object>,
    type_def: Option<Type>,
}

struct TagScope<'a> {
    name: &'a str,
    ty: Type,
}

#[derive(Default)]
struct VarAttr {
    is_typedef: bool,
}

pub struct Parser<'a> {
    source: &'a str,
    globals: Vec<Object>,
    var_scopes: Vec<VarScope<'a>>,
    tag_scopes: Vec<TagScope<'a>>,
    tokens: std::iter::Peekable<std::slice::Iter<'a, Token>>,
}

// struct TokenIterator<'a> {
//     tokens: &'a [Token],
// }
//
// impl TokenIterator<'_> {
//     fn peek(&self) -> Option<&Token> {
//         self.tokens.first()
//     }
// }

// impl<'a> Iterator for TokenIterator<'a> {
//     type Item = &'a Token;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         self.tokens.first()
//     }
// }

impl Parser<'_> {
    pub fn new<'a>(source: &'a str, tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser {
            source,
            globals: Vec::new(),
            var_scopes: Vec::new(),
            tag_scopes: Vec::new(),
            tokens: tokens.iter().peekable(),
        }
    }

    pub fn parse(&mut self) {
        loop {
            if let Some(tok) = self.tokens.peek() {
                if tok.kind == TokenKind::TOKEOF {
                    break;
                }
            }

            let mut attr = VarAttr::default();
            if attr.is_typedef {
                // parse_typedef(s, it, baseType);
                continue;
            }

            let base_type = self.declspec(&mut attr);

            if self.is_function() {
                // self.function();
            }
        }

        // while let Some(token) = tok_iter.next() {
        //     if token.kind == TokenKind::TOKEOF {
        //         break;
        //     }
        //
        //     self.declspec(&mut tok_iter);
        // }
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned().cloned()
    }

    fn skip(&mut self, s: &str) {
        if !self.next_token_equals(s) {
            eprintln!("Expected {s}");
            return;
        }
        self.tokens.next();
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

            self.tokens.next(); // advance
        }

        ty
    }

    fn new_variable(name: &str, ty: Type) -> Object {
        Object::VarObject(VarObject {
            name: name.to_string(),
            ty,
        })
        // push_scope();
    }

    fn create_param_local_vars(&mut self, param_types: &Vec<FuncParamType>) -> Vec<Object> {
        let mut locals: Vec<Object> = Vec::new();
        for ty in param_types {
            locals.push(Self::new_variable(&ty.name, ty.ty.clone()));
        }
        locals
    }

    fn function(&mut self, base_ty: Type) -> Object {
        let mut base_ty = base_ty;
        let (ty, ident) = self.declarator(&mut base_ty);

        // enter scope

        let is_func_def = !self.consume(";");
        if !is_func_def {
            return Object::FunctionObject(FunctionObject {
                name: ident.to_string(),
                locals: Vec::new(),
                params: Vec::new(),
                is_func_def: false,
                body: Vec::new(),
            });
        }

        if let Type::Func(func) = ty {
            let params = self.create_param_local_vars(&func.params);
            if self.consume("{") {
                self.tokens.next();
            }

            // call -> compound stmt

            return Object::FunctionObject(FunctionObject {
                name: ident.to_string(),
                locals: Vec::new(),
                params,
                is_func_def: false,
                body: Vec::new(),
            });
        }

        eprintln!("Expected function type!");
        // leave scope
        Object::Invalid
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
            let (ty, _) = self.declarator(&mut ty);
            match ty {
                Type::Func(_) => true,
                _ => false,
            }
        }
    }

    fn consume(&mut self, text: &str) -> bool {
        if let Some(token) = self.peek() {
            if self.text(&token) == text {
                self.tokens.next();
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
        self.tokens.next();
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
        self.tokens.next();
        ty
    }

    fn type_suffix(&mut self, ty: Type) -> Type {
        if self.next_token_equals("(") {
            self.func_params(ty)
        } else if self.next_token_equals("[") {
            self.skip("[");
            let tok = self.peek();
            if !self.next_token_kind_is(TokenKind::Numeric) {
                eprintln!("Expected a number!");
            }
            let size = tok.unwrap().val.unwrap();
            self.tokens.next(); // skip number
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
        if let Some(tok) = self.peek() {
            self.text(&tok) == s
        } else {
            false
        }
    }

    fn next_token_kind_is(&mut self, kind: TokenKind) -> bool {
        if let Some(tok) = self.peek() {
            tok.kind == kind
        } else {
            false
        }
    }
}
