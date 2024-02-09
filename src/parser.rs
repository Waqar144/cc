#![allow(dead_code)]
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

struct Object {
    name: String,
    offset: i32,
    ty: Box<Type>, // TODO
    is_local: bool,
    is_func: bool,
    is_func_def: bool,

    // function
    locals: Vec<Box<Object>>,
    params: Vec<Box<Object>>,
    body: Vec<Box<Node>>, // TODO
    // std::vector<NodePtr> body;
    stack_size: usize,
    init_data: String,
    // Type* type = nullptr;
}

pub struct Parser<'a> {
    source: &'a str,
    globals: Vec<Object>,
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

            if self.is_function() {
                //
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

    fn declspec(&mut self) -> Type {
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

    fn function(&mut self) {
        //
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
                Type::Func {
                    return_type,
                    params,
                } => true,
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
    fn declarator(&mut self, ty: &mut Type) -> (Type, &str) {
        while self.consume("*") {
            *ty = pointer_to(*ty)
        }

        if self.next_token_equals("(") {
            self.skip("(");
            // skip identifer, use a dummy type
            let mut dummy = Type::NoType;
            self.declarator(&mut dummy);

            self.skip(")");
        }

        if !self.next_token_kind_is(TokenKind::Identifier) {
            eprintln!("Expected an identifier!");
        }
        let name_token = self.peek().unwrap();
        let name = self.text(&name_token);
        self.tokens.next();
        let ty = self.type_suffix(*ty);
        (ty, name)
    }

    fn func_params(&mut self, return_ty: Type) -> Type {
        let mut params: Vec<Type> = Vec::new();
        while !self.next_token_equals(")") {
            if self.next_token_equals(",") {
                self.skip(",");
            }
            let mut base_type = self.declspec();
            let (ty, _) = self.declarator(&mut base_type);
            params.push(ty);
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
            if let Some(tok) = tok {
                if tok.kind != TokenKind::Numeric {
                    eprintln!("Expected a number!");
                }
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

    fn text(&self, token: &Token) -> &str {
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
