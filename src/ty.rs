#![allow(dead_code)]
#![allow(unused)]
use crate::parser::StructMember;

#[derive(Clone)]
pub struct FuncParamType {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone)]
pub struct FuncType {
    pub return_type: Box<Type>,
    pub params: Vec<FuncParamType>,
}

#[derive(Clone)]
pub enum Type {
    Void {
        size: usize,
        alignment: usize,
    },
    Char {
        size: usize,
        alignment: usize,
    },
    Short {
        size: usize,
        alignment: usize,
    },
    Int {
        size: usize,
        alignment: usize,
    },
    Long {
        size: usize,
        alignment: usize,
    },
    Ptr {
        size: usize,
        alignment: usize,
        base: Box<Type>,
    },
    Func(FuncType),
    Array {
        size: usize,
        alignment: usize,
        array_len: usize,
        base: Box<Type>,
    },
    Struct {
        size: usize,
        alignment: usize,
        members: Vec<StructMember>,
    },
    Union {
        size: usize,
        alignment: usize,
        members: Vec<StructMember>,
    },
    NoType,
}
pub fn void_type() -> Type {
    Type::Void {
        size: 1,
        alignment: 1,
    }
}
pub fn int_type() -> Type {
    Type::Void {
        size: 4,
        alignment: 4,
    }
}
pub fn char_type() -> Type {
    Type::Void {
        size: 1,
        alignment: 1,
    }
}
pub fn long_type() -> Type {
    Type::Void {
        size: 8,
        alignment: 8,
    }
}

pub fn pointer_to(base: Type) -> Type {
    Type::Ptr {
        size: 8,
        alignment: 8,
        base: Box::new(base),
    }
}

pub fn func_type(return_type: Type, params: Vec<FuncParamType>) -> Type {
    Type::Func(FuncType {
        return_type: Box::new(return_type),
        params,
    })
}

pub fn array_of(base_ty: Type, len: usize) -> Type {
    let (size, alignment) = match &base_ty {
        Type::Void { size, alignment } => (size, alignment),
        Type::Char { size, alignment } => (size, alignment),
        Type::Short { size, alignment } => (size, alignment),
        Type::Int { size, alignment } => (size, alignment),
        Type::Long { size, alignment } => (size, alignment),
        Type::Ptr {
            size,
            alignment,
            base,
        } => (size, alignment),
        Type::Array {
            size,
            alignment,
            array_len,
            base,
        } => (size, alignment),
        Type::Struct {
            size,
            alignment,
            members,
        } => (size, alignment),
        Type::Union {
            size,
            alignment,
            members,
        } => (size, alignment),
        _ => panic!(),
    };

    Type::Array {
        size: size * len,
        alignment: *alignment,
        array_len: len,
        base: Box::new(base_ty),
    }
}
