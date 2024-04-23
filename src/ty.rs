#[derive(Clone, Debug)]
pub struct FuncParamType {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct FuncType {
    pub return_type: Box<Type>,
    pub params: Vec<FuncParamType>,
}

#[derive(Clone, Debug)]
pub struct StructMember {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
}

#[derive(Clone, Debug)]
pub enum Type {
    Void {
        size: usize,
        alignment: usize,
    },
    Bool {
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
    Enum {
        size: usize,
        alignment: usize,
    },
    NoType,
}

impl Type {
    pub fn void_type() -> Type {
        Type::Void {
            size: 1,
            alignment: 1,
        }
    }
    pub fn int_type() -> Type {
        Type::Int {
            size: 4,
            alignment: 4,
        }
    }
    pub fn char_type() -> Type {
        Type::Char {
            size: 1,
            alignment: 1,
        }
    }
    pub fn long_type() -> Type {
        Type::Long {
            size: 8,
            alignment: 8,
        }
    }
    pub fn enum_type() -> Type {
        Type::Enum {
            size: 4,
            alignment: 4,
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
        let (size, alignment) = base_ty.size_and_alignment();

        Type::Array {
            size: size * len,
            alignment,
            array_len: len,
            base: Box::new(base_ty),
        }
    }

    pub fn size(&self) -> usize {
        self.size_and_alignment().0
    }

    pub fn alignment(&self) -> usize {
        self.size_and_alignment().1
    }

    fn size_and_alignment(&self) -> (usize, usize) {
        match *self {
            Type::Void { size, alignment } => (size, alignment),
            Type::Char { size, alignment } => (size, alignment),
            Type::Short { size, alignment } => (size, alignment),
            Type::Int { size, alignment } => (size, alignment),
            Type::Long { size, alignment } => (size, alignment),
            Type::Enum { size, alignment } => (size, alignment),
            Type::Ptr {
                size, alignment, ..
            } => (size, alignment),
            Type::Array {
                size, alignment, ..
            } => (size, alignment),
            Type::Struct {
                size, alignment, ..
            } => (size, alignment),
            Type::Union {
                size, alignment, ..
            } => (size, alignment),
            Type::Bool { size, alignment } => (size, alignment),
            Type::Func(_) => panic!(),
            Type::NoType => panic!(),
        }
    }

    pub fn has_type(&self) -> bool {
        match self {
            Type::NoType => false,
            _ => true,
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(
            self,
            Self::Int { .. }
                | Self::Char { .. }
                | Self::Short { .. }
                | Self::Long { .. }
                | Self::Bool { .. }
        )
    }

    pub fn base_ty(&self) -> Option<&Box<Type>> {
        match self {
            Type::Ptr { base, .. } | Type::Array { base, .. } => Some(base),
            _ => None,
        }
    }

    /// Returns `true` if the type is [`Array`].
    ///
    /// [`Array`]: Type::Array
    #[must_use]
    pub fn is_array(&self) -> bool {
        matches!(self, Self::Array { .. })
    }

    pub fn as_func(&self) -> Option<&FuncType> {
        if let Self::Func(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
