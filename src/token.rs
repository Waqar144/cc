#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenKind {
    Keyword,
    Reserved,
    Numeric,
    Identifier,
    StringLiteral,
    TOKEOF,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub start: usize,
    pub len: usize,
    pub val: Option<usize>,
    pub line_no: usize,
    pub string_literal: Option<String>,
    pub kind: TokenKind,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}->{}, line:{}, [{:?}]",
            self.start,
            self.start + self.len,
            self.line_no,
            self.kind,
        )?;
        if let Some(val) = self.val {
            write!(f, ", {}", val)?;
        }
        if let Some(l) = &self.string_literal {
            write!(f, ", \"{}\"", l)?;
        }

        Ok(())
    }
}

impl Token {
    pub fn op(start: usize, len: usize, line_no: usize) -> Token {
        Token {
            start,
            len,
            val: None,
            line_no,
            string_literal: None,
            kind: TokenKind::Reserved,
        }
    }

    pub fn identifier(start: usize, len: usize, line_no: usize, is_keyword: bool) -> Token {
        Token {
            start,
            len,
            val: None,
            line_no,
            string_literal: None,
            kind: if is_keyword {
                TokenKind::Keyword
            } else {
                TokenKind::Identifier
            },
        }
    }

    pub fn digit(start: usize, len: usize, val: usize, line_no: usize) -> Token {
        Token {
            start,
            len,
            val: Some(val),
            line_no,
            string_literal: None,
            kind: TokenKind::Numeric,
        }
    }

    pub fn string_literal(start: usize, len: usize, line_no: usize, val: String) -> Token {
        Token {
            start,
            len,
            val: None,
            line_no,
            string_literal: Some(val),
            kind: TokenKind::StringLiteral,
        }
    }

    #[allow(dead_code)]
    pub fn text<'a>(&'a self, source: &'a str) -> &str {
        &source[self.start..self.start + self.len]
    }
}
