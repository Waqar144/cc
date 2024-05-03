use crate::token::Token;

fn is_start_of_identifier(c: char) -> bool {
    c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
}

fn is_identifier(c: char) -> bool {
    is_start_of_identifier(c) || c.is_ascii_digit()
}

fn is_octal_char(c: char) -> bool {
    ('0'..='7').contains(&c)
}

fn is_keyword(span: &str) -> bool {
    matches!(
        span,
        "if" | "else"
            | "return"
            | "for"
            | "while"
            | "int"
            | "sizeof"
            | "struct"
            | "union"
            | "long"
            | "short"
            | "void"
            | "char"
            | "typedef"
            | "_Bool"
            | "enum"
            | "static"
    )
}

fn is_punct(c: char) -> bool {
    (c == '+' || c == '-' || c.is_ascii_punctuation()) && c != '_' && c != '\''
}

fn read_escaped_char(source: &str) -> (char, usize) {
    let mut source = source;

    // octal
    if source.starts_with(is_octal_char) {
        let mut num = 0;
        let mut count = 0;
        for c in source.chars() {
            if !is_octal_char(c) || count >= 3 {
                break;
            }
            let number = c.to_digit(8).unwrap();
            num = (num << 3) + number;
            count += 1;
        }
        return (num as u8 as char, count);
    }

    // hex
    if source.starts_with('x') {
        source = &source[1..]; // skip x
        let mut num: u32 = 0;
        let mut count = 1; // start at 1 to account for x in the beginning
        for c in source.chars() {
            if !c.is_ascii_hexdigit() || count >= 3 {
                break;
            }
            let number = c.to_digit(16).unwrap();
            num = (num << 4) + number;
            count += 1;
        }
        return (num as u8 as char, count);
    }

    let c = source.chars().next().unwrap();
    let num = match c {
        'a' => 7,
        'b' => 8,
        't' => 9,
        'n' => 10,
        'v' => 11,
        'f' => 12,
        'r' => 13,
        'e' => 27,
        _ => c as u8,
    } as char;
    (num, 1)
}

fn read_string_literal(mut source: &str) -> Result<(usize, String), &str> {
    let mut string = String::new();
    let start = source;
    loop {
        let c = source.chars().next().unwrap();

        if c == '"' {
            break;
        }

        if c == '\n' || c == '\0' {
            // exit with error
            return Err("unclosed string literal");
        }

        // parse escape sequence
        if c == '\\' {
            // skip \
            source = &source[1..];

            let (ch, read_chars) = read_escaped_char(source);
            string.push(ch);
            source = &source[read_chars..];
            continue;
        }

        string.push(c);
        source = &source[1..];
    }

    let count = start.len() - source.len();

    Ok((count, string))
}

fn read_char_literal(source: &str) -> Result<(char, usize), &str> {
    let source = &source[1..]; // skip '
    let c = source.chars().next().ok_or("Unclosed char literal")?;
    let val;
    let read_chars;
    if c == '\\' {
        let source = &source[1..]; // skip \
        let (v, count) = read_escaped_char(source);
        read_chars = count + 1;
        val = v;
    } else {
        val = c;
        read_chars = 1;
    }

    Ok((val, read_chars))
}

fn read_int_literal(source: &str) -> Result<(usize, usize), String> {
    fn is_alnum(source: &str, i: usize) -> bool {
        source
            .chars()
            .nth(i)
            .map(|c| c.is_alphanumeric())
            .unwrap_or(false)
    }

    let (base, advance) =
        if (source.starts_with("0x") || source.starts_with("0X")) && is_alnum(source, 2) {
            (16, 2)
        } else if (source.starts_with("0b") || source.starts_with("0B")) && is_alnum(source, 2) {
            (2, 2)
        } else if source.starts_with('0') {
            (8, 0)
        } else {
            (10, 0)
        };

    let start = &source[advance..];
    let count = start.chars().take_while(|c| c.is_digit(base)).count();
    let num_span = &start[..count];
    let num = usize::from_str_radix(num_span, base).map_err(|e| e.to_string())?;
    Ok((num, count + advance))
}

pub fn tokenize(source: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut line = 0;
    let mut span = source;
    let mut offset = 0;
    loop {
        // single line comment
        if span.starts_with("//") {
            let count = span.find('\n').unwrap();
            span = &span[count..];
            offset += count;
            continue;
        }

        // multi line comment
        if span.starts_with("/*") {
            let count = span.find("*/").unwrap() + 2;
            span = &span[count..];
            offset += count;
            continue;
        }

        let Some(c) = span.chars().next() else {
            // println!("Reached end? {offset}");
            break;
        };

        // Skip white space
        if c == ' ' || c == '\n' {
            if c == '\n' {
                line += 1;
            }
            span = &span[1..];
            offset += 1;
            continue;
        }

        if c.is_ascii_digit() {
            let (num, read_chars) = read_int_literal(span)
                .map_or_else(|err| exit_with_error(&err, line, source, offset), |ok| ok);
            tokens.push(Token::digit(offset, read_chars, num, line));
            offset += read_chars;
            span = &span[read_chars..];
            continue;
        }

        // 2 char operators
        if span.starts_with("==")
            || span.starts_with("<=")
            || span.starts_with(">=")
            || span.starts_with("!=")
            || span.starts_with("->")
            || span.starts_with("+=")
            || span.starts_with("-=")
            || span.starts_with("*=")
            || span.starts_with("/=")
            || span.starts_with("++")
            || span.starts_with("--")
        {
            span = &span[2..];
            tokens.push(Token::op(offset, 2, line));
            offset += 2;
            continue;
        }

        // string
        if c == '"' {
            let start = offset;
            let string_literal_start = &span[1..]; // skip "
            let (read_chars, string) = read_string_literal(string_literal_start)
                .map_or_else(|err| exit_with_error(err, line, source, offset), |ok| ok);
            let len = string.len() + 2; // 2 because there are 2 quote marks
            offset += read_chars + 2;
            tokens.push(Token::string_literal(start, len, line, string));
            span = &span[read_chars + 2..]; // 2 because of quote marks
            continue;
        }

        if c == '\'' {
            let (val, read_chars) = read_char_literal(span)
                .map_or_else(|err| exit_with_error(err, line, source, offset), |ok| ok);
            tokens.push(Token::digit(
                offset,
                read_chars + 2,
                val as i8 as usize,
                line,
            ));
            offset += read_chars + 2;
            span = &span[read_chars + 2..]; // 2 ''
            continue;
        }

        if is_punct(c) {
            span = &span[1..];
            tokens.push(Token::op(offset, 1, line));
            offset += 1;
            continue;
        }

        if is_start_of_identifier(c) {
            for (idx, c) in span.char_indices() {
                if !is_identifier(c) {
                    // println!("break at {c} {idx} - Identifier: {}", &span[0..idx]);
                    tokens.push(Token::identifier(
                        offset,
                        idx,
                        line,
                        is_keyword(&span[0..idx]),
                    ));
                    span = &span[idx..];
                    offset += idx;
                    break;
                }
            }
            continue;
        }

        exit_with_error(&format!("Unexpected char {c}"), line, source, offset);
    }

    tokens.push(Token {
        start: 0,
        len: 0,
        val: None,
        line_no: line,
        string_literal: None,
        kind: crate::token::TokenKind::TOKEOF,
    });

    tokens
}

fn exit_with_error(error: &str, line: usize, source: &str, offset: usize) -> ! {
    // let mut span = &source[offset..];

    let line_start = source[..offset].rfind('\n').unwrap_or(0);
    let span = if let Some(new_line) = source[offset..].find('\n') {
        &source[line_start..(offset + new_line)]
        // span = &span[..new_line];
    } else {
        &source[line_start..]
    };

    eprintln!("{}", span);
    eprintln!("{:>offset$}", "^", offset = offset - line_start);
    eprintln!("Error:{line}: {error}");
    std::process::exit(1);
}

#[test]
fn test_tokenize() {
    use crate::token::TokenKind;
    let src = r#"int main() {
    // abc
    /* multi
     * line
     */
    "hello";
    return 420;
}
"#;
    let toks = tokenize(src);

    #[rustfmt::skip]
    let expected = vec![
        Token { start: 0, len: 3, val: None, line_no: 0, string_literal: None, kind: TokenKind::Keyword },
        Token { start: 4, len: 4, val: None, line_no: 0, string_literal: None, kind: TokenKind::Identifier },
        Token { start: 8, len: 1, val: None, line_no: 0, string_literal: None, kind: TokenKind::Reserved },
        Token { start: 9, len: 1, val: None, line_no: 0, string_literal: None, kind: TokenKind::Reserved },
        Token { start: 11, len: 1, val: None, line_no: 0, string_literal: None, kind: TokenKind::Reserved },
        Token { start: 61, len: 7, val: None, line_no: 3, string_literal: Some("hello".into()), kind: TokenKind::StringLiteral },
        Token { start: 68, len: 1, val: None, line_no: 3, string_literal: None, kind: TokenKind::Reserved },
        Token { start: 74, len: 6, val: None, line_no: 4, string_literal: None, kind: TokenKind::Keyword },
        Token { start: 81, len: 3, val: Some(420), line_no: 4, string_literal: None, kind: TokenKind::Numeric },
        Token { start: 84, len: 1, val: None, line_no: 4, string_literal: None, kind: TokenKind::Reserved },
        Token { start: 86, len: 1, val: None, line_no: 5, string_literal: None, kind: TokenKind::Reserved },
        Token { start: 0, len: 0, val: None, line_no: 6, string_literal: None, kind: TokenKind::TOKEOF }
    ];

    assert_eq!(toks.len(), expected.len());
    for (left, right) in toks.iter().zip(expected.iter()) {
        assert_eq!(left, right);
    }
}

#[test]
fn test_str_literal_with_newline() {
    use crate::token::TokenKind;
    let src = r#"
    "OK\n";
"#;
    let toks = tokenize(src);

    #[rustfmt::skip]
    let expected = vec![
        Token { start: 5, len: 5, val: None, line_no: 1, string_literal: Some("OK\n".into()), kind: TokenKind::StringLiteral },
        Token { start: 11, len: 1, val: None, line_no: 1, string_literal: None, kind: TokenKind::Reserved},
        Token { start: 0, len: 0, val: None, line_no: 2, string_literal: None, kind: TokenKind::TOKEOF },
    ];

    assert_eq!(toks.len(), expected.len());
    for (left, right) in toks.iter().zip(expected.iter()) {
        assert_eq!(left, right);
    }
}
