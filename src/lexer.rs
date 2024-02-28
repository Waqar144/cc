use crate::token::Token;

fn is_start_of_identifier(c: char) -> bool {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

fn is_identifier(c: char) -> bool {
    return is_start_of_identifier(c) || ('0' <= c && c <= '9');
}

fn is_octal_char(c: char) -> bool {
    '0' <= c && c <= '7'
}

fn is_keyword(span: &str) -> bool {
    match span {
        "if" | "else" | "return" | "for" | "while" | "int" | "sizeof" | "struct" | "union"
        | "long" | "short" | "void" | "char" | "typedef" => true,
        _ => false,
    }
}

fn read_escaped_char(source: &str) -> (char, usize) {
    let mut source = source;

    // octal
    if source.starts_with(|c| is_octal_char(c)) {
        let c = source.chars().nth(0).unwrap();
        let mut num = c.to_digit(8).unwrap();
        let mut count = 1;
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
        let count = 1;
        for c in source.chars() {
            if !c.is_ascii_hexdigit() || count >= 2 {
                break;
            }
            let number = c.to_digit(16).unwrap();
            num = (num << 4) + number;
        }
        return (num as u8 as char, count);
    }

    let c = source.chars().nth(0).unwrap();
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
    return (num, 1);
}

fn read_string_literal(mut source: &str) -> Result<(usize, String), &str> {
    let mut string = String::new();
    let start = source;
    loop {
        let c = source.chars().nth(0).unwrap();

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

        let Some(c) = span.chars().nth(0) else {
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
            for (idx, c) in span.char_indices() {
                if !c.is_ascii_digit() {
                    // println!("break at {c} {idx} - digit: {}", &span[0..idx]);
                    let num_span = &span[..idx];
                    let val = num_span.parse::<usize>().unwrap();
                    span = &span[idx..];
                    tokens.push(Token::digit(offset, idx, val, line));
                    offset += idx;
                    break;
                }
            }
            continue;
        }

        // 2 char operators
        if span.starts_with("==")
            || span.starts_with("<=")
            || span.starts_with(">=")
            || span.starts_with("!=")
            || span.starts_with("->")
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

        if c == '+' || c == '-' || c.is_ascii_punctuation() {
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
