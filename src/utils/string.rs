pub fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('0') => result.push('\0'),
                Some('\'') => result.push('\''),
                Some('\"') => result.push('\"'),
                Some('x') => {
                    let hex_chars: String = chars.by_ref().take(2).collect();
                    if hex_chars.len() == 2 {
                        if let Ok(value) = u8::from_str_radix(&hex_chars, 16) {
                            result.push(value as char);
                        } else {
                            result.push('\\');
                            result.push('x');
                            result.push_str(&hex_chars);
                        }
                    } else {
                        result.push('\\');
                        result.push('x');
                        result.push_str(&hex_chars);
                    }
                }
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(ch);
        }
    }

    result
}

pub fn parse_quoted_content(lexeme: &str, quote_char: char) -> Option<(String, Option<String>)> {
    let parts: Vec<&str> = lexeme.split(quote_char).collect();
    if parts.len() < 3 {
        return None;
    }

    let content = parts[1];
    let suffix = parts[2];

    let unescaped_content = unescape_string(content);
    let suffix = if suffix.is_empty() {
        None
    } else {
        Some(suffix.to_owned())
    };

    Some((unescaped_content, suffix))
}

pub fn parse_number_literal(lexeme: &str) -> (String, Option<String>) {
    let suffix_pos = if let Some(stripped) = lexeme.strip_prefix("0b") {
        stripped
            .find(|c| c != '0' && c != '1' && c != '_')
            .map(|x| x + 2)
    } else if let Some(stripped) = lexeme.strip_prefix("0o") {
        stripped
            .find(|c| !('0'..='7').contains(&c) && c != '_')
            .map(|x| x + 2)
    } else if let Some(stripped) = lexeme.strip_prefix("0x") {
        stripped
            .find(|c: char| !c.is_ascii_hexdigit() && c != '_')
            .map(|x| x + 2)
    } else {
        lexeme.find(|c: char| !c.is_ascii_digit() && c != '_')
    };

    let symbol = suffix_pos.map_or(lexeme.to_owned(), |pos| lexeme[..pos].to_owned());
    let suffix = suffix_pos.map(|pos| lexeme[pos..].to_owned());

    (symbol, suffix)
}
