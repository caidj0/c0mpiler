use std::collections::HashMap;

macro_rules! define_rules {
    ($($id:ident: $rule:literal), *) => {
        pub fn get_lex_rules() -> Vec<(String, String)> {
            let mut rules: Vec<(String, String)> = Vec::new();

            $(
                let mut li = $rule.to_string();
                for prev_rule in &rules {
                    li = li.replace(&prev_rule.0, &prev_rule.1);
                }
                rules.push((format!("{{{}}}", stringify!($id)), format!("({})", li)));
            )*

            rules
        }
    };
}

define_rules! {
    BIN_DIGIT: "[0-1]",
    OCT_DIGIT: "[0-7]",
    DEC_DIGIT: "[0-9]",
    HEX_DIGIT: "[0-9a-fA-F]",
    DEC_LITERAL: "{DEC_DIGIT}({DEC_DIGIT}|_)*",
    BIN_LITERAL: "0b({BIN_DIGIT}|_)*{BIN_DIGIT}({BIN_DIGIT}|_)*",
    OCT_LITERAL: "0o({OCT_DIGIT}|_)*{OCT_DIGIT}({OCT_DIGIT}|_)*",
    HEX_LITERAL: "0x({HEX_DIGIT}|_)*{HEX_DIGIT}({HEX_DIGIT}|_)*",


    IDENTIFIER_OR_KEYWORD: "[A-Za-z_][A-Za-z0-9_]*",
    RAW_IDENTIFIER: "r#{IDENTIFIER_OR_KEYWORD}",
    SUFFIX: "{IDENTIFIER_OR_KEYWORD}",
    SUFFIX_NO_E: "(?![eE]){SUFFIX}",

    INTEGER_LITERAL: "({DEC_LITERAL}|{BIN_LITERAL}|{OCT_LITERAL}|{HEX_LITERAL}){SUFFIX_NO_E}?",

    NUL: r"\u0000",
    TAB: r"\u0009",
    LF: r"\u000A",
    CR: r"\u000D",
    QUOTE_ESCAPE: r#"\\'|\\""#,
    ASCII_ESCAPE: r"\\x{OCT_DIGIT}{HEX_DIGIT}|\\n|\\r|\\t|\\\\|\\0",
    CHAR_LITERAL: r"'([^'\\{LF}{CR}{TAB}]|{QUOTE_ESCAPE}|{ASCII_ESCAPE})'{SUFFIX}?",
    STRING_CONTINUE: r"\\{LF}",
    STRING_LITERAL: r#""([^"\\{CR}]|{QUOTE_ESCAPE}|{ASCII_ESCAPE}|{STRING_CONTINUE})*"{SUFFIX}?"#,
    RAW_STRING_CONTENT: r###"(?<sharp>#*)"[^{CR}]*?"\k<sharp>"###,
    RAW_STRING_LITERAL: r"r{RAW_STRING_CONTENT}{SUFFIX}?",
    ASCII_FOR_CHAR: r"[^'\\{LF}{CR}{TAB}]",
    BYTE_ESCAPE: r#"\\x{HEX_DIGIT}{HEX_DIGIT}|\\n|\\r|\\t|\\\\|\\0|\\'|\\""#,
    BYTE_LITERAL: r"b'({ASCII_FOR_CHAR}|{BYTE_ESCAPE})'{SUFFIX}?",
    ASCII_FOR_STRING: r#"[^"\\{CR}]"#,
    BYTE_STRING_LITERAL: r#"b"({ASCII_FOR_STRING}|{BYTE_ESCAPE}|{STRING_CONTINUE})*"{SUFFIX}?"#,
    ASCII_FOR_RAW: r#"[^{CR}]"#,
    RAW_BYTE_STRING_CONTENT: r###"(?<sharp>#*)"{ASCII_FOR_RAW}*?"\k<sharp>"###,
    RAW_BYTE_STRING_LITERAL: r"br{RAW_BYTE_STRING_CONTENT}{SUFFIX}?",
    C_STRING_LITERAL: r#"c"[^"\\{CR}{NUL}]|BYTE_ESCAPE|STRING_CONTINUE"{SUFFIX}?"#,
    RAW_C_STRING_CONTENT: r###"(?<sharp>#*)"[^{CR}{NUL}]*?"\k<sharp>"###,
    RAW_C_STRING_LITERAL: r"cr{RAW_C_STRING_CONTENT}{SUFFIX}?",

    FLOAT_LITERAL: r"{DEC_LITERAL}\.{DEC_LITERAL}{SUFFIX_NO_E}?|{DEC_LITERAL}\.(?![\._a-zA-Z])",

    RESERVED_GUARDED_STRING_LITERAL: r"#+{STRING_LITERAL}",
    RESERVED_NUMBER: r"{BIN_LITERAL}[2-9]|{OCT_LITERAL}[8-9]|({BIN_LITERAL}|{OCT_LITERAL}|{HEX_LITERAL})\.(?![\._a-zA-Z])|0b_*(?=\z|[^{BIN_DIGIT}])|0o_*(?=\z|[^{OCT_DIGIT}])|0x_*(?=\z|[^{HEX_DIGIT}])"
}

macro_rules! define_tokens {
    (@action_is_skip) => {false};
    (@action_is_skip skip) => {true};
    (@action_is_skip $other:ident) => {false};

    (@action_is_err) => {false};
    (@action_is_err err) => {true};
    (@action_is_err $other:ident) => {false};

    (@is_literal) => {false};
    (@is_literal literal) => {true};
    (@is_literal $other:ident) => {false};

    ($($token:ident : $([$attr:ident])? $keyword:literal $(-> $action:ident)?), *) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum TokenType {
            $($token, )*
            EOF
        }

        pub fn create_keyword_map() -> HashMap<String, TokenType> {
            let mut map = HashMap::new();
            $(
                map.insert($keyword.to_string(), TokenType::$token);
            )*
            map
        }

        pub fn get_all_tokens() -> Vec<(TokenType, String)> {
            let lex_rules = get_lex_rules();

            let mut v = Vec::new();
            $(
                let pat = if (define_tokens!(@is_literal $($attr)?)) {
                    regex::escape($keyword)
                } else {
                    let mut t: String = $keyword.to_string();
                    for rule in &lex_rules {
                        t = t.replace(&rule.0, &rule.1);
                    }
                    t
                };
                let pat = format!(r"\A{}", pat);

                v.push((TokenType::$token, pat));
            )*
            v
        }

        impl TokenType {
            pub fn to_keyword(&self) -> Option<&'static str> {
                match self {
                    $(
                        TokenType::$token => Some($keyword),
                    )*
                    TokenType::EOF => None
                }
            }

            pub fn should_skip(&self) -> bool {
                match self {
                    $(
                        TokenType::$token => define_tokens!(@action_is_skip $($action)?),
                    )*
                    TokenType::EOF => false
                }
            }

            pub fn should_err(&self) -> bool {
                match self {
                    $(
                        TokenType::$token => define_tokens!(@action_is_err $($action)?),
                    )*
                    TokenType::EOF => false
                }
            }
        }
    };
}

define_tokens! {
    // Keywords
    As       :[literal] "as",
    Break    :[literal] "break",
    Const    :[literal] "const",
    Continue :[literal] "continue",
    Crate    :[literal] "crate",
    Else     :[literal] "else",
    Enum     :[literal] "enum",
    Extern   :[literal] "extern",
    False    :[literal] "false",
    Fn       :[literal] "fn",
    For      :[literal] "for",
    If       :[literal] "if",
    Impl     :[literal] "impl",
    In       :[literal] "in",
    Let      :[literal] "let",
    Loop     :[literal] "loop",
    Match    :[literal] "match",
    Mod      :[literal] "mod",
    Move     :[literal] "move",
    Mut      :[literal] "mut",
    Pub      :[literal] "pub",
    Ref      :[literal] "ref",
    Return   :[literal] "return",
    LSelfType:[literal] "self",
    SelfType :[literal] "Self",
    Static   :[literal] "static",
    Struct   :[literal] "struct",
    Super    :[literal] "super",
    Trait    :[literal] "trait",
    True     :[literal] "true",
    Type     :[literal] "type",
    Unsafe   :[literal] "unsafe",
    Use      :[literal] "use",
    Where    :[literal] "where",
    While    :[literal] "while",
    Async    :[literal] "async",
    Await    :[literal] "await",
    Dyn      :[literal] "dyn",
    Abstract :[literal] "abstract",
    Become   :[literal] "become",
    Box      :[literal] "box",
    Do       :[literal] "do",
    Final    :[literal] "final",
    Macro    :[literal] "macro",
    Override :[literal] "override",
    Priv     :[literal] "priv",
    Typeof   :[literal] "typepf",
    Unsized  :[literal] "unsized",
    Virtual  :[literal] "virtual",
    Yield    :[literal] "yield",

    // Reserved
    ReservedGuardedString  :"{RESERVED_GUARDED_STRING_LITERAL}" -> err,
    ReservedNumber         :"{RESERVED_NUMBER}" -> err,

    // Identifiers
    Id: "{IDENTIFIER_OR_KEYWORD}",

    // Literals
    Character: "{CHAR_LITERAL}",
    String: "{STRING_LITERAL}",
    RawString: "{RAW_STRING_LITERAL}",
    Byte: "{BYTE_LITERAL}",
    ByteString: "{BYTE_STRING_LITERAL}",
    RawByteString: "{RAW_BYTE_STRING_LITERAL}",
    CString: "{C_STRING_LITERAL}",
    RawCString: "{RAW_C_STRING_LITERAL}",
    Integer: "{INTEGER_LITERAL}",
    Float: "{FLOAT_LITERAL}",

    // Punctuation
    Plus     :[literal] "+"    ,
    Minus    :[literal] "-"    ,
    Star     :[literal] "*"    ,
    Slash    :[literal] "/"    ,
    Percent  :[literal] "%"    ,
    Caret    :[literal] "^"    ,
    Not      :[literal] "!"    ,
    And      :[literal] "&"    ,
    Or       :[literal] "|"    ,
    AndAnd   :[literal] "&&"   ,
    OrOr     :[literal] "||"   ,
    Shl      :[literal] "<<"   ,
    Shr      :[literal] ">>"   ,
    PlusEq   :[literal] "+="   ,
    MinusEq  :[literal] "-="   ,
    StarEq   :[literal] "*="   ,
    SlashEq  :[literal] "/="   ,
    PercentEq:[literal] "%="   ,
    CaretEq  :[literal] "^="   ,
    AndEq    :[literal] "&="   ,
    OrEq     :[literal] "|="   ,
    ShlEq    :[literal] "<<="  ,
    ShrEq    :[literal] ">>="  ,
    Eq       :[literal] "="    ,
    EqEq     :[literal] "=="   ,
    Ne       :[literal] "!="   ,
    Gt       :[literal] ">"    ,
    Lt       :[literal] "<"    ,
    Ge       :[literal] ">="   ,
    Le       :[literal] "<="   ,
    At       :[literal] "@"    ,
    Underscor:[literal] "_"    ,
    Dot      :[literal] "."    ,
    DotDot   :[literal] ".."   ,
    DotDotDot:[literal] "..."  ,
    DotDotEq :[literal] "..="  ,
    Comma    :[literal] ","    ,
    Semi     :[literal] ";"    ,
    Colon    :[literal] ":"    ,
    PathSep  :[literal] "::"   ,
    RArrow   :[literal] "->"   ,
    FatArrow :[literal] "=>"   ,
    LArrow   :[literal] "<-"   ,
    Pound    :[literal] "#"    ,
    Dollar   :[literal] "$"    ,
    Question :[literal] "?"    ,
    Tilde    :[literal] "~"    ,

    // Delimiters
    OpenPar   :[literal] "(",
    ClosePar  :[literal] ")",
    OpenSqu   :[literal] "[",
    CloseSqu  :[literal] "]",
    OpenCurly :[literal] "{",
    CloseCurly:[literal] "}",

    // Others
    Comment: r"//[^\r\n]*\r?\n" -> skip,
    CommentsBlock: r"/\*[\s\S]*?\*/" -> skip,
    UnfinishedCommentBlock: r"/\*" -> err,
    Whitespace: r"[\t\n\v\f\r ]+" -> skip
}
