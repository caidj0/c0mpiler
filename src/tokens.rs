use regex::Regex;
use std::collections::HashMap;

macro_rules! define_tokens {
    (@action_is_skip) => {false};
    (@action_is_skip skip) => {true};
    (@action_is_skip $other:ident) => {false};

    (@is_literal) => {false};
    (@is_literal literal) => {true};
    (@is_literal $other:ident) => {false};

    ($($token:ident : $([$attr:ident])? $keyword:literal $(-> $action:ident)?), *) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum TokenType {
            $($token, )*
        }

        pub fn create_keyword_map() -> HashMap<String, TokenType> {
            let mut map = HashMap::new();
            $(
                map.insert($keyword.to_string(), TokenType::$token);
            )*
            map
        }

        pub fn get_all_tokens() -> Vec<(TokenType, String)> {
            let mut v = Vec::new();
            $(
                let pat = if (define_tokens!(@is_literal $($attr)?)) {
                    r"\A".to_string() + &regex::escape($keyword)
                } else {
                    concat!(r"\A", $keyword).to_string()
                };
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
                }
            }

            pub fn should_skip(&self) -> bool {
                match self {
                    $(
                        TokenType::$token => define_tokens!(@action_is_skip $($action)?),
                    )*
                }
            }
        }
    };
}

define_tokens! {
    // Keywords
    As: "as",
    Break: "break",
    Const: "const",
    Continue: "continue",
    Crate: "crate",
    Else: "else",
    Enum: "enum",
    Extern: "extern",
    False: "false",
    Fn: "fn",
    For: "for",
    If: "if",
    Impl: "impl",
    In: "in",
    Let: "let",
    Loop: "loop",
    Match: "match",
    Mod: "mod",
    Move: "move",
    Mut: "mut",
    Pub: "pub",
    Ref: "ref",
    Return: "return",
    LSelfType: "self",
    SelfType: "Self",
    Static: "static",
    Struct: "struct",
    Super: "super",
    Trait: "trait",
    True: "true",
    Type: "type",
    Unsafe: "unsafe",
    Use: "use",
    Where: "where",
    While: "while",
    Async: "async",
    Await: "await",
    Dyn: "dyn",
    Abstract: "abstract",
    Become: "become",
    Box: "box",
    Do: "do",
    Final: "final",
    Macro: "macro",
    Override: "override",
    Priv: "priv",
    Typeof: "typepf",
    Unsized: "unsized",
    Virtual: "virtual",
    Yield: "yield",

    // Identifiers
    Id: "[A-Za-z][A-Za-z0-9_]*",

    // Literals
    Character: r#"'(?:[^'\\]|\\(?:[nrt\\0'"]|x[0-7][0-9A-Fa-f]))'"#,
        // TODO


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
    Comment: r"// [^\r\n]* \r? \n" -> skip,
    CommentsBlock: r"/* .*? */" -> skip,
    Whitespace: r"[ \t\r\n]" -> skip
}
