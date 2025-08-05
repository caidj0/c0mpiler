use std::collections::HashMap;

macro_rules! define_tokens {
    (@action_is_skip) => {false};
    (@action_is_skip skip) => {true};
    (@action_is_skip $other:ident) => {false};

    ($($token:ident : $keyword:literal $(-> $action:ident)?), *) => {
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
    Id: "[A-Za-z][A-Za-z0-9_]*|_[A-Za-z0-9_]+",

    // Literals
        // TODO


    // Punctuation
    Plus     :"+"    ,
    Minus    :"-"    ,
    Star     :"*"    ,
    Slash    :"/"    ,
    Percent  :"%"    ,
    Caret    :"^"    ,
    Not      :"!"    ,
    And      :"&"    ,
    Or       :"|"    ,
    AndAnd   :"&&"   ,
    OrOr     :"||"   ,
    Shl      :"<<"   ,
    Shr      :">>"   ,
    PlusEq   :"+="   ,
    MinusEq  :"-="   ,
    StarEq   :"*="   ,
    SlashEq  :"/="   ,
    PercentEq:"%="   ,
    CaretEq  :"^="   ,
    AndEq    :"&="   ,
    OrEq     :"|="   ,
    ShlEq    :"<<="  ,
    ShrEq    :">>="  ,
    Eq       :"="    ,
    EqEq     :"=="   ,
    Ne       :"!="   ,
    Gt       :">"    ,
    Lt       :"<"    ,
    Ge       :">="   ,
    Le       :"<="   ,
    At       :"@"    ,
    Underscor:"_"    ,
    Dot      :"."    ,
    DotDot   :".."   ,
    DotDotDot:"..."  ,
    DotDotEq :"..="  ,
    Comma    :","    ,
    Semi     :";"    ,
    Colon    :":"    ,
    PathSep  :"::"   ,
    RArrow   :"->"   ,
    FatArrow :"=>"   ,
    LArrow   :"<-"   ,
    Pound    :"#"    ,
    Dollar   :"$"    ,
    Question :"?"    ,
    Tilde    :"~"    ,

    // Delimiters
    OpenPar: "(",
    ClosePar: ")",
    OpenSqu: "[",
    CloseSqu: "]",
    OpenCurly: "{",
    CloseCurly: "}",

    // Others
    Comment: r"// [^\r\n]* \r? \n" -> skip,
    CommentsBlock: r"/* .*? */" -> skip,
    Whitespace: r"[ \t\r\n]" -> skip
}
