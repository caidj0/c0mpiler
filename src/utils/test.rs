use crate::{
    ast::{Crate, Eatable},
    lexer::{Lexer, TokenBuffer},
    semantics::analyzer::SemanticAnalyzer,
};

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct TestCaseInfo {
    pub name: String,
    pub compileexitcode: i32,
}

pub fn run(src: &str) -> Result<(), String> {
    let lexer = Lexer::new(src);
    let buffer = TokenBuffer::new(lexer)?;
    let mut iter = buffer.iter();
    let krate = Crate::eat(&mut iter);
    match krate {
        Ok(ast) => {
            let (analyzer, result) = SemanticAnalyzer::visit(&ast);
            match result {
                Ok(_) => Ok(()),
                Err(err) => Err(format!(
                    "Semantic error occured: {}, analyze stage: {:?}.\n{:#?}",
                    err,
                    analyzer.get_stage(),
                    src.lines().nth(err.span.unwrap().begin.line).unwrap()
                )),
            }
        }
        Err(err) => Err(format!(
            "Parse error occured: {:#?}.\n {:#?}",
            err,
            src.lines().nth(err.pos.line).unwrap()
        )),
    }
}
