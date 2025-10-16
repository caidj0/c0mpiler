use std::fmt::Display;

use crate::ast::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FullName(pub Vec<Symbol>);

impl Display for FullName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|x| x.0.clone())
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

#[test]
fn full_name_test() {
    let name = FullName(vec![
        Symbol("hello".to_string()),
        Symbol("world".to_string()),
        Symbol("!".to_string()),
    ]);

    println!("{name}");
}

#[derive(Debug)]
pub enum AnalyzeStage {
    SymbolCollect,
    Definition,
    Impl,
    Body,
}

pub const STAGES: [AnalyzeStage; 4] = [
    AnalyzeStage::SymbolCollect,
    AnalyzeStage::Definition,
    AnalyzeStage::Impl,
    AnalyzeStage::Body,
];
