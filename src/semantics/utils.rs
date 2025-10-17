use std::{collections::HashSet, fmt::Display};

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
    Body,
}

pub const STAGES: [AnalyzeStage; 3] = [
    AnalyzeStage::SymbolCollect,
    AnalyzeStage::Definition,
    AnalyzeStage::Body,
];

pub(crate) fn is_all_different<T: Eq + std::hash::Hash>(vec: &[T]) -> bool {
    let mut set = HashSet::new();
    for x in vec {
        if !set.insert(x) {
            return false;
        }
    }
    true
}
