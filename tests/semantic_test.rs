use std::{fs, panic};

use c0mpiler::{
    ast::{Crate, Eatable},
    lexer::{Lexer, TokenBuffer},
    semantics::SemanticAnalyzer,
    utils::test::TestCaseInfo,
};

fn run(src: &str) -> Result<(), String> {
    let lexer = Lexer::new(src);
    let buffer = TokenBuffer::new(lexer);
    let mut iter = buffer.iter();
    let krate = Crate::eat(&mut iter);
    match krate {
        Ok(ast) => {
            let (analyzer, result) = SemanticAnalyzer::visit(&ast);
            match result {
                Ok(_) => Ok(()),
                Err(err) => Err(format!(
                    "Semantic error occured: {:#?}, analyze stage: {:?}, state: {:?}.\n{:#?}",
                    err,
                    analyzer.get_stage(),
                    analyzer.get_state(),
                    src.lines()
                        .nth(analyzer.get_state().current_span.begin.line)
                        .unwrap()
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

#[test]
fn semantics_1() {
    let escape_list = [
        "loop1", "misc29", // let 缺失类型标注
        // "misc15", // 仅从控制流上分析不能确保 loop 能退出
        "misc28", // TODO: Copy Trait
    ];

    let mut entries: Vec<_> = fs::read_dir("RCompiler-Testcases/semantic-1")
        .unwrap()
        .collect::<Result<_, _>>()
        .unwrap();
    entries.sort_by_key(|x| x.file_name());
    for x in entries {
        let name = x.file_name().into_string().unwrap();
        if escape_list.contains(&name.as_str()) {
            println!("{} skiped!", name);
            continue;
        }
        let path = x.path();
        let info_path = path.join("testcase_info.json");
        let info: TestCaseInfo =
            serde_json::from_str(fs::read_to_string(info_path).unwrap().as_str()).unwrap();
        let src_path = path.join(format!("{}.rx", name));
        let src = fs::read_to_string(src_path).unwrap();
        let should_pass = info.compileexitcode == 0;
        let result = match panic::catch_unwind(|| run(src.as_str())) {
            Ok(result) => result,
            Err(_) => {
                panic!("{} caused panic!", name);
            }
        };

        match (should_pass, result) {
            (true, Ok(_)) | (false, Err(_)) => println!("{} passed!", name),
            (true, Err(e)) => {
                panic!("{} check failed, expect pass!\n{}", name, e);
            }
            (false, Ok(_)) => {
                panic!("{} check passed, expect fail!", name);
            }
        }
    }
}
