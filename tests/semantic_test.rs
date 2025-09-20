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
                    "Semantic error occured: {}, analyze stage: {:?}, state: {:?}.\n{:#?}",
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
fn my_semantic() {
    let escape_list = [
        "autoderef1", // TODO: &&A -> &A, &mut &mut A -> &mut A
        "copy_trait1",
        "copy_trait2",
        "copy_trait3", // 不清楚 Copy Trait 要实现到哪一步
        "operator1",   // TODO: &1 == &1,
    ];
    let case_path = "testcases/semantics";

    run_test_cases(&escape_list, case_path);
}

#[test]
fn semantics_1() {
    let escape_list = [
        // "misc15", // 仅从控制流上分析不能确保 loop 能退出
        "misc28", // TODO: Copy Trait
    ];
    let case_path = "RCompiler-Testcases/semantic-1";

    run_test_cases(&escape_list, case_path);
}

#[test]
#[ignore]
fn semantics_2() {
    let escape_list = [
        "comprehensive1",  // Tuple Type
        "comprehensive10", // No Exit
        "comprehensive11",
        "comprehensive12",
        "comprehensive13",
        "comprehensive14",
        "comprehensive15", // if condtion without parentheses
    ];
    let case_path = "RCompiler-Testcases/semantic-2";

    run_test_cases(&escape_list, case_path);
}

fn run_test_cases(escape_list: &[&'static str], case_path: &'static str) {
    let mut entries: Vec<_> = fs::read_dir(case_path)
        .unwrap()
        .collect::<Result<_, _>>()
        .unwrap();
    entries.sort_by_key(|x| x.file_name());
    for x in entries {
        let name = x.file_name().into_string().unwrap();
        if escape_list.contains(&name.as_str()) {
            println!("{name} skiped!");
            continue;
        }
        let path = x.path();
        let info_path = path.join("testcase_info.json");
        let info: TestCaseInfo =
            serde_json::from_str(fs::read_to_string(info_path).unwrap().as_str()).unwrap();
        let src_path = path.join(format!("{name}.rx"));
        let src = fs::read_to_string(src_path).unwrap();
        let should_pass = info.compileexitcode == 0;
        let result = match panic::catch_unwind(|| run(src.as_str())) {
            Ok(result) => result,
            Err(_) => {
                panic!("{name} caused panic!");
            }
        };

        match (should_pass, result) {
            (true, Ok(_)) | (false, Err(_)) => println!("{name} passed!"),
            (true, Err(e)) => {
                panic!("{name} check failed, expect pass!\n{e}");
            }
            (false, Ok(_)) => {
                panic!("{name} check passed, expect fail!");
            }
        }
    }
}
