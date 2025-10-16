use std::{fs, panic, path::PathBuf, str::FromStr};

use c0mpiler::{
    ast::{Crate, Eatable},
    lexer::{Lexer, TokenBuffer},
    semantics::analyzer::SemanticAnalyzer,
    utils::test::TestCaseInfo,
};

fn run(src: &str) -> Result<(), String> {
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
        "copy_trait1",
        "copy_trait2",
        "copy_trait3", // 不清楚 Copy Trait 要实现到哪一步
        "operator1",   // TODO: &1 == &1,
    ];
    let case_path = "testcases/semantics";

    run_test_cases(&escape_list, case_path, true);
}

#[test]
fn semantics_1() {
    let escape_list = [];
    let case_path = "RCompiler-Testcases/semantic-1";

    run_test_cases(&escape_list, case_path, true);
}

#[test]
fn semantics_2() {
    let escape_list = [];
    let case_path = "RCompiler-Testcases/semantic-2";

    run_test_cases(&escape_list, case_path, true);
}

fn run_test_cases(escape_list: &[&'static str], case_path: &'static str, stop_at_fault: bool) {
    let path = PathBuf::from_str(case_path).unwrap();
    let infos_path = path.join("global.json");
    let infos: Vec<TestCaseInfo> =
        serde_json::from_str(fs::read_to_string(infos_path).unwrap().as_str()).unwrap();

    let mut total: usize = 0;
    let mut success: usize = 0;

    macro_rules! fault {
        ($($t:tt)*) => {
            if stop_at_fault {
                panic!($($t)*);
            } else {
                println!($($t)*);
                println!();
                continue;
            }
        };
    }

    for x in infos {
        let name = x.name;
        if escape_list.contains(&name.as_str()) {
            println!("{name} skiped!");
            continue;
        }
        total += 1;
        let src_path = path.join(format!("src/{name}/{name}.rx"));
        let src = fs::read_to_string(src_path).unwrap();
        let should_pass = x.compileexitcode == 0;
        let result = match panic::catch_unwind(|| run(src.as_str())) {
            Ok(result) => result,
            Err(_) => {
                fault!("{name} caused panic!");
            }
        };

        match (should_pass, result) {
            (true, Ok(_)) | (false, Err(_)) => println!("{name} passed!"),
            (true, Err(e)) => {
                fault!("{name} check failed, expect pass!\n{e}");
            }
            (false, Ok(_)) => {
                fault!("{name} check passed, expect fail!");
            }
        }
        success += 1;
    }

    println!("Test Result: {}/{}", success, total);
    if success < total {
        panic!();
    }
}
