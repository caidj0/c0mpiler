use std::{
    fs,
    panic::{self, AssertUnwindSafe},
    path::PathBuf,
    process::{Command, Stdio},
    str::FromStr,
};

use c0mpiler::{
    ast::{Crate, Eatable},
    irgen::IRGenerator,
    lexer::{Lexer, TokenBuffer},
    semantics::analyzer::SemanticAnalyzer,
    utils::test::TestCaseInfo,
};

#[test]
fn semantic_1() {
    let escape_list = [];
    let case_path = "RCompiler-Testcases/semantic-1";

    run_test_cases(&escape_list, case_path, true, true);
}

#[test]
fn my_ir() {
    let escape_list = [];
    let case_path = "testcases/IR";

    run_test_cases(&escape_list, case_path, true, false);
}

#[test]
fn ir_1() {
    let escape_list = [];
    let case_path = "RCompiler-Testcases/IR-1";

    run_test_cases(&escape_list, case_path, true, false);
}

fn run_test_cases(
    escape_list: &[&'static str],
    case_path: &'static str,
    stop_at_fault: bool,
    dry_run: bool,
) {
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
        let in_path = path.join(format!("src/{name}/{name}.in"));
        let out_path = path.join(format!("src/{name}/{name}.out"));

        let src = fs::read_to_string(&src_path).unwrap();
        let should_pass = x.compileexitcode == 0;

        // Parse and semantic check
        let semantic_result = panic::catch_unwind(|| -> Result<_, String> {
            let lexer = Lexer::new(&src);
            let buffer = TokenBuffer::new(lexer).map_err(|e| format!("{:?}", e))?;
            let mut iter = buffer.iter();
            let krate = Crate::eat(&mut iter).map_err(|e| format!("{:?}", e))?;
            let (analyzer, result) = SemanticAnalyzer::visit(&krate);
            result.map_err(|e| format!("{:?}", e))?;
            Ok((analyzer, krate))
        });

        let (analyzer, krate) = match semantic_result {
            Ok(Ok((analyzer, krate))) => (analyzer, krate),
            Ok(Err(e)) => {
                if should_pass {
                    fault!("{name} semantic check failed, expect pass!\n{e}");
                } else {
                    println!("{name} passed (semantic check failed as expected)!");
                    success += 1;
                    continue;
                }
            }
            Err(_) => {
                fault!("{name} caused panic during semantic check!");
            }
        };

        // If semantic check passed but should fail
        if !should_pass {
            fault!("{name} semantic check passed, expect fail!");
        }

        // Generate IR
        let ir = match panic::catch_unwind(AssertUnwindSafe(|| {
            let mut generator = IRGenerator::new(&analyzer);
            generator.visit(&krate);
            generator.print()
        })) {
            Ok(ir) => ir,
            Err(_) => {
                fault!("{name} caused panic during IR generation!");
            }
        };

        if !dry_run {
            // Write IR to temporary file
            let ir_file = format!("target/tmp/{name}.ll");
            fs::create_dir_all("target/tmp").unwrap();
            fs::write(&ir_file, &ir).unwrap();

            // Compile with clang
            let compile_result = Command::new("clang")
                .args([
                    &ir_file,
                    "tests/prelude.c",
                    "-o",
                    &format!("target/tmp/{name}"),
                ])
                .output();

            let compile_output = match compile_result {
                Ok(output) => output,
                Err(e) => {
                    fault!("{name} failed to execute clang: {e}");
                }
            };

            if !compile_output.status.success() {
                let stderr = String::from_utf8_lossy(&compile_output.stderr);
                fault!("{name} compilation failed:\n{stderr}");
            }

            // Run the compiled program
            let input_data = if in_path.exists() {
                fs::read(&in_path).unwrap()
            } else {
                Vec::new()
            };

            let run_result = Command::new("sh")
                .arg("-c")
                .arg(format!("ulimit -s unlimited && exec target/tmp/{name}",))
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn();

            let mut child = match run_result {
                Ok(child) => child,
                Err(e) => {
                    fault!("{name} failed to execute program: {e}");
                }
            };

            // Write input to stdin
            if !input_data.is_empty() {
                use std::io::Write;
                if let Some(mut stdin) = child.stdin.take() {
                    stdin.write_all(&input_data).unwrap();
                }
            }

            let output = match child.wait_with_output() {
                Ok(output) => output,
                Err(e) => {
                    fault!("{name} failed to wait for program: {e}");
                }
            };

            // Check if program exited successfully
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                fault!("{name} program execution failed:\n{stderr}");
            }

            // Read expected output
            let expected_output = if out_path.exists() {
                fs::read(&out_path).unwrap()
            } else {
                Vec::new()
            };

            let actual_output = output.stdout;

            // Compare outputs
            if actual_output.trim_ascii_end() != expected_output.trim_ascii_end() {
                let actual_str = String::from_utf8_lossy(&actual_output);
                let expected_str = String::from_utf8_lossy(&expected_output);
                fault!(
                    "{name} output mismatch!\nExpected:\n{}\nActual:\n{}",
                    expected_str,
                    actual_str
                );
            }
        }

        println!("{name} passed!");
        success += 1;
    }

    println!("Test Result: {}/{}", success, total);
    if success < total {
        panic!();
    }
}
